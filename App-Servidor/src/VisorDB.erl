%%%-------------------------------------------------------------------
%%% @author Carlos Henrique Ponciano da Silva && Vinicius Luis da Silva
%%% @copyright (C) 2018, <Furb>
%%% @doc
%%%   Classe de controle de banco de dados do visor
%%% @end
%%%-------------------------------------------------------------------
-module('VisorDB').
-include_lib("stdlib/include/qlc.hrl"). %%Biblioteca para selects personalizados

%% API
-export([start/0,insert/2,select_current/0,delete/1,reset/0,call/0,select_all/0]).

%%Cria um objeto de senha
-record(visor, {id, sequencia, tipo}).

%%Constantes
-define(Table, visor).
-define(FIndex, "VI.DBINFO").

%%Inicializa banco de dados e caso não tenha cria a tabela SENHA com os atributos do Record
start() ->
  mnesia:create_schema([node()]),
  mnesia:start(),
  try
    mnesia:table_info(type, ?Table)
  catch
    exit: _ ->
      mnesia:create_table(?Table, [{attributes, record_info(fields, ?Table)},
        {type,bag},
        {disc_copies, [node()]}])
  end.

%%Reseta arquivo de controle de Index
reset()->
  'ControleDB':reset(?FIndex),
  delete_all().

%%Inseri uma senha da tabela solicitado o numero da senha e o tipo(0 - Normal, 1 - Preferencial)
insert(Sequencia, Tipo) ->
  Id = 'ControleDB':index(?FIndex), %%Pega o Index
  AF = fun() ->
    mnesia:write(#visor{id = Id,sequencia = Sequencia, tipo = Tipo}) %%Inseri
       end,
  mnesia:transaction(AF), %%Efetua a operacao
  Id. %%Retorna o id

select_all()->
  T = mnesia:transaction(fun() ->
    qlc:eval(qlc:q([X || X <- mnesia:table(?Table)])) end),
  X = (element(2, T)),
  X.

%%Seleciona a primeira senha na fila solicitando
select_current() ->
  %%Efetua o select,  1. Define as variaveis
  %%                  2. Seleciona a tabela e transfere para a variavel
  %%                  3. Define as condicoes
  T = mnesia:transaction(fun() ->
    qlc:eval(qlc:q([X || X <- mnesia:table(?Table)]))
                         end),

  %%Separa o titulo dos inserts
  X = (element(2, T)),

  %%Verifica se esta null, se estiver retorna 0 senão retorna o primeiro elemento
  try lists:sum(X) == 0 of
    true -> 0
  catch
    ExceptionType:_ ->
      Z = lists:min(X),
      Z
  end.

delete(Id)->
  %%Efetua o select,  1. Define as variaveis
  %%                  2. Seleciona a tabela e transfere para a variavel
  %%                  3. Define as condicoes
  AF = fun() ->
    Query = qlc:q([X || X <- mnesia:table(?Table),
      X#visor.id =:= Id]),
    Results = qlc:e(Query),

    %%Vai percorrendo os resultados e exclui
    F = fun() ->
      lists:foreach(fun(Result) ->
        mnesia:delete_object(Result)
                    end, Results)
        end,
    mnesia:transaction(F)

       end,
  mnesia:transaction(AF).

delete_all()->
  %%Efetua o select,  1. Define as variaveis
  %%                  2. Seleciona a tabela e transfere para a variavel
  %%                  3. Define as condicoes
  AF = fun() ->
    Query = qlc:q([X || X <- mnesia:table(?Table)]),
    Results = qlc:e(Query),

    %%Vai percorrendo os resultados e exclui
    F = fun() ->
      lists:foreach(fun(Result) ->
        mnesia:delete_object(Result)
                    end, Results)
        end,
    mnesia:transaction(F)

       end,
  mnesia:transaction(AF).

call() ->
  O = select_current(),
  if
    O == 0 ->
      false;
    O /= 0 ->
      Id = element(2, O),
      delete(Id),
      O
  end.