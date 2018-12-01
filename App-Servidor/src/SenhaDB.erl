%%%-------------------------------------------------------------------
%%% @author Carlos Henrique Ponciano da Silva && Vinicius Luis da Silva
%%% @copyright (C) 2018, <Furb>
%%% @doc
%%%   Classe de controle de banco de dados de senha
%%% @end
%%%-------------------------------------------------------------------
-module('SenhaDB').
-author("Carlos Henrique Ponciano da Silva && Vinicius Luis da Silva").

-include_lib("stdlib/include/qlc.hrl"). %%Biblioteca para selects personalizados

%% API
-export([start/0,insert/2,select_current/1,call/2,delete/1,reset/0,select_all/0,count/1]).

%%Cria um objeto de senha
-record(senha, {id, sequencia, dataSolicitada, tipo}).

%%Constantes
-define(Table, senha).
-define(FIndex, "SE.DBINFO").

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

count(Tp)->
  AF = fun() ->
    Query = qlc:q([X || X <- mnesia:table(?Table), X#senha.tipo =:= Tp]),
    Results = qlc:e(Query),
    Results
       end,
  X = mnesia:transaction(AF),
  Y = element(2,X),
  Z = lists:flatlength(Y),
  Z.


%%Reseta arquivo de controle de Index
reset()->
  'ControleDB':reset(?FIndex),
  delete_all().

%%Inseri uma senha da tabela solicitado o numero da senha e o tipo(0 - Normal, 1 - Preferencial)
insert(Sequencia, Tipo) ->
  Id = 'ControleDB':index(?FIndex), %%Pega o Index
  AF = fun() ->
    {DataSolicitada, _} = calendar:local_time(), %%Define a data e a hora de solcitacao
    mnesia:write(#senha{id = Id,dataSolicitada = DataSolicitada,sequencia = Sequencia, tipo = Tipo}) %%Inseri
       end,
  mnesia:transaction(AF), %%Efetua a operacao
  Id. %%Retorna o id

%%Seleciona a primeira senha na fila solicitando o tipo(0 - Normal, 1 - Preferencial)
select_current(Tp) ->
  %%Efetua o select,  1. Define as variaveis
  %%                  2. Seleciona a tabela e transfere para a variavel
  %%                  3. Define as condicoes
  T = mnesia:transaction(fun() ->
    qlc:eval(qlc:q([X || X <- mnesia:table(?Table), X#senha.tipo =:= Tp]))
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

%%Seleciona todos os elementos da tabela

select_all() ->
  %%Efetua o select,  1. Define as variaveis
  %%                  2. Seleciona a tabela e transfere para a variavel
  T =mnesia:transaction(fun() ->
    qlc:eval(qlc:q([X || X <- mnesia:table(?Table)])) end),
  %X = tuple_to_list(T), %%Converte uma Tuple em lista
  {_, X} = T,
  X.

delete(Id)->
  %%Efetua o select,  1. Define as variaveis
  %%                  2. Seleciona a tabela e transfere para a variavel
  %%                  3. Define as condicoes
  AF = fun() ->
    Query = qlc:q([X || X <- mnesia:table(?Table),
      X#senha.id =:= Id]),
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

%%Controle de senha
call(At,Tp) ->
  T = select_current(Tp), %%Seleciona a senha da fila
  if
    T == 0 ->
      io:format("~s~w~s", ["Senha - Nao tem do tipo: ", Tp, "\n"]),
      false;
    T /= 0 ->
      Id = element(2,T),
      io:format("~s~w~s~s~w~s", ["Senha: ", T, "\n", "ID: ", Id, "\n"]),
      Dt = element(4,T),
      Se = element(3,T),
      'HistoricoDB':insert(Se,At,Dt,Tp),
      'VisorDB':insert(Se,Tp),
      delete(Id),
      Se
  end.
