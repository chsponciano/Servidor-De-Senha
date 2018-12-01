%%%-------------------------------------------------------------------
%%% @author Carlos Henrique Ponciano da Silva && Vinicius Luis da Silva
%%% @copyright (C) 2018, <Furb>
%%% @doc
%%%   Classe de controle de banco de dados de historico de senhas
%%% @end
%%%-------------------------------------------------------------------
-module('HistoricoDB').
-author("Carlos Henrique Ponciano da Silva && Vinicius Luis da Silva").

-include_lib("stdlib/include/qlc.hrl"). %%Biblioteca para selects personalizados

%% API
-export([start/0,reset/0,insert/4,delete/1,select_all/0, select_ano/1, select_mes/2, select_dia/3]).

%%Cria um objeto de historico
-record(historico, {id, sequencia, atendente, dataSolicitada, dataChamada, tipo}).

%%Constantes
-define(Table, historico).
-define(FIndex, "HI.DBINFO").

%%Inicializa banco de dados e caso não tenha cria a tabela HISTORICO com os atributos do Record
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

%%Inseri uma senha da tabela solicitado o numero da senha, atendente, data de solicitacao e o tipo(0 - Normal, 1 - Preferencial)
insert(Sequencia, Atendente, DataSolicitacao, Tipo) ->
  Id = 'ControleDB':index(?FIndex), %%Pega o Index
  AF = fun() ->
    {DataChamada, _} = calendar:local_time(), %%Define a data e a hora de chamda
    mnesia:write(#historico{id = Id, sequencia = Sequencia, atendente = Atendente, dataSolicitada = DataSolicitacao, dataChamada = DataChamada, tipo = Tipo}) %%Inseri
       end,
  mnesia:transaction(AF), %%Efetua a operacao
  Id. %%Retorna o id

%%Seleciona todos os elementos da tabela
select_all() ->
  %%Efetua o select,  1. Define as variaveis
  %%                  2. Seleciona a tabela e transfere para a variavel
  T =mnesia:transaction(fun() ->
    qlc:eval(qlc:q([X || X <- mnesia:table(?Table)])) end),
  {_, X} = T,
  X.

delete(Id)->
  %%Efetua o select,  1. Define as variaveis
  %%                  2. Seleciona a tabela e transfere para a variavel
  %%                  3. Define as condicoes
  AF = fun() ->
    Query = qlc:q([X || X <- mnesia:table(?Table),
      X#historico.id =:= Id]),
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


select_ano(A) ->
  AF = fun() ->
    Q = qlc:q([X || X <- mnesia:table(?Table),
      (lists:nth(1, tuple_to_list(X#historico.dataSolicitada)) =:= A)]),
    R = qlc:e(Q),

    lists:map(fun(I) ->
      %{_, RS} = 'AtendenteDB':select(list_to_integer(I#historico.atendente)),
      %{_, _, A, _, _} = lists:nth(1, RS),
      {
        I#historico.id,
        I#historico.sequencia,
        I#historico.tipo,
        I#historico.atendente,
        I#historico.dataChamada,
        I#historico.dataSolicitada
      }
              end, R)
       end,

  RS = mnesia:transaction(AF),
  {_, R} = RS,
  R.

select_mes(M, A) ->
  AF = fun() ->
    Q = qlc:q([X || X <- mnesia:table(?Table),
      (lists:nth(1, tuple_to_list(X#historico.dataSolicitada)) =:= A)
        and
        (lists:nth(2, tuple_to_list(X#historico.dataSolicitada)) =:= M)]),
    R = qlc:e(Q),

    lists:map(fun(I) ->
      %{_, RS} = 'AtendenteDB':select(list_to_integer(I#historico.atendente)),
      %{_, _, A, _, _} = lists:nth(1, RS),
      {
        I#historico.id,
        I#historico.sequencia,
        I#historico.tipo,
        I#historico.atendente,
        I#historico.dataChamada,
        I#historico.dataSolicitada
      }
              end, R)
       end,

  RS = mnesia:transaction(AF),
  {_, R} = RS,
  R.

select_dia(D, M, A) ->
  AF = fun() ->
    Q = qlc:q([X || X <- mnesia:table(?Table),
      (lists:nth(1, tuple_to_list(X#historico.dataSolicitada)) =:= A)
        and
        (lists:nth(2, tuple_to_list(X#historico.dataSolicitada)) =:= M)
        and
        (lists:nth(3, tuple_to_list(X#historico.dataSolicitada)) =:= D)]),
    R = qlc:e(Q),

    lists:map(fun(I) ->
      %{_, RS} = 'AtendenteDB':select(list_to_integer(I#historico.atendente)),
      %{_, _, A, _, _} = lists:nth(1, RS),
      {
        I#historico.id,
        I#historico.sequencia,
        I#historico.tipo,
        I#historico.atendente,
        I#historico.dataChamada,
        I#historico.dataSolicitada
      }
              end, R)
       end,

  RS = mnesia:transaction(AF),
  {_, R} = RS,
  R.
