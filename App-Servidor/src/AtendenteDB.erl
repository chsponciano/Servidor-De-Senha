%%%-------------------------------------------------------------------
%%% @author Carlos Henrique Ponciano da Silva && Vinicius Luis da Silva
%%% @copyright (C) 2018, <Furb>
%%% @doc
%%%   Classe de controle de banco de dados de Atendentes
%%% @end
%%%-------------------------------------------------------------------
-module('AtendenteDB').
-author("Carlos Henrique Ponciano da Silva && Vinicius Luis da Silva").

-include_lib("stdlib/include/qlc.hrl"). %%Biblioteca para selects personalizados

%% API
-export([start/0,reset/0,insert/3,delete/1,select/1,exist/2]).

%%Cria um objeto de atendente
-record(atendente, {id, nome, cpf, senha}).

%%Constantes
-define(Table, atendente).
-define(FIndex, "AT.DBINFO").

%%Inicializa banco de dados e caso não tenha cria a tabela ATENDENTE com os atributos do Record
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

%%Inseri um Atendente da tabela solicitado o Nome, Cpf e Senha
insert(Nome, Cpf, Senha) ->
  Id = 'ControleDB':index(?FIndex), %%Pega o Index
  AF = fun() ->
    mnesia:write(#atendente{id = Id,nome = Nome,cpf = Cpf,senha = Senha}) %%Inseri
       end,
  mnesia:transaction(AF), %%Efetua a operacao
  Id. %%Retorna o id

%%Seleciona todos os elementos da tabela
select(Id) ->
  %%Efetua o select,  1. Define as variaveis
  %%                  2. Seleciona a tabela e transfere para a variavel
  T =mnesia:transaction(fun() ->
    qlc:eval(qlc:q([X || X <- mnesia:table(?Table), X#atendente.id =:= Id])) end),
  X = tuple_to_list(T), %%Converte uma Tuple em lista
  X.

delete(Id)->
  %%Efetua o select,  1. Define as variaveis
  %%                  2. Seleciona a tabela e transfere para a variavel
  %%                  3. Define as condicoes
  AF = fun() ->
    Query = qlc:q([X || X <- mnesia:table(?Table),
      X#atendente.id =:= Id]),
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

%%Verifica se existe atendente, solicita o cpf e a senha e retorna o id do atendente
exist(Cpf, Sen) ->
  AF = fun() ->
    Query = qlc:q([X || X <- mnesia:table(?Table),
      (X#atendente.cpf =:= Cpf) and (X#atendente.senha =:= Sen)]),
    Results = qlc:e(Query),

    lists:map(fun(Item) -> {Item#atendente.id} end, Results)
       end,

  R = mnesia:transaction(AF),     % PEGA A TUPLE QUE RETORNA DA CONSULTA
  {_, RS} = R,                    % PEGA A LISTA RESULTADO DA CONSULTA DE DENTRO DA TUPLE
  C = lists:flatlength(RS),              % PEGA O TAMANHO DA LISTA

  if
    C > 0 ->                      % SE TIVER ALGO NA LISTA
      T = lists:nth(1, RS),       % PEGA O PRIMEIRO ELEMENTO (TUPLE)
      {ID} = T,                   % EXTRAI O ID DESSA TUPLE
      ID                          % E RETORNA
    ; true -> -1                  % CASO CONTRARIO RETORNA NEGATIVO
  end.
