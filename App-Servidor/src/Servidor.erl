%%%-------------------------------------------------------------------
%%% @author Carlos
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. nov 2018 20:34
%%%-------------------------------------------------------------------
-module('Servidor').
-author("Carlos Henrique Ponciano da Silva && Vinicius Luis da Silva").

%% API
-export([start_server/0,reset/0]).
-define(Port,9000).
-define(FCall, "CALL.config").
-define(FUsn, "USN.config").
-define(FUsp, "USP.config").

start_server() ->
  'AtendenteDB':start(),
  'SenhaDB':start(),
  'HistoricoDB':start(),
  'VisorDB':start(),

  Pid = spawn_link(fun() ->
    {ok, LSocket} = gen_tcp:listen(?Port, [binary, {active, false}]),
    spawn(fun() -> acceptState(LSocket) end), timer:sleep(infinity) end),
  {ok, Pid}.

acceptState(LSocket) ->
  {ok, ASocket} = gen_tcp:accept(LSocket),
  spawn(fun() -> acceptState(LSocket) end),
  handler(ASocket).


reset()->
  'ControleDB':reset(?FUsn),
  'ControleDB':reset(?FUsp),
  'SenhaDB':reset(),
  'HistoricoDB':reset(),
  'AtendenteDB':reset(),
  'VisorDB':reset().


handler(ASocket) ->
  inet:setopts(ASocket, [{active, once}]),
  receive
    {tcp, ASocket, <<"Sair">>} ->
      gen_tcp:close(ASocket);

    {tcp, ASocket, <<"LAT=", X/binary>>} -> %% AUTENTICACAO DE ATENDENTE
      Y = string:lexemes(binary_to_list(X), "&"), %% Separa o request
      C = list_to_integer(lists:nth(1,Y)), %%Pega o CPF
      S = list_to_integer(lists:nth(2,Y)), %% Pega a Senha
      R = 'AtendenteDB':exist(C,S), %% Verifica se existe o atendente
      gen_tcp:send(ASocket, integer_to_list(R)), %% envia o retorno

      handler(ASocket);

    {tcp, ASocket, <<"SPC=", X/binary>>} -> % SOLICITAR PROXIMA SENHA A SER CHAMADA
      Atendente = list_to_integer(binary_to_list(X)),
      Tipo = kind_of_next_to_call(),

      if
        Tipo == "N" -> N = 'SenhaDB':call(Atendente,0),
          StrN = string:concat("SN", integer_to_list(N)),
          gen_tcp:send(ASocket, StrN);
        Tipo == "P" -> N = 'SenhaDB':call(Atendente,1),
          StrN = string:concat("SP", integer_to_list(N)),
          gen_tcp:send(ASocket, StrN);
        true -> gen_tcp:send(ASocket, "SEM SENHA NOVA")
      end,

      handler(ASocket);

    {tcp, ASocket, <<"SRSA=",X/binary >>} -> %% SOLICITAR O RELATORIO DE SENHAS CHAMADAS POR ANO
      R = 'HistoricoDB':select_ano(binary_to_integer(X)),
      IOList = io_lib:format("~w", [R]),
      FlatList = lists:flatten(IOList),
      gen_tcp:send(ASocket, FlatList), %%Envia
      handler(ASocket);

    {tcp, ASocket, <<"SRSM=",X/binary >>} -> %% SOLICITAR O RELATORIO DE SENHAS CHAMADAS POR MES E ANO
      Y = string:lexemes(binary_to_list(X), "/"), %% Separa o request
      M = list_to_integer(lists:nth(1,Y)), %%Pega o Mes
      A = list_to_integer(lists:nth(2,Y)), %% Pega a Ano
      R = 'HistoricoDB':select_mes(M, A),
      IOList = io_lib:format("~w", [R]),
      FlatList = lists:flatten(IOList),
      gen_tcp:send(ASocket, FlatList),
      handler(ASocket);

    {tcp, ASocket, <<"SRSD=",X/binary >>} -> %% SOLICITAR O RELATORIO DE SENHAS CHAMADAS POR DIA, MES E ANO
      Y = string:lexemes(binary_to_list(X), "/"), %% Separa o request
      D = list_to_integer(lists:nth(1,Y)), %%Pega o Dia
      M = list_to_integer(lists:nth(2,Y)), %% Pega a Mes
      A = list_to_integer(lists:nth(3,Y)), %% Pega a Ano
      R = 'HistoricoDB':select_dia(D, M, A),
      IOList = io_lib:format("~w", [R]),
      FlatList = lists:flatten(IOList),
      gen_tcp:send(ASocket, FlatList),
      handler(ASocket);

    {tcp, ASocket,BinaryMsg} ->
      if
        (BinaryMsg =:= <<"SSN">>)-> % SOLICITAR SENHA NORMAL
          N = 'ControleDB':index(?FUsn),
          'SenhaDB':insert(N,0),

          StrN = string:concat("SN", integer_to_list(N)),
          gen_tcp:send(ASocket, StrN);

        (BinaryMsg =:= <<"SSP">>)-> % SOLICITAR SENHA PREFERENCIAL
          N = 'ControleDB':index(?FUsp),
          'SenhaDB':insert(N,1),

          StrN = string:concat("SP", integer_to_list(N)),
          gen_tcp:send(ASocket, StrN);

        (BinaryMsg =:= <<"SARS">>) -> %SOLICITAR O HISTORICO COMPLETO
          N = 'HistoricoDB':select_all(),
          IOList = io_lib:format("~w", [N]),
          FlatList = lists:flatten(IOList),
          gen_tcp:send(ASocket, FlatList);

        (BinaryMsg =:= <<"SSE">>) -> %SOLICITAR SENHA A SER EXIBIDA NO VISOR.
          N = 'VisorDB':call(),
          if
            N /= false ->
              IOList = io_lib:format("~w", [N]),
              FlatList = lists:flatten(IOList),
              io:fwrite(FlatList),
              io:fwrite("\n"),
              gen_tcp:send(ASocket, FlatList);
            true -> gen_tcp:send(ASocket, "-1")
          end;

        (BinaryMsg =:= <<"SEE">>) -> %% SOLICITAR LISTA DE SENHA EM ESPERA
          R = 'SenhaDB':select_all(), %%Pega toda as senhas
          IOList = io_lib:format("~w", [R]),
          FlatList = lists:flatten(IOList),
          gen_tcp:send(ASocket, FlatList); %%Envia

        true ->
          gen_tcp:send(ASocket, "Mensagem Inválida")
      end,
      handler(ASocket)
  end.

kind_of_next_to_call()->
  N = 'ControleDB':read(?FCall),
  CountSPref = 'SenhaDB':count(1),
  CountSNorm = 'SenhaDB':count(0),

  io:format("~s~w~s", ["Status de Chamada: ", N, "\n"]),
  if
    N == 3 ->
      if
        CountSPref > 0 -> 'ControleDB':write(?FCall, 1), "P";
        true ->
          if
            CountSNorm > 0 -> "N";
            true -> false
          end
      end;
    true ->
      if
        CountSNorm > 0 -> 'ControleDB':write(?FCall, N + 1), "N";
        CountSPref > 0 -> 'ControleDB':write(?FCall, 1), "P";
        true -> false
      end
  end.
