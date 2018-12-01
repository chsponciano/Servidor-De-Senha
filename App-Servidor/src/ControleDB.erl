%%%-------------------------------------------------------------------
%%% @author Carlos Henrique Ponciano da Silva && Vinicius Luis da Silva
%%% @copyright (C) 2018, <Furb>
%%% @doc
%%%   Classe de controle
%%% @end
%%%-------------------------------------------------------------------
-module('ControleDB').
-author("Carlos Henrique Ponciano da Silva && Vinicius Luis da Silva").

%% API
-export([reset/1,index/1,write/2,read/1]).

reset(File) ->
  write(File, 0).

index(File) ->
  N = read(File) + 1,
  write(File, N),
  io:format("~s~s~s~w~s", ["Index ", File, ": ", N, "\n"]),
  N.

write(File, Val)->
  {ok, IO} = file:open(File, [read,write]),
  Data = term_to_binary(Val),
  file:write(IO, Data).

read(File) ->
  {ok, IO} = file:read_file(File),
  Val = binary_to_term(IO),
  Val.
