%%%-------------------------------------------------------------------
%%% @author nietaki
%%% @copyright (C) 2016, nietaki.github.io
%%% @doc
%%%
%%% @end
%%% Created : 02. Jan 2016 14:44
%%%-------------------------------------------------------------------
-module(echo).
-author("nietaki").

%% API
-export([start/1]).

start(Foo) -> 
  io:format("function args are: ~p~n", [Foo]),
  io:format("write words to have them echoed to you or write 'exit' to exit~n"),
  echo_loop(0),
  init:stop().

echo_loop(I) ->
  LineNo = integer_to_list(I),
  Input = io:fread("say sth " ++ LineNo ++ "> ", "~s"),
  case Input of
    {ok, ["exit"]} -> exit;
    {ok, [Word]} ->
      io:format("written " ++ LineNo ++ ": ~p~n", [Word]),
      echo_loop(I+1);
    SthElse -> SthElse
  end. 
