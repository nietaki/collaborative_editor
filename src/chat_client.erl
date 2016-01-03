%%%-------------------------------------------------------------------
%%% @author nietaki
%%% @copyright (C) 2016, nietaki.github.io
%%% @doc
%%%
%%% @end
%%% Created : 03. Jan 2016 00:04
%%%-------------------------------------------------------------------
-module(chat_client).
-author("nietaki").

%% API
-export([start/2, messaging_loop/1, print_message/1]).

-include("chat_types.hrl").

start(Username, Server) ->
  MessagingClientPid = spawn_link(?MODULE, messaging_loop, [Server]),
  MessagingClientPid ! {join, Username},
  input_loop(MessagingClientPid).

input_loop(MessagingClientPid) ->
  Input = io:get_line(""),
  MessagingClientPid ! {say, Input},
  input_loop(MessagingClientPid).

messaging_loop(Server) -> 
  receive
    {join, Username} ->
      {history, History} = gen_server:call(Server, {join, Username}),
      HistoryInOrder = lists:reverse(History),
      lists:foreach(fun print_message/1, HistoryInOrder),
      messaging_loop(Server);
    {say, Text} -> 
      _Reply = gen_server:call(Server, {say, Text}),
      messaging_loop(Server);
    {said, Username, Text} ->
      print_message({Username, Text}),
      messaging_loop(Server)
  end.

print_message(Message) ->
  case Message of
    {Username, Text} -> io:format("<~p> ~p~n",[Username, Text])
  end.