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

-include("chat_types.hrl").

-export([start_client1/0, start_client2/0, start/2, messaging_loop/1, print_message/1]).

%% convenience functions
start_client1() -> start("Jacek", {chat_server, serv@shiny}).
start_client2() -> start("Frane", {chat_server, serv@shiny}). 

%% API


start(Username, Server) ->
  MessagingClientPid = spawn_link(?MODULE, messaging_loop, [Server]),
  MessagingClientPid ! {join, Username},
  input_loop(MessagingClientPid).

input_loop(MessagingClientPid) ->
  Input = string:strip(io:get_line(""), both, $\n),
  case Input of
    "part" ->
      io:format("disconnecting from the server~n"),
      MessagingClientPid ! part;
    _ ->
      MessagingClientPid ! {say, Input},
      input_loop(MessagingClientPid)
  end. 

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
      messaging_loop(Server);
    part -> 
      gen_server:cast(Server, {part, self()}),
      io:format("messaging loop finishing~n"),
      init:stop()
  end.

print_message(Message) ->
  case Message of
    {Username, Text} -> io:format("<~ts> ~ts~n",[Username, Text])
  end.