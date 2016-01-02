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
  init:stop().
