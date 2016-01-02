%%%-------------------------------------------------------------------
%%% @author nietaki
%%% @copyright (C) 2016, nietaki.github.io
%%% @doc
%%%
%%% @end
%%% Created : 02. Jan 2016 18:22
%%%-------------------------------------------------------------------
-module(utils).
-author("nietaki").

%% API
-export([set_cookie/0]).

%% @doc
%% requires you to start erlang with `erl -sname node_name`  
%% @end
set_cookie() -> erlang:set_cookie(node(), default).
