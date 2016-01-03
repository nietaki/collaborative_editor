%%%-------------------------------------------------------------------
%%% @author nietaki
%%% @copyright (C) 2016, nietaki.github.io
%%% @doc
%%%
%%% @end
%%% Created : 02. Jan 2016 17:00
%%%-------------------------------------------------------------------
-module(chat_server).
-author("nietaki").

-behaviour(gen_server).

-import(utils, [list_contains/2]).

%% API
-export([start_link/0, start/0, stop/0]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-include("chat_types.hrl").

-record(state, {clients=[] :: [client()],messages=[] :: [message()]}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

start() -> start_link().

stop() -> gen_server:stop(?MODULE).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([]) ->
  {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: command(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
  {reply, Reply :: chat_server_response(), NewState :: #state{}} |
  {reply, Reply :: chat_server_response(), NewState :: #state{}, timeout() | hibernate} |
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_call(Request, From, State) ->
  io:format("server: received call: ~p~n", [Request]),
  {Pid, Tag} = From,
  #state{clients=Clients, messages=Messages} = State,
  case Request of
    % user joining
    {join, Username} -> 
      case client_with_pid_exists(Pid, Clients) of
        true -> {reply, already_joined, State};
        false ->
          erlang:monitor(process, Pid),
          NewClients = [{Pid, Tag, Username}|Clients],
          {reply, {history, Messages}, State#state{clients=NewClients}};
        _ -> this_shouldnt_happen 
      end;
    {say, Text} ->
      case get_username(Pid, Clients) of
        false -> {reply, you_are_not_a_user_yet, State}; 
        Username -> 
          send_message_to_clients(Username, Text, Clients),
          {reply, you_said_something, State#state{messages=[{Username,Text} | Messages]}}
      end;
    get_clients -> {reply, State#state.clients, State};
    get_history -> {reply, State#state.messages, State};
    stop -> {stop, normal, shutdown_ok, State};
    _ -> {reply, wat, State}
  end. 

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_cast(Request, State) ->
  case Request of
    {part, Pid} -> 
      io:format("~p wants to part, removing it from clients~n", [Pid]),
      no_reply_with_state_without_pid(State, Pid);
    _ ->
      io:format("server: received cast: ~p~n", [Request]),
      {noreply, State}
  end. 

no_reply_with_state_without_pid(State, Pid) ->
  OldClients = State#state.clients,
  NewClients = get_clients_without_pid(Pid, OldClients),
  {noreply, State#state{clients = NewClients}}. 

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_info(Info, State) ->
  case Info of
    {'DOWN', _Ref, process, Pid, Why} ->
      io:format("~p has died because of ~p, removing it from clients~n", [Pid, Why]),
      no_reply_with_state_without_pid(State, Pid);
    _ ->
      io:format("server: received info: ~p~n", [Info]),
      {noreply, State}
  end. 

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, _State) ->
  io:format("server: terminating~n"),
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
  {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

client_has_pid(Client, Pid) ->
  {Client_pid, _Tag, _Name} = Client,
  Client_pid == Pid.

client_with_pid_exists(Pid, Clients) -> 
  lists:any(fun(C) -> client_has_pid(C, Pid) end, Clients).

get_clients_without_pid(Pid, Clients) ->
  lists:filter(fun(C) -> not client_has_pid(C, Pid) end, Clients). 

-spec get_username(Pid :: pid(), Clients :: [client()]) -> false | string().
get_username(Pid, Clients) ->
  case lists:keyfind(Pid, 1, Clients) of
    {_Pid, _Tag, Username} -> Username;
    false -> false
  end.

get_client_pid(Client) -> 
  case Client of
    {Pid, _Tag, _Username} -> Pid
  end.

send_message_to_clients(Username, Text, Clients) ->
  Pids = lists:map(fun get_client_pid/1, Clients),
  SendMessageToPid= fun(Pid) ->
    io:format("server: sending ~p to ~p~n", [{said, Username, Text}, Pid]),
    Pid ! {said, Username, Text}
  end,
  lists:foreach(SendMessageToPid, Pids).

