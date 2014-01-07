%%
%%  Copyright (C) 2013-2014 Elizaveta Lukicheva.
%%
%%  This file is part of Schedule Server.
%%
%%  Schedule Server is free software: you can redistribute it and/or modify
%%  it under the terms of the GNU General Public License as published by
%%  the Free Software Foundation, either version 3 of the License, or
%%  (at your option) any later version.
%%
%%  Schedule Server is distributed in the hope that it will be useful,
%%  but WITHOUT ANY WARRANTY; without even the implied warranty of
%%  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%  GNU General Public License for more details.
%%
%%  You should have received a copy of the GNU General Public License
%%  along with Schedule Server.  If not, see <http://www.gnu.org/licenses/>.

%%  Author: Elizaveta Lukicheva <mailto: liza.lukicheva@gmail.com>
%%  License: <http://www.gnu.org/licenses/gpl.html>

%% @author Elizaveta Lukicheva <liza.lukicheva@gmail.com>
%% @copyright 2013-2014 Elizaveta Lukicheva
%% @doc This is the tier 1 Schedule Server packet processor. It receives a raw
%% message from the supervisor (and supervisor receives
%% it from the acceptor) and handles it.

-module(io).
-behaviour(gen_server).
-include("types.hrl").

%% Handling:
-export([start_link/1]).
-export([process/2]).

%% Callbacks:
-export([init/1, terminate/2]).
-export([handle_call/3, handle_info/2, handle_cast/2]).
-export([code_change/3]).

%%% @spec start_link() -> Result
%%%    Result = {ok,Pid} | ignore | {error,Error}
%%%     Pid = pid()
%%%     Error = {already_started,Pid} | term()
%%%  
%%% @doc Creates new Schedule Server packet processor.
%%%
start_link([Socket]) ->
  gen_server:start_link(io, [Socket], []).

%%% @spec process(Pid, Message) -> Result
%%%    Pid = pid()
%%%    Message = record(recv)
%%%    Result = {stop, Reason, null}
%%%     Reason = normal | {error, Error}
%%%      Error = term()
%%%  
%%% @doc Main parsing routine starter. Calls process/1 via 
%%% handle_cast to perform asynchronous processing. Pid is
%%% one associated with the client's tokem in token storage.
%%%
process(Pid, Message) when is_record(Message, recv) ->
  report(1, "Processing message with IO", Pid),
  report(3, "Message", Message),
  gen_server:cast(Pid, Message). % -> handle_cast -> process/1.
  
%% Routines:
  
%% @doc Main parsing routine of the processor. Parses and handles packet.
process(Message)  ->
  {ok, null}.
  
%% Callbacks:

%% @doc Initializes random generator.
init(_) ->
  random:seed(now()),
  report(1, "IO starting"),
  {ok, null}.
  
%% @hidden
handle_cast(Message, _) when is_record(Message, recv) ->
  process(Message),
  {stop, normal, null};
  
handle_cast(Data, State) ->
  report(0, "Wrong cast in IO", Data),
  {noreply, State}.

%% @hidden
handle_call(Data, _, State) ->
  report(0, "Wrong sync event in IO",Data),
  {reply, ok, State}.

%% @hidden
handle_info(Data, State) ->
  report(0, "Wrong info in IO",Data),
  {noreply, State}.
  
%% @hidden
terminate(Reason, _) ->
  report(1, "Terminating IO"), 
  report(2, "Reason", Reason),
  ok.

%% @hidden
code_change(_, StateData, _) ->
  report(1, "Code changing in IO"),
  {ok, StateData}.