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

-module(clients).
-behaviour(gen_server).

-export([add/2, remove/1]).

%% Callbacks:
-export([init/1, terminate/2]).
-export([handle_call/3, handle_info/2, handle_cast/2]).
-export([code_change/3]).

-export([start_link/1]).

%% Debug
-import(jdb, [report/3, report/2, appenv/3, getenv/2, unixtimestamp/0]).

-record(client, {
	id, 		%% Integer
	io_handler 	%% io_worker
	}).

-record(state, {
	clients_storage	%% list of "client"
	}).

start_link(Args) ->
  report(1, "Starting clients storage"),
  gen_server:start_link(
    {local, ?MODULE},
    ?MODULE,
    Args, 
    []
  ).

init(_Args) ->
	Clients = ets:new(clients_storage, [set, protected, named_table, {write_concurrency, true}, {read_concurrency, true}, {keypos, #client.id}]),
	State = #state{clients_storage=Clients},
	{ok, State}.

terminate(_Reason, _State) ->
	ok.

handle_info(_Info, State) ->
	report(1, "Unexpected clients info"),
	{noreply, State}.

handle_call(_, _From, State) -> 
	report(1, "Unexpected clients call"),
	{noreply, State}.

%% @doc We can add new users asynchronously
handle_cast({add, Id, IOHandler}, State) -> 
	ets:insert(State#state.clients_storage, #client{id=Id, io_handler=IOHandler}),
	report(1, "New client added", {IOHandler, Id}),
	{noreply, State};
handle_cast({remove, Id}, State) -> 
	ets:delete(State#state.clients_storage, Id),
	report(1, "Client removed", Id),
	{noreply, State};
handle_cast(_, State) ->
	report(1, "Unexpected clients cast"),
	{noreply, State}.

code_change(_OldVsn, State, _Extra) -> 
	report(1, "Unexpected clients code_change"),
	{ok, State}.

%% Public methods

add(Id, IOHandler) ->
	gen_server:cast(?MODULE, {add, Id, IOHandler}).

remove(Id) ->
	gen_server:cast(?MODULE, {remove, Id}).