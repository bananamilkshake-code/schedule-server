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
-include("enums.hrl").

-export([add/2, remove/1, update/2]).

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
handle_cast({table, {TableId, Time, UserId, Name, Description}}, State) ->
	table(State#state.clients_storage, TableId, Time, UserId, Name, Description),
	{noreply, State};
handle_cast({task, {TableId, TaskId, Time, UserId, Name, Description, StartDate, EndDate, StartTime, EndTime}}, State) ->
	task(State#state.clients_storage, TableId, TaskId, Time, UserId, Name, Description, StartDate, EndDate, StartTime, EndTime),
	{noreply, State};
handle_cast({comment, {TableId, TaskId, Time, UserId, Commentary}}, State) ->
	comment(State#state.clients_storage, TableId, TaskId, Time, UserId, Commentary),
	{noreply, State};
handle_cast({permission, {TableId, UserId, ReaderId, Permission}}, State) ->
	permission(State#state.clients_storage, TableId, UserId, ReaderId, Permission),
	{noreply, State};
handle_cast(_, State) ->
	report(1, "Unexpected clients cast"),
	{noreply, State}.

code_change(_OldVsn, State, _Extra) -> 
	report(1, "Unexpected clients code_change"),
	{ok, State}.

%% Private methods
online_users(TableId, UserId, Clients) ->
	Users = database:get_readers_for(TableId, UserId),
	report(1, "Users", Users),
	lists:foldl(fun(User, OnlineClients) ->
		case ets:lookup(Clients, User) of
			[Client] ->	lists:append(OnlineClients, Client#client.io_handler);
			[] -> OnlineClients
		end end, [], Users).

send(Clients, Type, Packet) ->
	lists:foreach(fun(IoClient) -> io_worker:cast(IoClient, {send, Type, Packet}) end, Clients).

table(Clients, TableId, Time, UserId, Name, Description) ->
	report(1, "table name", Name),
	report(1, "name len", string:len(Name)),
	Sockets = online_users(TableId, UserId, Clients),
	NameBin = list_to_binary(Name),
	NameLength = string:len(Name),
	DescBin = list_to_binary(Description),
	DescriptionLength = string:len(Description),
	send(Sockets, ?SERVER_CHANGE_TABLE, <<TableId:?ID_LENGTH, Time:?UNIXTIME_LENGTH, UserId:?ID_LENGTH, 
		NameLength:?STRING_LENGTH, NameBin/binary, DescriptionLength:?STRING_LENGTH, DescBin/binary>>).

task(Clients, TableId, TaskId, Time, UserId, Name, Description, StartDate, EndDate, StartTime, EndTime) ->
	Sockets = online_users(TableId, UserId, Clients),
	NameBin = list_to_binary(Name),
	NameLength = string:len(Name),
	DescriptionLength = string:len(Description),
	DescBin = list_to_binary(Description),
	StartDateBin = list_to_binary(StartDate),
	EndDateBin = list_to_binary(EndDate),
	StartTimeBin = list_to_binary(StartTime),
	EndTimeBin = list_to_binary(EndTime),
	send(Sockets, ?SERVER_CHANGE_TASK, <<TableId:?ID_LENGTH, TaskId:?ID_LENGTH, Time:?UNIXTIME_LENGTH, UserId:?ID_LENGTH, 
		NameLength:?STRING_LENGTH, NameBin/binary, DescriptionLength:?STRING_LENGTH, DescBin/binary,
		StartDateBin/binary, EndDateBin/binary, StartTimeBin/binary, EndTimeBin/binary>>).

comment(Clients, TableId, TaskId, Time, UserId, Commentary) ->
	Sockets = online_users(TableId, UserId, Clients),
	CommentLength = string:len(Commentary),
	CommentaryBin = list_to_binary(Commentary),
	send(Sockets, ?SERVER_COMMENTARY, <<TableId:?ID_LENGTH, TaskId:?ID_LENGTH, Time:?UNIXTIME_LENGTH, 
		UserId:?ID_LENGTH, CommentLength:?STRING_LENGTH, CommentaryBin/binary>>).

permission(Clients, TableId, UserId, ReaderId, Permission) ->
	Sockets = online_users(TableId, UserId, Clients),
	send(Sockets, ?SERVER_PERMISSION, <<TableId:?ID_LENGTH, ReaderId:?ID_LENGTH, Permission:8>>).

%% Public methods

add(Id, IOHandler) ->
	gen_server:cast(?MODULE, {add, Id, IOHandler}).
remove(Id) ->
	gen_server:cast(?MODULE, {remove, Id}).

update(table, Data) ->
	gen_server:cast(?MODULE, {table, Data});
update(task, Data) ->
	gen_server:cast(?MODULE, {task, Data});
update(comment, Data) ->
	gen_server:cast(?MODULE, {comment, Data});
update(permission, Data) ->
	gen_server:cast(?MODULE, {permission, Data}).