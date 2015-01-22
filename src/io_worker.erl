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

-module(io_worker).
-behaviour(gen_server).

-import(jdb, [report/3, report/2, appenv/3, getenv/2]).
-import(database, [load/3]).

-include("types.hrl").
-include("enums.hrl").

%% Handling:
-export([start_link/1]).

%% Callbacks:
-export([init/1, terminate/2]).
-export([handle_call/3, handle_info/2, handle_cast/2]).
-export([code_change/3]).

-export([message/2]).

%%% The worker state. Handles information about connection.
-record(state, {
	socket,
	buffer,		% keep TCP packeet here until whole data received
	user_id		% loggined user id
}).

%%% @spec start_link() -> Result
%%%    Result = {ok,Pid} | ignore | {error,Error}
%%%     Pid = pid()
%%%     Error = {already_started,Pid} | term()
%%%  
%%% @doc Creates new Schedule Server packet processor.
%%%
start_link(Socket) ->
	gen_server:start_link(io_worker, [Socket], []).

%% Callbacks:
init([Socket]) ->
	report(1, "IO started"),
	{ok, #state{socket=Socket, buffer = <<>>}}.

handle_call(Data, _From, State) ->
	report(0, "Wrong sync event in IO", Data),
	{reply, ok, State}.

%%% @spec handle_cast(Message, State) -> Result
%%%		Message = term()
%%%
%%%		Result = {noreply, State} | {stop, normal, State}
%%%		State = {socket, buffer, user_id}
%%%
%%%	@doc Handles asynchronous message to io_worker.
%%%

handle_cast({login, Name, Password, LastUpdateTime}, State) ->
	report(1, "handle_cast LOGIN"),
	case do_login(Name, Password) of
		{error} ->
			report(1, "Wrong auth data", {Name, Password}),
			{noreply, State};
		{ok, Id} ->
			report(1, "User successfully logined in", {Id, Name, Password}),
			NewState = State#state{user_id=Id},
			sync(LastUpdateTime),
			{noreply, NewState}
	end;
handle_cast({register, Name, Password}, State) ->
	report(1, "handle_cast REGISTER"),
	case do_register(Name, Password) of
		{error} ->
			report(1, "Duplicate user name", {Name, Password}),
			{noreply, State};
		{ok, Id} ->
			report(1, "New user successfully registered", {Id, Name, Password}),
			NewState = State#state{user_id=Id},
			{noreply, NewState}
	end;

handle_cast({new_table, Table}, State) ->
	report(1, "handle_cast NEW_TABLE"),
	do_create(table, Table#table{creator_id=State#state.user_id}),
	{noreply, State};
handle_cast({new_task, Task}, State) ->
	report(1, "handle_cast NEW_TASK"),
	do_create(task, Task#task{creator_id=State#state.user_id}),
	{noreply, State};
handle_cast({new_comment, Comment}, State) ->
	report(1, "handle_cast NEW_Comment"),
	do_create(comment, Comment#comment{creator_id=State#state.user_id}),
	{noreply, State};
handle_cast({table_change, Table}, State) ->
	report(1, "handle_cast TABLE_CHANGE"),
	do_change(table, Table#table{creator_id=State#state.user_id}),
	{noreply, State};
handle_cast({task_change, Task}, State) ->
	report(1, "handle_cast TASK_CHANGE"),
	do_change(task, Task#task{creator_id=State#state.user_id}),
	{noreply, State};
handle_cast({permission_change, Permission}, State) ->
	report(1, "handle_cast PERMISSION"),
	do_change(permission, Permission#permission{creator_id=State#state.user_id}),
	{noreply, State};

handle_cast({message_table, Table}, State) ->
	report(1, "handle_cast Someone made table change"),
	message(table, Table),
	{noreply, State};
handle_cast({message_task, Task}, State) ->
	report(1, "handle_cast Someone made task change"),
	message(task, Task),
	{noreply, State};

handle_cast({sync, FromTime}, State) ->
	report(1, "handle_cast SYNC"),
	do_sync(State#state.user_id, FromTime),
	{noreply, State};
handle_cast({send, Type, Data}, State) ->
	send(Type, Data, State#state.socket),
	{noreply, State};
handle_cast(Data, State) ->
	report(1, "Wrong cast event on IO", Data),
	{stop, normal, State}.

%% @hidden
handle_info({tcp, _Socket, Message}, State) ->
	{ok, NewBuffer} = proceed(Message, State#state.buffer),
	{noreply, State#state{buffer=NewBuffer}};
handle_info({tcp_closed, Socket}, State) ->
	report(1, "TCP connection was closed", Socket),
	{stop, normal, State};
handle_info({tcp_error, Socket}, State) ->
	report(1, "TCP error occured", Socket),
	{stop, normal, State}.

%% @hidden
terminate(Reason, State) ->
	report(1, "Terminating IO"), 
	report(2, "Reason", Reason),
	logout(State#state.user_id),
	ok.

%% @hidden
code_change(_, State, _) ->
	report(1, "Code changing in IO"),
	{ok, State}.

%%% @spec proceed(Message, Buffer) -> Result
%%%     Result = {ok, NewBuffer}
%%%     NewBuffer = binary (left data from proceeded Buffer)
%%%
%%% @doc Handling received from client packets. Due to fragmentation of packets this
%%% method collects received data (Message) into buffer (appends it to previously collected
%%% data in Buffer). Method checks that information about packet's type, size and its data is 
%%% already received. If it is true, it cuts packet data from buffer and proceeds it to
%%% handle_packet() method.
%%%
proceed(Message, Buffer) ->
	NewBuffer = <<Buffer/binary, Message/binary>>,
	report(1, "New packet was received", NewBuffer),
	BufferSize = bit_size(NewBuffer),
	report(1, "Size of the handled packets", BufferSize),
	if
		BufferSize > ?TYPE_SIZE + ?PACKET_SIZE ->    %%% Check if we recieved type and size of the packet
			<<Type:?TYPE_SIZE, Size:?PACKET_SIZE, Data/binary>> = NewBuffer,  %%% extract type, size and recieved data of the packet
			report(1, "Packet size", Size),
			if
				BufferSize >= ?PACKET_SIZE + ?TYPE_SIZE + Size -> %%% check if we received whole packet
					<<Packet:Size/bitstring, LeftData/binary>> = Data,
					report(1, "Packet data", Packet),
					handle_packet(Type, Packet), %%% parse data from packet and get new state
					{ok, LeftData};              %%% saving new buffer
				true->
					{ok, NewBuffer}
			end;
		true ->
			{ok, NewBuffer}
	end.

%%% @doc Parse client data by packet type
handle_packet(Type, Packet) when Type =:= ?CLIENT_REGISTER ->
	{ok, Name, Password} = parse(register, Packet),
	register_new(Name, Password);
handle_packet(Type, Packet) when Type =:= ?CLIENT_LOGIN ->
	{ok, Name, Password, LastUpdateTime} = parse(login, Packet),
	login(Name, Password, LastUpdateTime);
handle_packet(Type, Packet) when Type =:= ?CLIENT_NEW_TABLE ->
	{ok, Table} = parse(table_data, Packet),
	create(table, Table);
handle_packet(Type, Packet) when Type =:= ?CLIENT_NEW_TASK ->
	{ok, Task} = parse(task_data, Packet),
	create(task, Task);
handle_packet(Type, Packet) when Type =:= ?CLIENT_TABLE_CHANGE ->
	{ok, Table} = parse(table_data, Packet),
	change(table, Table);
handle_packet(Type, Packet) when Type =:= ?CLIENT_TASK_CHANGE ->
	{ok, Task} = parse(task_data, Packet),
	change(task, Task);
handle_packet(Type, Packet) when Type =:= ?CLIENT_PERMISSION ->
	{ok, Permission} = parse(permission, Packet),
	change(permission, Permission);
handle_packet(Type, Packet) when Type =:= ?CLIENT_COMMENT ->
	{ok, Comment} = parse(comment, Packet),
	create(comment, Comment);
handle_packet(Type, _) ->
	report(1, "Wrong packet type", Type),
	{stop, normal}.

%%% @doc Sends message to socket
send(Type, Data, Socket) ->
	report(1, "Sending back", Data),
	Size = byte_size(Data),
	gen_tcp:send(Socket, <<Type:?TYPE_SIZE, Size:?PACKET_SIZE, Data/binary>>).

parse(login, Data) ->
	report(1, "Parse LOGIN packet"),
	<<NameLength:?STRING_LENGTH, NameBin:NameLength/bitstring, PasswordLength:?STRING_LENGTH, PasswordBin:PasswordLength/bitstring, LastUpdateTime:?UNIXTIME_LENGTH>> = Data,
	Name = binary_to_list(NameBin),
	Password = binary_to_list(PasswordBin),
	{ok, Name, Password, LastUpdateTime};
parse(register, Data) ->
	report(1, "Parse registered packet"),
	<<NameLength:?STRING_LENGTH, NameBin:NameLength/bitstring, PasswordLength:?STRING_LENGTH, PasswordBin:PasswordLength/bitstring>> = Data,
	Name = binary_to_list(NameBin),
	Password = binary_to_list(PasswordBin),
	{ok, Name, Password};
parse(table_data, Data) ->
	report(1, "Parse NEW_TABLE packet"),
	<<TableId:?ID_LENGTH, Time:?UNIXTIME_LENGTH, NameLength:?STRING_LENGTH, NameBin:NameLength/bitstring, DescLength:?STRING_LENGTH, DescBin:DescLength/bitstring>> = Data,
	Name = binary_to_list(NameBin),
	Description = binary_to_list(DescBin),
	{ok, #table{id=TableId, time=Time, name=Name, description=Description}};
parse(task_data, Data) ->
	report(1, "Parse NEW_TASK packet"),
	<<TaskId:?ID_LENGTH, TableId:?ID_LENGTH, Time:?UNIXTIME_LENGTH, 
		NameLength:?STRING_LENGTH, NameBin:NameLength/bitstring,
		DescLength:?STRING_LENGTH, DescBin:DescLength/bitstring,
		StartDateBin:?DATE_LENGTH/bitstring, EndDateBin:?DATE_LENGTH/bitstring,
		StartTimeBin:?TIME_LENGTH/bitstring, EndTimeBin:?TIME_LENGTH/bitstring, Period:8>> = Data,
	Name = binary_to_list(NameBin),
	Description = binary_to_list(DescBin),
	StartDate = binary_to_list(StartDateBin),
	EndDate = binary_to_list(EndDateBin),
	StartTime = binary_to_list(StartTimeBin),
	EndTime = binary_to_list(EndTimeBin),
	{ok, #task{id=TaskId, table_id=TableId, time=Time, name=Name, description=Description, start_date=StartDate, end_date=EndDate, start_time=StartTime, end_time=EndTime, period=Period}};
parse(permission, Data) ->
	report(1, "Parse PERMISSION packet"),
	<<TableId:?ID_LENGTH, UserId:?ID_LENGTH, Permission:8>> = Data,
	{ok, #permission{table_id=TableId, user_id=UserId, permission=Permission}};
parse(comment, Data) ->
	report(1, "Parse Comment packet"),
	<<TaskId:?ID_LENGTH, TableId:?ID_LENGTH, Time:?UNIXTIME_LENGTH, CommentLength:?STRING_LENGTH, CommentBin:CommentLength/bitstring>> = Data,
	Text = binary_to_list(CommentBin),
	{ok, #comment{table_id=TableId, task_id=TaskId, time=Time, text=Text}};
parse(_, _) ->
	report(1, "Wrong parse packet call"),
	{error}.

login(Name, Password, LastUpdateTime) ->
	gen_server:cast(self(), {login, Name, Password, LastUpdateTime}).
register_new(Name, Password) ->
	gen_server:cast(self(), {register, Name, Password}).
create(table, Table) ->
	gen_server:cast(self(), {new_table, Table});
create(task, Task) ->
	gen_server:cast(self(), {new_task, Task});
create(comment, Comment) ->
	gen_server:cast(self(), {new_Comment, Comment}).
change(table, Table) ->
	gen_server:cast(self(), {table_change, Table});
change(task, Task) ->
	gen_server:cast(self(), {task_change, Task});
change(permission, Permission) ->
	gen_server:cast(self(), {permission_change, Permission}).
send(Type, Data) ->
	gen_server:cast(self(), {send, Type, Data}).
sync(FromTime) ->
	gen_server:cast(self(), {sync, FromTime}).


%%% Hidden methods. Called only by io_worker itself.
%%% It is doing real work.
%%%

do_register(Name, Password) ->
	report(1, "Registering", Name),
	case database:check_username(Name) of
		error ->
			database:register(Name, Password),
			report(1, "New user registered", Name),
			send(?SERVER_REGISTER, <<?REGISTER_SUCCESS:8>>),
			login(Name, Password, 0);
		_ ->
			send(?SERVER_REGISTER, <<?REGISTER_FAILURE:8>>),
			{error}
	end.

do_login(Name, Password) ->
	report(1, "Logining in", Name),
	case database:auth(Name, Password) of
		error ->
			Answer = <<?LOGIN_FAILURE:8>>,
			send(?SERVER_LOGIN, Answer),
			{error};
		{ok, Id} ->
			Answer = <<?LOGIN_SUCCESS:8, Id:?ID_LENGTH>>,
			report(1, "User logined in", Name),
			send(?SERVER_LOGIN, Answer),
			clients:add(Id, self()),
			{ok, Id}
	end.

%%% Called via client packets. It creates new data.

do_create(table, Table) ->
	{ok, Id} = database:create(table, Table),
	%send(?SERVER_GLOBAL_TABLE, <<TableClientId:?ID_LENGTH, TableId:?ID_LENGTH>>, Socket),
	clients:update(table, Table#table{id=Id});

do_create(task, Task) ->
	{ok, Id} = database:create(task, Task),
	%send(?SERVER_GLOBAL_TASK, <<TaskClientId:?ID_LENGTH, TaskId:?ID_LENGTH, TableId:?ID_LENGTH>>, Socket),
	clients:update(task, Task#task{id=Id});

do_create(comment, Comment) ->
	case database:check_permission(Comment#comment.creator_id, Comment#comment.table_id, ?PERMISSION_READ) of
		true ->
			data_changed(comment, Comment);
		false ->
			report(1, "User do not have permission to create Comment", {Comment#comment.creator_id, Comment#comment.table_id})
	end.

do_change(table, Table) ->
	case database:check_permission(Table#table.creator_id, Table#table.id, ?PERMISSION_WRITE) of
		true ->
			data_changed(table, Table);
		false ->
			report(1, "User do not have permission to change table", {Table#table.creator_id, Table#table.id})
	end;

do_change(task, Task) ->
	case database:check_permission(Task#task.creator_id, Task#task.table_id, ?PERMISSION_WRITE) of
		true ->
			data_changed(task, Task);
		false ->
			report(1, "User do not have permission to change task", {Task#task.creator_id, Task#task.table_id})
	end;

do_change(permission, Permission) ->
	case database:check_permission(Permission#permission.creator_id, Permission#permission.table_id, ?PERMISSION_WRITE) of
		true ->
			data_changed(permission, Permission);
		false ->
			report(1, "User do not have permission to change permissions", {Permission#permission.creator_id, Permission#permission.table_id})
	end.

data_changed(Type, Data) ->
	database:change(Type, Data),
	clients:update(Type, Data).

logout(UserId) ->
	if 
		UserId == undefined -> ok;
		true -> clients:remove(UserId)
	end.

%%%
%%% Called via clients
%%%
message(table, Table) ->
	report(1, "Sending message about created table"),
	TableId = Table#table.id,
	Time = Table#table.time,
	CreatorId = Table#table.creator_id,
	NameBin = list_to_binary(Table#table.name),
	NameLength = string:len(Table#table.name),
	DescBin = list_to_binary(Table#table.description),
	DescriptionLength = string:len(Table#table.description),
	send(?SERVER_CHANGE_TABLE, <<TableId:?ID_LENGTH, Time:?UNIXTIME_LENGTH, CreatorId:?ID_LENGTH, 
		NameLength:?STRING_LENGTH, NameBin/binary, DescriptionLength:?STRING_LENGTH, DescBin/binary>>);

message(task, Task) ->
	TableId = Task#task.id,
	TaskId = Task#task.id,
	Time = Task#task.time,
	CreatorId = Task#task.creator_id,
	Period = Task#task.period,
	NameBin = list_to_binary(Task#task.name),
	NameLength = string:len(Task#task.name),
	DescriptionLength = string:len(Task#task.description),
	DescBin = list_to_binary(Task#task.description),
	StartDateBin = list_to_binary(Task#task.start_date),
	EndDateBin = list_to_binary(Task#task.end_date),
	StartTimeBin = list_to_binary(Task#task.start_time),
	EndTimeBin = list_to_binary(Task#task.end_time),
	send(?SERVER_CHANGE_TASK, <<TableId:?ID_LENGTH, TaskId:?ID_LENGTH, Time:?UNIXTIME_LENGTH, CreatorId:?ID_LENGTH, 
		NameLength:?STRING_LENGTH, NameBin/binary, DescriptionLength:?STRING_LENGTH, DescBin/binary,
		StartDateBin/binary, EndDateBin/binary, StartTimeBin/binary, EndTimeBin/binary, Period:8>>).

sync_data(ListDataAtom, DataAtom, UserId, FromTime) ->
	Data = database:load(ListDataAtom, UserId, FromTime),
	lists:foreach(fun(Element) -> message(DataAtom, Element) end, Data).

do_sync(UserId, FromTime) ->
	sync_data(tables, table, UserId, FromTime),
	sync_data(tasks, task, UserId, FromTime).