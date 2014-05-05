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

-include("types.hrl").
-include("enums.hrl").

%% Handling:
-export([start_link/1]).

%% Callbacks:
-export([init/1, terminate/2]).
-export([handle_call/3, handle_info/2, handle_cast/2]).
-export([code_change/3]).

-define(PACKET_SIZE, 16).
-define(TYPE_SIZE, 8).
-define(CHAR_SIZE, 8).
-define(STRING_LENGTH, 16).
-define(ID_LENGTH, 32).
-define(DATE_LENGTH, 64). %% ddMMyyyy
-define(TIME_LENGTH, 32). %% HHMM
-define(UNIXTIME_LENGTH, 64).

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

handle_cast({login, Name, Password}, State) ->
  report(1, "handle_cast LOGIN"),
  case login(State#state.socket, Name, Password) of
    {error} ->
      report(1, "Wrong auth data", {Name, Password}),
      {noreply, State};
    {ok, Id} ->
      report(1, "User successfully logined in", {Id, Name, Password}),
      NewState = State#state{user_id=Id},
      {noreply, NewState}
  end;
handle_cast({register, Name, Password}, State) ->
  report(1, "handle_cast REGISTER"),
  case register(State#state.socket, Name, Password) of
    {error} ->
      report(1, "Duplicate user name", {Name, Password}),
      {noreply, State};
    {ok, Id} ->
      report(1, "New user successfully registered", {Id, Name, Password}),
      NewState = State#state{user_id=Id},
      {noreply, NewState}
  end;
handle_cast({new_table, TableId, Time, Name, Description}, State) ->
  report(1, "handle_cast NEW_TABLE"),
  ok = new_table(State#state.socket, State#state.user_id, TableId, Time, Name, Description),
  {noreply, State};
handle_cast({new_task, TaskId, TableId, Time, Name, Description, StartDate, EndDate, StartTime, EndTime}, State) ->
  report(1, "handle_cast NEW_TASK"),
  ok = new_task(State#state.socket, State#state.user_id, TaskId, TableId, Time, Name, Description, StartDate, EndDate, StartTime, EndTime),
  {noreply, State};
handle_cast({new_commentary, TableId, TaskId, Time, Commentary}, State) ->
  report(1, "handle_cast NEW_COMMENTARY"),
  ok = new_commentary(State#state.socket, State#state.user_id, TableId, TaskId, Time, Commentary),
  {noreply, State};
handle_cast({table_change, TableId, Time, Name, Description}, State) ->
  report(1, "handle_cast TABLE_CHANGE"),
  ok = table_change(State#state.socket, State#state.user_id, TableId, Time, Name, Description),
  {noreply, State};
handle_cast({task_change, TaskId, TableId, Time, Name, Description, StartDate, EndDate, StartTime, EndTime}, State) ->
  report(1, "handle_cast TASK_CHANGE"),
  ok = task_change(State#state.socket, State#state.user_id, TableId, TaskId, Time, Name, Description, StartDate, EndDate, StartTime, EndTime),
  {noreply, State};
handle_cast({permission_change, TableId, UserId, Permission}, State) ->
  report(1, "handle_cast PERMISSION"),
  ok = permission_change(State#state.socket, TableId, UserId, Permission),
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
%%%       NewBuffer = binary (left data from proceeded Buffer)
%%%
%%% @doc Handling received from client packets
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
  do_register(Name, Password);
handle_packet(Type, Packet) when Type =:= ?CLIENT_LOGIN ->
  {ok, Name, Password} = parse(login, Packet),
  do_login(Name, Password);
handle_packet(Type, Packet) when Type =:= ?CLIENT_NEW_TABLE ->
  {ok, TableId, Time, Name, Description} = parse(table_data, Packet),
  create_new_table(TableId, Time, Name, Description);
handle_packet(Type, Packet) when Type =:= ?CLIENT_NEW_TASK ->
  {ok, TaskId, TableId, Time, Name, Description, StartDate, EndDate, StartTime, EndTime} = parse(task_data, Packet),
  create_new_task(TaskId, TableId, Time, Name, Description, StartDate, EndDate, StartTime, EndTime);
handle_packet(Type, Packet) when Type =:= ?CLIENT_TABLE_CHANGE ->
  {ok, TableId, Time, Name, Description} = parse(table_data, Packet),
  do_change_table(TableId, Time, Name, Description);
handle_packet(Type, Packet) when Type =:= ?CLIENT_TASK_CHANGE ->
  {ok, TaskId, TableId, Time, Name, Description, StartDate, EndDate, StartTime, EndTime} = parse(task_data, Packet),
  do_change_task(TaskId, TableId, Time, Name, Description, StartDate, EndDate, StartTime, EndTime);
handle_packet(Type, Packet) when Type =:= ?CLIENT_PERMISSION ->
  {ok, TableId, UserId, Permission} = parse(permission, Packet),
  do_change_permission(TableId, UserId, Permission);
handle_packet(Type, Packet) when Type =:= ?CLIENT_COMMENTARY ->
  {ok, TableId, TaskId, Time, Commentary} = parse(commentary, Packet),
  create_new_commentary(TableId, TaskId, Time, Commentary);
handle_packet(Type, _) ->
  report(1, "Wrong packet type", Type),
  {stop, normal}.

%%% @doc Sends message to socket
send(Type, Data, Socket) ->
  report(1, "Sending back", Data),
  Size = bit_size(Data),
  gen_tcp:send(Socket, <<Type:?TYPE_SIZE, Size:?PACKET_SIZE, Data/binary>>).

parse(login, Data) ->
  report(1, "Parse LOGIN packet"),
  <<NameLength:?STRING_LENGTH, NameBin:NameLength/bitstring, PasswordLength:?STRING_LENGTH, PasswordBin:PasswordLength/bitstring>> = Data,
  Name = binary_to_list(NameBin),
  Password = binary_to_list(PasswordBin),
  {ok, Name, Password};
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
  {ok, Table, Time, Name, Description};
parse(task_data, Data) ->
  report(1, "Parse NEW_TASK packet"),
  <<TaskId:?ID_LENGTH, TableId:?ID_LENGTH, Time:?UNIXTIME_LENGTH, 
    NameLength:?STRING_LENGTH, NameBin:NameLength/bitstring,
    DescLength:?STRING_LENGTH, DescBin:DescLength/bitstring,
    StartDateBin:?DATE_LENGTH/bitstring, EndDateBin:?DATE_LENGTH/bitstring,
    StartTimeBin:?TIME_LENGTH/bitstring, EndTimeBin:?TIME_LENGTH/bitstring>> = Data,
  Name = binary_to_list(NameBin),
  Description = binary_to_list(DescBin),
  StartDate = binary_to_list(StartDateBin),
  EndDate = binary_to_list(EndDateBin),
  StartTime = binary_to_list(StartTimeBin),
  EndTime = binary_to_list(EndTimeBin),
  {ok, TaskId, TableId, Time, Name, Description, StartDate, EndDate, StartTime, EndTime};
parse(permission, Data) ->
  report(1, "Parse PERMISSION packet"),
  <<TableId:?ID_LENGTH, UserId:?ID_LENGTH, Permission:8>> = Data,
  {ok, TableId, UserId, Permission};
parse(commentary, Data) ->
  report(1, "Parse COMMENTARY packet"),
  <<TaskId:?ID_LENGTH, TableId:?ID_LENGTH, Time:?UNIXTIME_LENGTH, CommentLength:?STRING_LENGTH, CommentBin:CommentLength/bitstring>> = Data,
  Commentary = binary_to_list(CommentBin),
  {ok, TableId, TaskId, Time, Commentary};
parse(_, _) ->
  report(1, "Wrong parse packet call"),
  {error}.

do_login(Name, Password) ->
  gen_server:cast(self(), {login, Name, Password}).
do_register(Name, Password) ->
  gen_server:cast(self(), {register, Name, Password}).
create_new_table(TaskId, Time, Name, Description) ->
  gen_server:cast(self(), {new_table, TaskId, Time, Name, Description}).
create_new_task(TaskId, TableId, Time, Name, Description, StartDate, EndDate, StartTime, EndTime) ->
  gen_server:cast(self(), {new_task, TaskId, TableId, Time, Name, Description, StartDate, EndDate, StartTime, EndTime}).
create_new_commentary(TableId, TaskId, Time, Commentary) ->
  gen_server:cast(self(), {new_commentary, TableId, TaskId, Time, Commentary}).
do_change_table(TableId, Time, Name, Description) ->
  gen_server:cast(self(), {table_change, TableId, Time, Name, Description}).
do_change_task(TaskId, TableId, Time, Name, Description, StartDate, EndDate, StartTime, EndTime) ->
  gen_server:cast(self(), {task_change, TaskId, TableId, Time, Name, Description, StartDate, EndDate, StartTime, EndTime}).
do_change_permission(TableId, UserId, Permission) ->
  gen_server:cast(self(), {permission_change, TableId, UserId, Permission}).

register(Socket, Name, Password) ->
  case database:check_username(Name) of
    [] ->
      AnswerSuccess = <<?REGISTER_SUCCESS:8>>,
      database:register(Name, Password),
      report(1, "New user registered", Name),
      send(?SERVER_REGISTER, AnswerSuccess, Socket),
      login(Socket, Name, Password);
    _ ->
      AnswerFailure = <<?REGISTER_FAILURE:8>>,
      send(?SERVER_REGISTER, AnswerFailure, Socket),
      {error}
  end.

login(Socket, Name, Password) ->
  case database:auth(Name, Password) of
    error ->
      Answer = <<?LOGIN_FAILURE:8>>,
      send(?SERVER_LOGIN, Answer, Socket),
      {error};
    {ok, Id} ->
      Answer = <<?LOGIN_SUCCESS:8, Id:?ID_LENGTH>>,
      report(1, "User logined in", Name),
      send(?SERVER_LOGIN, Answer, Socket),
      clients:add(Id, self()),
      {ok, Id}
  end.

new_table(_Socket, TableClientId, Time, UserId, Name, Description) ->
  {ok, TableId} = database:create_new_table(UserId, Time, Name, Description),
  send(?SERVER_GLOBAL_TABLE, <<TableClientId:?ID_LENGTH, TableId:?ID_LENGTH>>, Socket).
new_task(_Socket, UserId, TaskClientId, TableId, Time, Name, Description, StartDate, EndDate, StartTime, EndTime) ->
  {ok, TaskId} = database:create_new_task(UserId, TableId, Time, Name, Description, StartDate, EndDate, StartTime, EndTime),
  send(?SERVER_GLOBAL_TASK, <<TaskClientId:?ID_LENGTH, TaskId:?ID_LENGTH, TableId:?ID_LENGTH>>, Socket).
new_commentary(_Socket, UserId, TableId, TaskId, Time, Commentary) ->
  database:create_commentary(UserId, TableId, TaskId, Time, Commentary).
table_change(_Socket, UserId, TableId, Time, Name, Description) ->
  database:change_table(UserId, TableId, Time, Name, Description).
task_change(_Socket, UserId, TableId, TaskId, Time, Name, Description, StartDate, EndDate, StartTime, EndTime) ->
  database:change_task(UserId, TableId, TaskId, Time, Name, Description, StartDate, EndDate, StartTime, EndTime).
permission_change(_Socket, TableId, UserId, Permission) ->
  database:change_permission(TableId, UserId, Permission).

logout(UserId) ->
  if 
    UserId == undefined -> ok;
    true -> clients:remove(UserId)
  end.