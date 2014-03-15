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
-include("database.hrl").

-import(database, [add_user/2, check_username/2, check_user/2]).

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

handle_cast(Data, State) ->
  report(1, "Wrong cast event on IO", Data),
  {stop, normal, State}.

handle_call(Data, _, State) ->
  report(0, "Wrong sync event in IO", Data),
  {reply, ok, State}.

%% @hidden
handle_info({tcp, _Socket, Message}, State) ->
  {ok, NewState} = proceed(Message, State),
  {noreply, NewState};
handle_info({tcp_closed, Socket}, State) ->
  report(1, "TCP connection was closed", Socket),
  database:logout(State#state.user),
  {stop, normal, State};
handle_info({tcp_error, Socket}, State) ->
  report(1, "TCP error occured", Socket),
  database:logout(State#state.user),
  {stop, normal, State}.

%% @hidden
terminate(Reason, _) ->
  report(1, "Terminating IO"), 
  report(2, "Reason", Reason),
  ok.

%% @hidden
code_change(_, State, _) ->
  report(1, "Code changing in IO"),
  {ok, State}.

%%% @doc Handling received from client packets
proceed(Message, State) ->
  Buffer = State#state.buffer,
  NewBuffer = <<Buffer/binary, Message/binary>>,
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
          {ok, RetState} = parse(Type, Packet, State), %%% parse data from packet and get new state
          NewState = RetState#state{buffer = LeftData}; %%% saving new buffer
        true->
          NewState = State#state{buffer = NewBuffer}
      end;
    true ->
      NewState = State#state{buffer = NewBuffer}
  end,
  {ok, NewState}.

%%% @doc Parse client data by packet type
parse(Type, Packet, State) when Type =:= ?CLIENT_REGISTER ->
  register_user(Packet, State);
parse(Type, Packet, State) when Type =:= ?CLIENT_LOGIN ->
  login(Packet, State);
parse(Type, _, State) ->
  report(1, "Wrong packet type", Type),
  {stop, normal, State}.

%%% @doc Sends message to socket
send(Type, Data, Socket) ->
  report(1, "Sending back", Data),
  Size = bit_size(Data),
  gen_tcp:send(Socket, <<Type:?TYPE_SIZE, Size:?PACKET_SIZE, Data/binary>>).

register_user(Data, State) ->
  <<NameLength:?STRING_LENGTH, Name:NameLength/bitstring, PasswordLength:?STRING_LENGTH, Password:PasswordLength/bitstring>> = Data,
  case database:check_username(Name) of
    [] ->
      AnswerSuccess = <<?REGISTER_SUCCESS:8>>,
      database:add_user(Name, Password),
      report(1, "New user registered", Name),
      send(?SERVER_REGISTER, AnswerSuccess, State#state.socket),
      {ok, State#state{user=Name}};
    _ ->
      AnswerFailure = <<?REGISTER_FAILURE:8>>,
      send(?SERVER_REGISTER, AnswerFailure, State#state.socket),
      {ok, State}
  end.

login(Data, State) ->
  <<NameLength:?STRING_LENGTH, Name:NameLength/bitstring, PasswordLength:?STRING_LENGTH, Password:PasswordLength/bitstring>> = Data,
  case database:check_user(Name, Password) of
    [] ->
      Answer = <<?LOGIN_FAILURE:8>>,
      send(?SERVER_LOGIN, Answer, State#state.socket),
      {ok, State};
    [User] ->
      Answer = <<?LOGIN_SUCCESS:8>>,
      report(1, "User logined in", Name),
      send(?SERVER_LOGIN, Answer, State#state.socket),
      {ok, State#state{user=User}}
  end.