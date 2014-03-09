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

%% Handling:
-export([start_link/1]).

%% Callbacks:
-export([init/1, terminate/2]).
-export([handle_call/3, handle_info/2, handle_cast/2]).
-export([code_change/3]).

-define(PACKET_SIZE, 16).
-define(TYPE_SIZE, 8).

-define(LOGIN, 0).

parse(Type, Packet, State) ->
  case (Type) of
    ?LOGIN ->
      report(1, "Sending back", Packet),
      send(0, Packet, State#state.socket),
      report(1, "Sended");
    _Else -> false
  end.

recv(Message, State) ->
  Buffer = State#state.buffer,
  NewBuffer = <<Buffer/binary, Message/binary>>,
  Size = bit_size(NewBuffer),
  report(1, "Size of the handled packets", Size),
  if
    Size > ?PACKET_SIZE -> %%% Check if we recieved size of the packet
      <<PacketSize:?PACKET_SIZE, Data/binary>> = NewBuffer,  %%% extract size of the packet and sended data
      report(1, "Packet size", PacketSize),
      if
        Size >= ?PACKET_SIZE + ?TYPE_SIZE + PacketSize -> %%% check if we received whole packet
          report(1, "data", Data),
          <<Type:?TYPE_SIZE, Packet:PacketSize/bitstring, LeftData/binary>> = Data,
          report(1, "Packet data", Packet),
          NewState = State#state{buffer = LeftData}, %%% saving new buffer
          parse(Type, Packet, State); %%% parse data from packet
        true->
          NewState = State#state{buffer = NewBuffer}
      end;
    true ->
      NewState = State#state{buffer = NewBuffer}
  end,
  {ok, NewState}.

send(Type, Data, Socket) ->
  Size = bit_size(Data),
  gen_tcp:send(Socket, <<Size:?PACKET_SIZE, Type:?TYPE_SIZE, Data/binary>>).

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
  report(1, "Message", Message),
  MessageSize = byte_size(Message),
  report(1, "Message size", MessageSize),
  {ok, NewState} = recv(Message, State),
  {noreply, NewState};
handle_info({tcp_closed, Socket}, State) ->
  report(1, "TCP connection was closed", Socket),
  {stop, normal, State};
handle_info({tcp_error, Socket}, State) ->
  report(1, "TCP error occured", Socket),
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