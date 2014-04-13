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
%% @doc This is the main Schedule Server server module describes callbacks for 
%% gen_server handling UDP socket for both incoming and outcoming connections.

-module(acceptor).
-behaviour(gen_server).
-include("types.hrl").

%% Callbacks:
-export([terminate/2]).
-export([init/1]).
-export([handle_info/2, handle_call/3, handle_cast/2]).
-export([code_change/3]).

%% Working with the server (starting):
-export([start_link/1]).

%%% @spec start_link(Args) -> Result
%%%    Args = term()
%%%    Result = {ok,Pid} | ignore | {error,Error}
%%%     Pid = pid()
%%%     Error = {already_started,Pid} | term()
%%%
%%% @doc Starts the Schedule TCP server. Args are ignored.
%%% For now server starts in active mode. That means, process
%%% will receive all the packets as Erlang messages, without 
%%% explicit recv/2 using.
%%%
start_link(Args) ->
  report(1, "Starting Schedule acceptor"),
  gen_server:start_link(
    {local, ?MODULE},
    ?MODULE,
    Args, 
    []
  ).

%% Callbacks:
%% @doc Configures and opens a port and stores it as gen_server internal state.
init(_) ->
  Port = getenv(tcp_port, "Unable to get TCP port"),
  report(1, "Listening port with acceptor"),
  {ok, Socket} = gen_tcp:listen(Port, [binary, {active, true}, {reuseaddr, true}]),
  gen_server:cast(self(), accept),
  {ok, Socket}.

%% @doc closes port at gen_server shutdown.
terminate(Reason, Socket) ->
  report(1, "Terminating acceptor"), 
  report(2, "Reason", Reason),
  gen_tcp:close(Socket). % closes the socket

%% @doc Handles message from the port. Since server is in active mode, all the messages are 
%% comming to the process as special Erlang messages.
handle_info(Data, State) ->
  report(0, "Wrong info in Schedule Server acceptor", Data),
  {noreply, State}.

handle_call(Data, _, State) ->
  report(0, "Wrong call in Schedule Server acceptor", Data),
  {reply, unknown, State}.

handle_cast(accept, Socket) ->
  accept(Socket),
  {noreply, null}.

code_change(_, State, _) ->
  report(1, "Code change in Schedule Server acceptor"),
  {ok, State}.

%%% @doc Accepting new client connection. Creates new io_worker and delegates 
%%% client handling to it.
accept(Socket) ->
  report(1, "Opening new connection"),
  {ok, SocketIo} = gen_tcp:accept(Socket),
  {ok, Child} = io_sup:start_io(SocketIo),
  gen_tcp:controlling_process(SocketIo, Child),
  accept(Socket).