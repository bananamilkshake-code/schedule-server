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

-module(database).
-behaviour(gen_server).

-include("database.hrl").

%% Handling:
-export([start_link/1]).

%% Callbacks:
-export([init/1, terminate/2]).
-export([handle_call/3, handle_info/2, handle_cast/2]).
-export([code_change/3]).

%% Debug
-import(jdb, [report/3, report/2, appenv/3, getenv/2]).

-export([authorize/2, logout/1,
		add_user/2, check_username/1, check_user/2]).

start_link(Args) ->
	report(1, "Starting database"),
  gen_server:start_link(
    {local, ?MODULE},
    ?MODULE,
    Args, 
    []
  ).

%% Callbacks:  
%% @doc Creates database schema and loads data from saved tables
init([Params]) ->
	case odbc:connect(Params, []) ->
		{ok, DBHandler} -> 
			{ok, DBHandler};
		{error, Reason} ->
			{stop, Reason}
	end.

%% @doc closes port at gen_server shutdown.
terminate(Reason, DBHandler) ->
  report(1, "Terminating database"), 
  report(2, "Reason", Reason),
  odbc:disconnect(DBHandler).

%% @doc Handles message from the port. Since server is in active mode, all the messages are 
%% comming to the process as special Erlang messages.
handle_info(Data, DBHandler) ->
  report(0, "Wrong info in Schedule Server database", Data),
  {noreply, DBHandler}.

handle_call(Data, From, DBHandler) ->
  report(0, "Wrong call in Schedule Server database", Data),
  {reply, unknown, State}.

handle_cast(Data, DBHandler) ->
  report(0, "Wrong cast in Schedule Server database", Data),
  {noreply, State}.

code_change(_, State, _) ->
  report(1, "Code change in Schedule Server database"),
  {ok, State}.

%%% Add new client to clients list (move to clients supervisor)
authorize(_Login, _Socket) ->
	end.

%%% Remove client to clients list (move to clients supervisor)
logout(_Login) ->
	end.

add_user(DBHandler, Login, Password) ->
	.

check_username(DBHandler, Login) ->
	F = fun() -> 
		UserMatch = #user{login=Login, _='_'},
		Guard = [],
		Return = [login],
		mnesia:select(user, [{UserMatch	, Guard, Return}])
	end,
	mnesia:activity(transaction, F).%%% will return a list of all items that fit the match specification

check_user(DBHandler, Login, Password) ->
	F = fun() -> 
		UserMatch = #user{login=Login, password=Password, _='_'},
		Guard = [],
		Return = [login],
		mnesia:select(user, [{UserMatch	, Guard, Return}])
	end,
	mnesia:activity(transaction, F).%%% will return a list of all items that fit the match specification

%%% Private functions
