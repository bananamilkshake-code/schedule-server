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

%% Handling:
-export([start_link/1]).
-export([register/2, check_username/1, auth/2]).

%% Callbacks:
-export([init/1, terminate/2]).
-export([handle_call/3, handle_info/2, handle_cast/2]).
-export([code_change/3]).

%% Debug
-import(jdb, [report/3, report/2, appenv/3, getenv/2, unixtimestamp/0]).

start_link(Params) ->
	report(1, "Starting database"),
  gen_server:start_link(
    {local, ?MODULE},
    ?MODULE,
    Params, 
    []
  ).

%% Callbacks:  
%% @doc Creates database schema and loads data from saved tables
init(Params) ->
	case odbc:connect(Params, []) of
		{ok, DBHandler} -> 
			{ok, DBHandler};
		{error, Reason} ->
			{stop, Reason}
	end.

%% @doc Closes database connection when server stops
terminate(Reason, DBHandler) ->
  report(1, "Terminating database"), 
  report(2, "Reason", Reason),
  odbc:disconnect(DBHandler).

%% @doc Handles message from the port. Since server is in active mode, all the messages are 
%% comming to the process as special Erlang messages.
handle_info(Data, DBHandler) ->
  report(0, "Wrong info in Schedule Server database", Data),
  {noreply, DBHandler}.

%% @doc Handles database command
handle_call({check_username, Login}, _From, DBHandler) ->
  Ret = check_username(DBHandler, Login),
  {reply, Ret, DBHandler};
handle_call({register, Login, Password}, _From, DBHandler) ->
  Ret = register(DBHandler, Login, Password),
  {reply, Ret, DBHandler};
handle_call({auth, Login, Password}, _From, DBHandler) ->
  Ret = auth(DBHandler, Login, Password),
  {reply, Ret, DBHandler};
handle_call(Call, From, _DBHandler) ->
  {stop, "Wrong database call", {Call, From}}.

handle_cast(Data, DBHandler) ->
  report(0, "Wrong cast in Schedule Server database", Data),
  {noreply, DBHandler}.

code_change(_, DBHandler, _) ->
  report(1, "Code change in Schedule Server database"),
  {ok, DBHandler}.

%% Publc
check_username(Login) ->
	gen_server:call(?MODULE, {check_username, Login}).

register(Login, Password) ->
	gen_server:call(?MODULE, {register, Login, Password}).

auth(Login, Password) ->
	gen_server:call(?MODULE, {auth, Login, Password}).

%% Private
check_username(DBHandler, Login) ->
	{selected, _Cols, Rows} = odbc:param_query(DBHandler, "SELECT * FROM users WHERE login = ?",
		[{{sql_varchar, 20}, [Login]}]),
	Rows.

register(DBHandler, Login, Password) ->
	{updated, _Count} = odbc:param_query(DBHandler, "INSERT INTO users(login, password, register_time) VALUES (?, ?, ?)",
		[{{sql_varchar, 20}, [Login]},
		 {{sql_varchar, 20}, [Password]},
		 {sql_integer, [unixtimestamp()]}
		]).

get_first_column(List, Null) ->
	case List of
		[] -> Null;
		[Row | _] ->
			I = element(1, Row), 
			{ok, I}
	end.

auth(DBHandler, Login, Password) ->
	{selected, _Cols, Rows} = odbc:param_query(DBHandler, "SELECT id FROM users WHERE login = ? AND password = ?",
		[{{sql_varchar, 20}, [Login]},
		 {{sql_varchar, 20}, [Password]}
		]),
	get_first_column(Rows, error).
