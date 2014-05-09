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

-include("enums.hrl").

%% Handling:
-export([start_link/1]).
-export([register/2, check_username/1, auth/2, 
	create_new_table/4, create_new_task/9, create_commentary/5, 
	change_table/5, change_task/10, change_permission/3, get_readers_for/2]).

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
handle_call({create_new_table, User, Time, Name, Description}, _From, DBHandler) ->
  Ret = create_new_table(DBHandler, User, Time, Name, Description),
  {reply, Ret, DBHandler};
handle_call({create_new_task, User, Table, Time, Name, Description, StartDate, EndDate, StartTime, EndTime}, _From, DBHandler) ->
  Ret = create_new_task(DBHandler, User, Table, Time, Name, Description, StartDate, EndDate, StartTime, EndTime),
  {reply, Ret, DBHandler};
handle_call({get_readers, TableId, UserId}, _From, DBHandler) ->
  Ret = get_readers(DBHandler, TableId, UserId),
  {reply, Ret, DBHandler};
handle_call(Call, From, _DBHandler) ->
  {stop, "Wrong database call", {Call, From}}.

handle_cast({create_commentary, _User, _Table, _Task, _Time, _Commentary}, DBHandler) ->
  {noreply, DBHandler};
handle_cast({change_table, User, Table, Time, Name, Description}, DBHandler) ->
  change_table(DBHandler, User, Table, Time, Name, Description),
  {noreply, DBHandler};
handle_cast({change_task, User, Table, Task, Time, Name, Description, StartDate, EndDate, StartTime, EndTime}, DBHandler) ->
  change_task(DBHandler, User, Table, Task, Time, Name, Description, StartDate, EndDate, StartTime, EndTime),
  {noreply, DBHandler};
handle_cast({change_permission, Table, User, Permission}, DBHandler) ->
  change_permission(DBHandler, Table, User, Permission),
  {noreply, DBHandler};
handle_cast(Data, DBHandler) ->
  report(0, "Wrong cast in Schedule Server database", Data),
  {noreply, DBHandler}.

code_change(_, DBHandler, _) ->
  report(1, "Code change in Schedule Server database"),
  {ok, DBHandler}.

%% Publc
check_username(Login) ->
	report(1, "Checking username", Login),
	gen_server:call(?MODULE, {check_username, Login}).
register(Login, Password) ->
	report(1, "Registering", {Login, Password}),
	gen_server:call(?MODULE, {register, Login, Password}).
auth(Login, Password) ->
	report(1, "Checking auth", {Login, Password}),
	gen_server:call(?MODULE, {auth, Login, Password}).
create_new_table(User, Time, Name, Description) ->
	report(1, "Creating new table", {User, Time, Name, Description}),
	gen_server:call(?MODULE, {create_new_table, User, Time, Name, Description}).
create_new_task(User, Table, Time, Name, Description, StartDate, EndDate, StartTime, EndTime) ->
	report(1, "Creating new task", {User, Table, Time, Name, Description, StartDate, EndDate, StartTime, EndTime}),
	gen_server:call(?MODULE, {create_new_task, User, Table, Time, Name, Description, StartDate, EndDate, StartTime, EndTime}).
create_commentary(User, Table, Task, Time, Commentary) ->
	report(1, "Create new commentary", {User, Table, Task, Time, Commentary}),
	gen_server:cast(?MODULE, {create_commentary, User, Table, Task, Time, Commentary}).
change_table(User, Table, Time, Name, Description) ->
	report(1, "Changing table", {User, Table, Time, Name, Description}),
	gen_server:cast(?MODULE, {change_table, User, Table, Time, Name, Description}).
change_task(User, Table, Task, Time, Name, Description, StartDate, EndDate, StartTime, EndTime) ->
	report(1, "Changing task", {User, Table, Task, Time, Name, Description, StartDate, EndDate, StartTime, EndTime}), 
	gen_server:cast(?MODULE, {change_task, User, Table, Task, Time, Name, Description, StartDate, EndDate, StartTime, EndTime}).
change_permission(User, Table, Permission) ->
	report(1, "Changing permission", {User, Table, Permission}),
	gen_server:cast(?MODULE, {change_permission, User, Table, Permission}).
get_readers_for(TableId, UserId) ->
	gen_server:call(?MODULE, {get_readers, TableId, UserId}).

%% Private
check_username(DBHandler, Login) ->
	{selected, _Cols, Rows} = odbc:param_query(DBHandler, "SELECT * FROM users WHERE login = ?",
		[{{sql_varchar, 20}, [Login]}]),
	get_first_column(Rows, error).

register(DBHandler, Login, Password) ->
	{updated, _Count} = odbc:param_query(DBHandler, "INSERT INTO users(login, password, register_time) VALUES (?, ?, ?)",
		[{{sql_varchar, 20}, [Login]},
		 {{sql_varchar, 20}, [Password]},
		 {sql_integer, [unixtimestamp()]}
		]).

auth(DBHandler, Login, Password) ->
	{selected, _Cols, Rows} = odbc:param_query(DBHandler, "SELECT id FROM users WHERE login = ? AND password = ?",
		[{{sql_varchar, 20}, [Login]},
		 {{sql_varchar, 20}, [Password]}
		]),
	get_first_column(Rows, error).

create_new_table(DBHandler, Time, User, Name, Description) ->
	{updated, _} = odbc:sql_query(DBHandler, "INSERT INTO tables VALUES()"),
	{selected, _Cols, Rows} = odbc:sql_query(DBHandler, "SELECT max(id) AS last_id FROM tables"),
	{ok, Table} = get_first_column(Rows, error),
	change_table(Table, Time, User, Name, Description),
	change_permission(Table, User, ?PERMISSION_WRITE),
	report(1, "New table added", Table),
	{ok, Table}.

create_new_task(DBHandler, User, Table, Time, Name, Description, StartDate, EndDate, StartTime, EndTime) ->
	{updated, _} = odbc:param_query(DBHandler, "INSERT INTO tasks(table_id) VALUES(?)", [{sql_integer, [Table]}]),
	{selected, _Cols, Rows} = odbc:param_query(DBHandler, "SELECT max(id) AS last_id FROM tasks WHERE table_id = ?", [{sql_integer, [Table]}]),
	{ok, Task} = get_first_column(Rows, error),
	change_task(User, Table, Task, Time, Name, Description, StartDate, EndDate, StartTime, EndTime),
	report(1, "New task added", Task),
	{ok, Task}.

change_table(DBHandler, Table, Time, User, Name, Description) ->
	{updated, _} = odbc:param_query(DBHandler, "INSERT INTO table_changes(table_id, time, user_id, name, description) VALUES(?, ?, ?, ?, ?)",	
				[{sql_integer, [Table]},
				 {sql_integer, [Time]},
				 {sql_integer, [User]},
				 {{sql_varchar, 100}, [Name]},
				 {{sql_varchar, 65535}, [Description]}
				]).

change_task(DBHandler, User, Table, Task, Time, Name, Description, StartDate, EndDate, StartTime, EndTime) ->
	{updated, _} = odbc:param_query(DBHandler, "INSERT INTO task_changes(task_id, table_id, time, user_id, name, description, start_date, end_date, start_time, end_time) VALUES(?, ?, ?, ?, ?, ?, STR_TO_DATE(?, \"%d%m%Y\"), STR_TO_DATE(?, \"%d%m%Y\"), STR_TO_DATE(?, \"%h%i\"), STR_TO_DATE(?, \"%h%i\"))",	
				[{sql_integer, [Table]},
				 {sql_integer, [Task]},
				 {sql_integer, [Time]},
				 {sql_integer, [User]},
				 {{sql_varchar, 100}, [Name]},
				 {{sql_varchar, 65535}, [Description]},
				 {{sql_varchar, 8}, [StartDate]},
				 {{sql_varchar, 8}, [EndDate]},
				 {{sql_varchar, 4}, [StartTime]},
				 {{sql_varchar, 4}, [EndTime]}
				]).

change_permission(DBHandler, Table, User, Permission) ->
	{updated, _} = odbc:param_query(DBHandler, "INSERT INTO readers VALUES (?, ?, ?)",
				[{sql_integer, [User]},
				 {sql_integer, [Table]},
				 {sql_integer, [Permission]}
				]).

get_readers(DBHandler, Table, User) ->
	{selected, _Cols, Rows} = odbc:param_query(DBHandler, "SELECT user_id FROM readers WHERE table_id = ? AND user_id != ?", 
				[{sql_integer, [Table]},
				 {sql_integer, [User]}
				]),
	lists:foldl(fun({Id}, List) -> lists:append(List, Id) end, [], Rows).

%% Helpers
get_first_column(List, Null) ->
	case List of
		[] -> 
			Null;
		[Row | _] ->
			I = element(1, Row), 
			{ok, I}
	end.