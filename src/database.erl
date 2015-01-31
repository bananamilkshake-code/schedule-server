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

-include("types.hrl").
-include("enums.hrl").

%% Handling:
-export([start_link/1]).
-export([register/2, check_username/1, auth/2, 
	create/2, change/2, load/3,
	get_readers_for/1, check_permission/3, logout_update/1]).

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

%% @doc Synchronous calls to database.
handle_call({check_username, Login}, _From, DBHandler) ->
	Ret = check_username(DBHandler, Login),
	{reply, Ret, DBHandler};
handle_call({register, Login, Password}, _From, DBHandler) ->
	Ret = register(DBHandler, Login, Password),
	{reply, Ret, DBHandler};
handle_call({auth, Login, Password}, _From, DBHandler) ->
	Ret = auth(DBHandler, Login, Password),
	{reply, Ret, DBHandler};
handle_call({create, Type, Data}, _From, DBHandler) ->
	Ret = create(Type, DBHandler, Data),
	{reply, Ret, DBHandler};
handle_call({change, Type, Data}, _From, DBHandler) ->
	Ret = change(Type, DBHandler, Data),
	{reply, Ret, DBHandler};
handle_call({load, Type, UserId, FromTime}, _From, DBHandler) ->
	Ret = load(Type, DBHandler, UserId, FromTime),
	{reply, Ret, DBHandler};
handle_call({get_readers, TableId}, _From, DBHandler) ->
	Ret = get_readers(DBHandler, TableId),
	{reply, Ret, DBHandler};
handle_call({check_permission, User, Table, Permission}, _From, DBHandler) ->
	Ret = check_permission(DBHandler, User, Table, Permission),
	{reply, Ret, DBHandler};
handle_call(Call, From, _DBHandler) ->
	{stop, "Wrong database call", {Call, From}}.

%%% @doc Asynchronous calls to database
handle_cast({create, Type, Data}, DBHandler) ->
	create(Type, DBHandler, Data),
	{noreply, DBHandler};
handle_cast({change, Type, Data}, DBHandler) ->
	change(Type, DBHandler, Data),
	{noreply, DBHandler};
 handle_cast({logout_update, User}, DBHandler) ->
	logout_update(DBHandler, User),
	{noreply, DBHandler};
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
	report(1, "Registering", {Login, Password}),
	gen_server:call(?MODULE, {register, Login, Password}).
auth(Login, Password) ->
	report(1, "Checking auth", {Login, Password}),
	gen_server:call(?MODULE, {auth, Login, Password}).

create(table, Table) ->
	report(1, "Creating new table", Table),
	gen_server:call(?MODULE, {create, table, Table});
create(task, Task) ->
	report(1, "Creating new task", Task),
	gen_server:call(?MODULE, {create, task, Task});
create(comment, Comment) ->
	report(1, "Create new comment", Comment),
	gen_server:cast(?MODULE, {create, comment, Comment}).

change(Type, Data) ->
	report(1, "Changing", {Type, Data}),
	gen_server:cast(?MODULE, {change, Type, Data}).
load(Type, UserId, FromTime) ->
	report(1, "Loading", {Type, UserId, FromTime}),
	gen_server:call(?MODULE, {load, Type, UserId, FromTime}).
logout_update(User) ->
	gen_server:cast(?MODULE, {logout_update, User}).
get_readers_for(Table) ->
	gen_server:call(?MODULE, {get_readers, Table}).
check_permission(UserId, TableId, Permission) ->
	gen_server:call(?MODULE, {check_permission, UserId, TableId, Permission}).

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

create(table, DBHandler, TableData) ->
	{updated, _} = odbc:sql_query(DBHandler, "INSERT INTO tables VALUES()"),
	{selected, _Cols, Rows} = odbc:sql_query(DBHandler, "SELECT MAX(id) AS last_id FROM tables"),
	{ok, Id} = get_first_column(Rows, error),{ok, TableDesc} = 
	Table = TableData#table{id=Id},
	change(table, Table),
	change(permission, #permission{permission=?PERMISSION_WRITE, table_id=Id, user_id=Table#table.creator_id, creator_id=Table#table.creator_id}),
	report(1, "New table added", {Id, Table}),
	{ok, Id};

create(task, DBHandler, TaskData) ->
	{selected, _Cols, Rows} = odbc:param_query(DBHandler, 
		"SELECT CAST(COALESCE(MAX(id), 0) + 1 AS UNSIGNED) AS last_id FROM tasks WHERE table_id = ?",
			[{sql_integer, [TaskData#task.table_id]}
		]),
	% There is some bug in odbc driver that returns string type from COALESCE result
	% instead of integer. If you check in MySQL what type must be returned in that query, it will be integer.
	{ok, IdStr} = get_first_column(Rows, error),
	{Id, _} = string:to_integer(IdStr),
	Task = TaskData#task{id=Id},
	{updated, _} = odbc:param_query(DBHandler, "INSERT INTO tasks(id, table_id) VALUES(?,?)", 
				[{sql_integer, [Task#task.id]},
				 {sql_integer, [Task#task.table_id]}
				]),
	report(1, "New task added", {Id, Task}),
	change(task, Task),
	{ok, Id};

create(comment, DBHandler, Comment) ->
	{updated, _} = odbc:param_query(DBHandler, "INSERT INTO comments(commentator_id, table_id, task_id, comment, time) VALUES(?, ?, ?, ?, ?)", 
				[{sql_integer, [Comment#comment.creator_id]},
				 {sql_integer, [Comment#comment.table_id]},
				 {sql_integer, [Comment#comment.task_id]},
				 {{sql_varchar, 100}, [Comment#comment.text]},
				 {sql_integer, [Comment#comment.time]}
				]),
	report(1, "New comment added", Comment).

change(table, DBHandler, Table) ->
	{updated, _} = odbc:param_query(DBHandler, "INSERT INTO table_changes(table_id, time, user_id, name, description) VALUES(?, ?, ?, ?, ?)",	
				[{sql_integer, [Table#table.id]},
				 {sql_integer, [Table#table.time]},
				 {sql_integer, [Table#table.creator_id]},
				 {{sql_varchar, 100}, [Table#table.name]},
				 {{sql_varchar, 65535}, [Table#table.description]}
				]),
	report(1, "Table changed", Table);

change(task, DBHandler, Task) ->
	{updated, _} = odbc:param_query(DBHandler, "INSERT INTO task_changes(task_id, table_id, time, user_id, name, description, start_date, end_date, start_time, end_time, period) VALUES(?, ?, ?, ?, ?, ?, STR_TO_DATE(?, \"%d%m%Y\"), STR_TO_DATE(?, \"%d%m%Y\"), STR_TO_DATE(?, \"%h%i\"), STR_TO_DATE(?, \"%h%i\"), ?)",
				[{sql_integer, [Task#task.id]},
				 {sql_integer, [Task#task.table_id]},
				 {sql_integer, [Task#task.time]},
				 {sql_integer, [Task#task.creator_id]},
				 {{sql_varchar, 100}, [Task#task.name]},
				 {{sql_varchar, 65535}, [Task#task.description]},
				 {{sql_varchar, 8}, [Task#task.start_date]},
				 {{sql_varchar, 8}, [Task#task.end_date]},
				 {{sql_varchar, 4}, [Task#task.start_time]},
				 {{sql_varchar, 4}, [Task#task.end_time]},
				 {sql_integer, [Task#task.period]}
				]), 
	report(1, "Task changed", Task);

change(permission, DBHandler, Permission) ->
	{updated, _} = odbc:param_query(DBHandler, "INSERT INTO readers(reader_id, table_id, permission) VALUES (?, ?, ?)",
				[{sql_integer, [Permission#permission.user_id]},
				 {sql_integer, [Permission#permission.table_id]},
				 {sql_integer, [Permission#permission.permission]}
				]),
	report(1, "Permission changed", Permission).

load(tables, DBHandler, UserId, FromTime) ->
	{selected, _Cols, Rows} = odbc:param_query(DBHandler, "SELECT * FROM table_changes WHERE user_id = ? AND time > ? ORDER BY time",
		[{sql_integer, [UserId]},
		 {sql_integer, [FromTime]}]),
	report(1, "Tables loaded", Rows),
	lists:flatmap(fun({TableId, Time, CreatorId, Name, Desc}) ->
					report(1, "Parsed", {TableId, Time, CreatorId, Name, Desc}),
					[#table{id=TableId, time=Time, creator_id=CreatorId, name=Name, description=Desc}]
					end,
				Rows);

load(tasks, DBHandler, UserId, FromTime) ->
	{selected, _Cols, Rows} = odbc:param_query(DBHandler, "SELECT * FROM task_changes WHERE user_id = ? AND time > ? ORDER BY time",
		[{sql_integer, [UserId]},
		 {sql_integer, [FromTime]}]),
	lists:flatmap(fun({TaskId, TableId, Time, CreatorId, Name, Desc, StartDate, EndDate, StartTime, EndTime, Period}) ->
					[#task{id=TaskId, table_id=TableId, time=Time, creator_id=CreatorId, name=Name, description=Desc, start_date=StartDate, end_date=EndDate, start_time=StartTime, end_time=EndTime, period=Period}]
					end,
				Rows).

logout_update(DBHandler, User) ->
	{updated, _} = odbc:param_query(DBHandler, "UPDATE users SET logout_time = UNIX_TIMESTAMP() WHERE id = ?",
				[{sql_integer, [User]}]),
	report(1, "Logout updated", {User}).

get_readers(DBHandler, Table) ->
	{selected, _Cols, Rows} = odbc:param_query(DBHandler, "SELECT reader_id FROM readers WHERE table_id = ?", 
				[{sql_integer, [Table]}]),
	Rows.

check_permission(DBHandler, User, Table, Permission) ->
	{selected, _Cols, Rows} = odbc:param_query(DBHandler, "SELECT reader_id FROM readers WHERE table_id = ? AND reader_id = ? AND permission >= ?",
				[{sql_integer, [Table]},
				 {sql_integer, [User]},
				 {sql_integer, [Permission]}
				]),
	length(Rows) > 0.

%% Helpers
get_first_column(List, Null) ->
	case List of
		[] -> 
			Null;
		[Row | _] ->
			I = element(1, Row), 
			{ok, I}
	end.