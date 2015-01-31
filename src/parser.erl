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
%% @doc Methods for parsing packets.

-module(parser).

-export([parse/2]).

-import(jdb, [report/3, report/2, appenv/3, getenv/2]).

-include("types.hrl").
-include("enums.hrl").

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
parse(table_new, Data) ->
	parse(table_change, Data);
parse(task_new, Data) ->
	parse(task_change, Data);
parse(table_change, Data) ->
	report(1, "Parse NEW_TABLE packet"),
	<<TableId:?ID_LENGTH, Time:?UNIXTIME_LENGTH, Left/binary>> = Data,
	Table = parse_left(table, Left, #table{id=TableId, time=Time}),
	{ok, Table};
parse(task_change, Data) ->
	report(1, "Parse NEW_TASK packet"),
	<<TaskId:?ID_LENGTH, TableId:?ID_LENGTH, Time:?UNIXTIME_LENGTH, Left/binary>> = Data,
	Task = parse_left(task, Left, #task{id=TaskId, table_id=TableId, time=Time}),
	{ok, Task};
parse(permission, Data) ->
	report(1, "Parse PERMISSION packet"),
	<<TableId:?ID_LENGTH, UserId:?ID_LENGTH, Permission:8>> = Data,
	{ok, #permission{table_id=TableId, user_id=UserId, permission=Permission}};
parse(comment, Data) ->
	report(1, "Parse Comment packet"),
	<<TaskId:?ID_LENGTH, TableId:?ID_LENGTH, Time:?UNIXTIME_LENGTH, CommentLength:?STRING_LENGTH, CommentBin:CommentLength/bitstring>> = Data,
	Text = binary_to_list(CommentBin),
	{ok, #comment{table_id=TableId, task_id=TaskId, time=Time, text=Text}};
parse(Type, _) ->
	report(1, "Wrong parse packet call", {Type}),
	{error}.

parse_left(_, Data, Record) when Data =:= <<>> ->
	Record;
parse_left(table, Data, Table) ->
	<<Type:8, Left/bitstring>> = Data,
	case Type of
		?TABLE_DATA_NAME ->
			{Name, LeftData} = retrieve(string, Left),
			report(1, "Table name parsed", {Name}),
			parse_left(table, LeftData, Table#table{name=Name});
		?TABLE_DATA_DESC ->
			{Description, LeftData} = retrieve(string, Left),
			report(1, "Table description parsed", {Description}),
			parse_left(table, LeftData, Table#table{description=Description})
	end;
parse_left(task, Data, Task) ->
	<<Type:8, Left/bitstring>> = Data,
	case Type of 
		?TASK_DATA_NAME ->
			{Name, LeftData} = retrieve(string, Left),
			report(1, "Task name parsed", {Name}),
			parse_left(task, LeftData, Task#task{name=Name});
		?TASK_DATA_DESC ->
			{Description, LeftData} = retrieve(string, Left),
			report(1, "Task description parsed", {Description}),
			parse_left(task, LeftData, Task#task{description=Description});
		?TASK_DATA_START_DATE ->
			{StartDate, LeftData} = retrieve(string, Left),
			report(1, "Task start_date parsed", {StartDate}),
			parse_left(task, LeftData, Task#task{start_date=StartDate});
		?TASK_DATA_END_DATE ->
			{EndDate, LeftData} = retrieve(string, Left),
			report(1, "Task end_date parsed", {EndDate}),
			parse_left(task, LeftData, Task#task{end_date=EndDate});
		?TASK_DATA_START_TIME ->
			{StartTime, LeftData} = retrieve(string, Left),
			report(1, "Task start_time parsed", {StartTime}),
			parse_left(task, LeftData, Task#task{start_time=StartTime});
		?TASK_DATA_END_TIME ->
			{EndTime, LeftData} = retrieve(string, Left),
			report(1, "Task end_time parsed", {EndTime}),
			parse_left(task, LeftData, Task#task{end_time=EndTime});
		?TASK_DATA_PERIOD ->
			{Period, LeftData} = retrieve(short, Left),
			report(1, "Task period parsed", {Period}),
			parse_left(task, LeftData, Task#task{period=Period})
	end.

retrieve(string, Data) ->
	<<Length:?STRING_LENGTH, StringBin:Length/bitstring, Left/binary>> = Data,
	Value = binary_to_list(StringBin),
	{Value, Left};
retrieve(short, Data) ->
	<<Value:16, Left/binary>> = Data,
	{Value, Left}.