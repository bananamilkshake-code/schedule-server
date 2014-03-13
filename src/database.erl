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

-include("database.hrl").

%% Debug
-import(jdb, [report/3, report/2, appenv/3, getenv/2]).

-export([start/1, stop/0]).
-export([add_user/2, check_username/1, check_user/2]).

%% Creating database
install(Nodes) ->
	mnesia:create_schema(Nodes),
	mnesia:create_table(user,
		[{attributes, record_info(fields, user)}]).

%% Open already created database.
start(Nodes) ->
	error_logger:info_msg("Starting Mnesia database", self()),
	report(0, "Starting Mnesia database"),
	Dir = getenv(database_dir, "Unable to get Database Directory"),
	application:set_env(mnesia, dir, Dir),
	install(Nodes),
	ok.

%% Close database.
stop() ->
	ok.

add_user(Login, Password) ->
	F = fun() ->
		mnesia:write(#user{
			login=Login,
			password=Password,
			register_time=calendar:universal_time(),
			last_update=0,
			logout_time=0})
	end,
	mnesia:activity(transaction, F).

check_username(Login) ->
	F = fun() -> 
		UserMatch = #user{login=Login, _='_'},
		Guard = [],
		Return = [login],
		mnesia:select(user, [{UserMatch	, Guard, Return}])
	end,
	mnesia:activity(transaction, F).%%% will return a list of all items that fit the match specification

check_user(Login, Password) ->
	F = fun() -> 
		UserMatch = #user{login=Login, password=Password, _='_'},
		Guard = [],
		Return = [login],
		mnesia:select(user, [{UserMatch	, Guard, Return}])
	end,
	mnesia:activity(transaction, F).%%% will return a list of all items that fit the match specification