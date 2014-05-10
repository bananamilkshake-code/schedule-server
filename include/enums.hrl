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
%% @doc List of packet types and packet's data that can
%% be proceed between server and client.

-define(PACKET_SIZE, 16).
-define(TYPE_SIZE, 8).
-define(CHAR_SIZE, 8).
-define(STRING_LENGTH, 16).
-define(ID_LENGTH, 32).
-define(DATE_LENGTH, 64). %% ddMMyyyy
-define(TIME_LENGTH, 32). %% HHMM
-define(UNIXTIME_LENGTH, 64).

%%% Server's packets
-define(SERVER_REGISTER,		0).
-define(SERVER_LOGIN,			1).
-define(SERVER_GLOBAL_TABLE,	2).
-define(SERVER_GLOBAL_TASK, 	3).
-define(SERVER_CHANGE_TABLE,	4).
-define(SERVER_CHANGE_TASK,		5).
-define(SERVER_PERMISSION, 		6).
-define(SERVER_COMMENTARY,		7).
-define(SERVER_USER,			8).

%%% Client's packets
-define(CLIENT_REGISTER,	0).
-define(CLIENT_LOGIN,		1).
-define(CLIENT_NEW_TABLE,	2).
-define(CLIENT_NEW_TASK,	3).
-define(CLIENT_TABLE_CHANGE,4).
-define(CLIENT_TASK_CHANGE,	5).
-define(CLIENT_PERMISSION,	6).
-define(CLIENT_COMMENTARY,	7).

%%% Register status
-define(REGISTER_SUCCESS, 0).
-define(REGISTER_FAILURE, 1).

%%% Login status
-define(LOGIN_SUCCESS, 0).
-define(LOGIN_FAILURE, 1).

%%% Permission types
-define(PERMISSION_NONE, 0).
-define(PERMISSION_READ, 1).
-define(PERMISSION_WRITE, 2).