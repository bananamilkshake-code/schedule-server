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

%%% Server's packets
-define(SERVER_REGISTER,	0).
-define(SERVER_LOGIN,		1).

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