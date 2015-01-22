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

-record(table, {
	id,
	time,
	creator_id,
	name,
	description
}).

-record(task, {
	id,
	table_id,
	time,
	creator_id,
	name,
	description,
	start_date,
	end_date,
	start_time,
	end_time,
	period
}).

-record(comment, {
	table_id,
	task_id,
	time,
	creator_id,
	text
}).

-record(permission, {
	table_id,
	creator_id,
	user_id,
	permission
}).