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

-import(jdb, [report/3, report/2, appenv/3, getenv/2]).

%%% Message sent between processes, it contains source address 
%%% and port and the raw data of the incoming message.
-record(recv, 
    {
      from :: {inet:ip_address(), inet:port_number()},
      data :: binary()
    }
  ).

%%% The TCP message received via an active gen_tcp socket.
-record(tcp, 
    {
      socket,  % = socket()
      data  % = binary()
    }
  ).