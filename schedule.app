%%
%%  Copyright (C) 2013-2014 Elizaveta Lukicheva.
%%
%%  This file is part of Schedule Server.
%%
%%  Schedule Server is free software: you can redistribute it and/or modify
%%  it under the terms of the GNU General Public License as published 
%%% by the Free Software Foundation, either version 3 of the License, or
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


%%% -----------------------------------------------------
%%% This is the Schedule Server application .app file.
%%% -----------------------------------------------------

{application, schedule,
  [
    {
      description,
      "Schedule Server"
    },
    {
      registered,
      [
        schedule,
        main_sup,
        acceptor,
        io_sup,
        io_worker
      ]
    },
    {
      modules, 
      [
        schedule,
        main_sup,
        acceptor,
        io_sup
      ]
    },
    {
      vsn, "0.1"
    },
    { 
      mod, 
      {schedule, {2, 3600}} % Maximum restart frequency (for internal supervisor).
    },
    { 
      env, 
      [
        {logfile, { open, "./log.txt"}},
        {loglevel, 1},
        {tcp_port, 4567},    % which port socket must bind.
        {max_token, 999999 },   % maximum token to generate.
        {sleep_time, 200},
        {timeout, 2500},    % tcp session timeout
        {is_tty, true}    % tty logging switcher
      ]
    },
    {
      applications,
      [
        kernel,
        stdlib,
        sasl
      ]
    }
  ]
}.