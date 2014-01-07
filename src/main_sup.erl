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
%% @doc This is the main supervisor of Schedule Server server. It looks 
%% after all the major parts of the server:
%%   acceptor
%%   io_sup
%%
%% For further information about this modules see their 
%% documentation.
%%
%% This modue describes callbacks for supervisor starting  
%% and terminating.

-module(main_sup).
-behaviour(supervisor).

%% Debug:
-import(jdb, [report/3, report/2, appenv/3, ret/1]).

%% Callback and start function 
-export([start_link/1, init/1]).

%%% @spec start_link(Timing) -> Result
%%%    Timing = { MaxR, MaxT } 
%%%    Result = startlink_ret()
%%%
%%% @doc Initializes Schedule Server major modules with the supervision timings provided
%%%
start_link({MaxR, MaxT}) ->
  supervisor:start_link(
    {local, ?MODULE},
    ?MODULE,
    {MaxR, MaxT}
  ).

%%% @private
%%% @doc Starts acceptor and superviser for io processes
init({MaxR, MaxT}) ->
  report(1, "Schedule Server main supervisor initializing"),
  {ok,
    {
      {one_for_one, MaxR, MaxT},
      [%<Internal name> <Module name> <Start func> <Arguments> <Restart type> <Exit timeout> <Process type> <Depends>
        {acceptor, {acceptor, start_link, [null]}, transient, 1000, worker, [acceptor]},
        {io_sup, {io_sup, start_link, [{MaxR, MaxT, 100}]}, transient, infinity, supervisor, [io_sup]} 
      ]
    }
  }.