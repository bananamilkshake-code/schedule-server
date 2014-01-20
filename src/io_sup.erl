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
%% @doc This is the processors' supervisor of Schedule Server server. 
%% It looks after an essential workers processing all
%% the incoming data before it sends to the downloader
%% process (or just dropped, if data is malformed).
%%
%% For further information about processor module see its 
%% documentation.
%%
%% This modue describes callbacks for supervisor starting  
%% and terminating as well as a worker adding method.


-module(io_sup).
-behaviour(supervisor).

%% Debug
-import(jdb, [report/3, report/2, appenv/3, getenv/1]).

%% Making new processor and supervisor starting.
-export([start_link/1]).
-export([start_io/1]).

%% Callbacks:
-export([init/1]).

%%% @spec child() -> {ok, child()} | {error, term()}
%%%    
%%% @doc Starts new Schedule Server processor and returns its PID.
%%%
start_io(Socket) ->
  report(1, "Child creating in IO supervisor"),
  supervisor:start_child(?MODULE, [Socket]).

%%% @spec start_link( Timing) -> Result
%%%    Timing = { MaxR, MaxT, Await} 
%%%     MaxR = integer() 
%%%     MaxT = integer() 
%%%     Await = integer() | infinity
%%%    Result = startlink_ret()
%%%
%%% @doc Initializes Schedule Server processors and handles them.
%%%
start_link({MaxR, MaxT, Await}) ->
  report(1, "Starting IO supervisor"),
  supervisor:start_link(
    {local, ?MODULE},
    ?MODULE,
    {MaxR, MaxT, Await} %% @see supervisor manual.
  ).

%% Callbacks:
%%% @private
%%% @doc Starts simple one-for-one supervisor, producing arya_processor modules.
%%%
init( {MaxR, MaxT, Await} ) ->
  report(1, "IO supervisor starting"),
  {ok, 
    {
      {simple_one_for_one , MaxR, MaxT}, 
      [   
        {   
          io_worker, {io_worker, start_link, []}, temporary, Await, worker, [io] % See supervisor behaviour articles
        }
      ]
    }
  }.
