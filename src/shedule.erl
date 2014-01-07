%%
%% Copyright (C) 2013-2014 Elizaveta Lukicheva.
%%
%% This file is part of Shedule Server.
%%
%% Shedule Server is free software: you can redistribute it and/or modify
%% it under the terms of the GNU General Public License as published by
%% the Free Software Foundation, either version 3 of the License, or
%% (at your option) any later version.
%%
%% Shedule Server is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%% GNU General Public License for more details.
%%
%% You should have received a copy of the GNU General Public License
%% along with Shedule Server.  If not, see <http://www.gnu.org/licenses/>.

%% Author: Elizaveta Lukicheva <mailto: liza.lukicheva@gmail.com>
%% License: <http://www.gnu.org/licenses/gpl.html>

%% @author Elizaveta Lukicheva <liza.lukicheva@gmail.com>
%% @copyright 2013-2014 Elizaveta Lukicheva
%% @doc This is the main Shedule Server module describes callbacks for
%% application starting and terminating.

-module(shedule).
-behaviour(application).

%% Debug:
-import(jdb, [report/3, appenv/3, ret/1]).

%% Callbacks:
-export([start/2, stop/1]).

%%% @spec start(Type, Timing) -> Result
%%%     Type = term()
%%%     Timing = { MaxR :: integer(), MaxT :: integer() } 
%%%     Result = startlink_ret()
%%%
%%% @doc Starts the Shedule Server application with the supervision timings provided
%%%
start( _, Timing) ->
  jdb:configure(),
  case main_sup:start_link(Timing) of
  ignore ->
    report(0, "Unable to load Shedule Server application", ignore),
    { error, ignore };
  Error = {error, _} ->
    report(0, "Unable to load Shedule Server application", Error);
  Ret -> 
    Ret
  end.
 
%%% @spec stop(Reason) -> Result
%%%     Reason = term()
%%%     Result = ok | {error, Reason}
%%%     Reason = term()
%%%
%%% @doc Stops the Shedule Server application.
%%%
stop( _ ) ->
  error_logger:logfile(close),
  ok.