%%
%%  Copyright (C) 2013-2014 Roman Nazarenko.
%%
%%  This file is part of Arya.
%%
%%  Arya is free software: you can redistribute it and/or modify
%%  it under the terms of the GNU General Public License as published by
%%  the Free Software Foundation, either version 3 of the License, or
%%  (at your option) any later version.
%%
%%  Arya is distributed in the hope that it will be useful,
%%  but WITHOUT ANY WARRANTY; without even the implied warranty of
%%  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%  GNU General Public License for more details.
%%
%%  You should have received a copy of the GNU General Public License
%%  along with Arya.  If not, see <http://www.gnu.org/licenses/>.

%%  Author: Roman Nazarenko <mailto: me@jtalk.me>
%%  License: <http://www.gnu.org/licenses/gpl.html>

%% @author Roman Nazarenko <me@jtalk.me>
%% @copyright 2012-2013 Roman Nazarenko
%% @doc JDB is a debug module with debug leveling support. It uses 
%% standard Erlang error_logger module, supports both tty and file output.

-module(jdb).

-export([appenv/3, getenv/2, report/2, report/3, configure/0, unixtimestamp/0]).

%%% @spec appenv(Name, Function, ErrorMessage) -> term() | undefined
%%%     Name = atom()
%%%     Function = function()
%%%     ErrorMessage = string()
%%%
%%% @doc Applies Function to an application environmental variable Name
%%% If variable lookup fails, reports ErrorMessage to the report handlers
%%% configured.
%%%
appenv(Name, Function, ErrorMessage) ->
  case application:get_env(Name) of
    {ok, Value} ->
      Function(Value);
    Error ->
      report(0, ErrorMessage, Error),
      Error
  end.
  
%%% @spec getenv(Name, ErrorMessage) -> term() | undefined
%%%     Name = atom()
%%%     ErrorMessage = string()
%%%  
%%% @doc Returns environmental variable Name, if lookup failed, reports
%%% ErrorMessage to report handlers configured.
%%%
getenv(Name, ErrorMessage) ->
  appenv(Name, fun(Value) -> Value end, ErrorMessage).

%%% @spec report(Level, Report) -> ok | false
%%%     Level = integer() 
%%%     Report = string()
%%%  
%%% @doc Reports to standard report handler, if Level matches loglevel 
%%% configured. Returns false if doesn't.
%%%
report(Level, Report) ->
  {ok,Default} = application:get_env(loglevel),
  if 
    Level == 0 ->
      error_logger:format(Report, self());
    Level =< Default ->
      error_logger:info_msg(Report, self());
    true ->
      false
  end.
  
%%% @spec report(Level, Report, Data) -> ok | false
%%%     Level = integer() 
%%%     Report = string()
%%%     Data = term()
%%%  
%%% @doc Reports to standard report handler, appending any Erlang data provided,
%%% if Level matches loglevel configured. Returns Data.
%%%
report(Level, Report, Data) ->
  {ok,Default} = application:get_env(loglevel),
  if 
    Level == 0 ->
      error_logger:format(Report, {self(), Data}),
      Data;
    Level =< Default ->
      error_logger:info_msg(Report, {self(), Data}),
      Data;
    true ->
      Data
  end.
    
%%% @doc Configures Erlang error logger in accordiance with .app file configuration.
%%%
configure() ->
  appenv(logfile, fun error_logger:logfile/1, "Unable to create log file"),
  appenv(is_tty, fun error_logger:tty/1, "Unable to read tty configuration").

%%% @doc Converts Erlang now() result to unixtimestamp
unixtimestamp() ->
  element(1, now()) * 1000000 + element(2, now()).