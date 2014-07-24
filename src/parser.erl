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

-module(parser).

-export([parse/2]).

parse(Mask, Data) ->
	[H|T] = Mask,
	{Element, Left} = get_element(E, Data),
	[Element | parse(T, Left)];

parse([_], []) ->
	no_data;

parse([], [_]) ->
	no_mask;

parse([], []) ->
	[].

get_element(Desc, Data) when Desc =:= $B ->
	<<Element:8, Left/binary>>,
	{Element, Left}.

get_element(Desc, Data) when Desc =:= $W ->
	<<Element:16, Left/binary>>,
	{Element, Left}.

get_element(Desc, Data) when Desc =:= $I ->
	<<Element:32, Left/binary>>,
	{Element, Left}.

get_element(Desc, Data) when Desc =:= $L ->
	<<Element:64, Left/binary>>,
	{Element, Left}.

get_element(Desc, Data) when Desc =:= $S ->
	{Size, Left} = get_element($I, Data),
	<<Element:Size/bitstring, Left1>> = Left,
	{binary_to_list(Element), Left1}.