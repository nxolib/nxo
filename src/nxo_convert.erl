-module(nxo_convert).
-include("nxolib.hrl").
-export([
          to_list/1
        , to_atom/1
        , to_existing_atom/1
        , to_binary/1
        , to_integer/1
        , to_float/1
        ]).

%% This code is borrowed from the Nitrogen Project:
%%     https://github.com/nitrogen/nitrogen
%% Please see the License at the bottom of this file for details.

-spec to_list(term()) -> list().
to_list(undefined) -> [];
to_list(L) when ?IS_STRING(L) -> L;
to_list(L) when is_list(L) ->
    SubLists = [inner_to_list(X) || X <- L],
    lists:flatten(SubLists);
to_list(A) -> inner_to_list(A).

-spec to_atom(term()) -> atom().
to_atom(A) when is_atom(A) -> A;
to_atom(B) when is_binary(B) -> to_atom(binary_to_list(B));
to_atom(I) when is_integer(I) -> to_atom(integer_to_list(I));
to_atom(F) when is_float(F) -> to_atom(nxo_mochinum:digits(F));
to_atom(L) when is_list(L) ->
  list_to_atom(binary_to_list(iolist_to_binary(L))).

-spec to_existing_atom(term()) -> atom().
to_existing_atom(A) when is_atom(A) -> A;
to_existing_atom(B) when is_binary(B) -> to_existing_atom(binary_to_list(B));
to_existing_atom(I) when is_integer(I) -> to_existing_atom(integer_to_list(I));
to_existing_atom(L) when is_list(L) ->
  list_to_existing_atom(binary_to_list(iolist_to_binary(L))).

-spec to_binary(term()) -> binary().
to_binary(A) when is_atom(A) -> to_binary(atom_to_list(A));
to_binary(B) when is_binary(B) -> B;
to_binary(I) when is_integer(I) -> to_binary(integer_to_list(I));
to_binary(F) when is_float(F) -> to_binary(nxo_mochinum:digits(F));
to_binary(L) when is_list(L) -> list_to_binary(to_list(L)).


-spec to_integer(term()) -> integer().
to_integer(A) when is_atom(A) -> to_integer(atom_to_list(A));
to_integer(B) when is_binary(B) -> to_integer(binary_to_list(B));
to_integer(I) when is_integer(I) -> I;
to_integer(L) when is_list(L) -> list_to_integer(L);
to_integer(F) when is_float(F) -> round(F).

-spec to_float(term()) -> float().
to_float(F) when is_float(F) -> F;
to_float(I) when is_integer(I) -> float(I);
to_float(T) when is_list(T);
                 is_binary(T);
                 is_atom(T) -> safe_to_float(wf:to_list(T)).

safe_to_float(L) when is_list(L) ->
    try list_to_float(L)
    catch _:badarg -> float(list_to_integer(L))
    end.


-spec inner_to_list(term()) -> list().
inner_to_list(A) when is_atom(A) -> atom_to_list(A);
inner_to_list(B) when is_binary(B) -> binary_to_list(B);
inner_to_list(I) when is_integer(I) -> integer_to_list(I);
inner_to_list(F) when is_float(F) ->
	case F == round(F) of
		true -> inner_to_list(round(F));
		false -> nxo_mochinum:digits(F)
	end;
inner_to_list(L) when is_list(L) -> L.


%%%%%%%%%%%%%%%%%%%%%%
%% ORIGINAL LICENSE %%
%%%%%%%%%%%%%%%%%%%%%%

%% Copyright (c) 2008-2013 Rusty Klophaus/Jesse Gumm

%% Permission is hereby granted, free of charge, to any person
%% obtaining a copy of this software and associated documentation
%% files (the "Software"), to deal in the Software without
%% restriction, including without limitation the rights to use, copy,
%% modify, merge, publish, distribute, sublicense, and/or sell copies
%% of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:

%% The above copyright notice and this permission notice shall be
%% included in all copies or substantial portions of the Software.

%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
%% MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
%% NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
%% BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
%% ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
%% CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
%% SOFTWARE.
