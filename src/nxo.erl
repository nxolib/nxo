-module(nxo).
-include("nxolib.hrl").
-export([
          application/0
        , application/1
        , template/1
        , is_development/0
        , version/0
        , version/1
        , keys_to_binary/1
        , is_string/1
        , to_list/1
        , to_binary/1
        , to_atom/1
        , to_existing_atom/1
        , to_integer/1
        , to_float/1
        , fa/1
        , pickle/1
        , depickle/1
        , url_path/1
        , coalesce/1
        , uuid/0
        , is_uuid/1
        , is_real_list/1
        ]).

-spec template(file:name_all()) -> file:filename_all().
template(File) ->
  nxo_template_name_cache:lookup(File).

%% Retrieve the name of the NXO enabled application.
-spec application() -> atom().
application() ->
  {ok, App} = application:get_env(nxo, application),
  App.

%% Set the name fo the NXO enabled application.
%%
%% This can be set in the config file:
%%   {nxo, [{application, myapp}]}
-spec application(atom()) -> atom().
application(App) ->
  ok = application:set_env(nxo, application, App),
  application().

%% Returns true if development_mode is set in the app config.
%%
%%  {myapp, [{development_mode, true}]}
-spec is_development() -> boolean().
is_development() ->
  application:get_env(application(), development_mode, false).

%% Returns the version string of the configured app.
-spec version() -> term().
version() -> version(application()).

-spec version(atom()) -> term().
version(App) ->
  {_, _, V} = lists:keyfind(App, 1, application:loaded_applications()),
  V.

%% Casts map keys to binary.
-spec keys_to_binary(map()) -> map().
keys_to_binary(Map) when is_map(Map) ->
  Fun = fun(K, V, NewMap) -> maps:put(to_binary(K), V, NewMap) end,
  maps:fold(Fun, #{}, Map).

%% Returns true if the Term is a string.
is_string(Term) when ?IS_STRING(Term) -> true;
is_string(_) -> false.

%% @doc Returns the Font Awesome HTML for the specified glyph.  The
%% glyph may be specified as either a string or atom and should not
%% include the 'fa-' prefix.
%% @param Icon the Font Awesome glyph to render.
-spec fa(Icon :: string() | atom()) -> string().
fa(Icon) when is_atom(Icon) -> fa(atom_to_list(Icon));
fa(Icon)                    ->  "<i class=\"fa fa-fw fa-" ++ Icon ++ "\"></i>".

%% @doc Encode a term suitable for DB storage.
-spec pickle(term()) -> base64:ascii_binary().
pickle(Term) ->
  base64:encode(term_to_binary(Term)).

%% @doc Decode a pickled payload and return a term.
-spec depickle(base64:ascii_binary()) -> term().
depickle(PickledTerm) ->
  binary_to_term(base64:decode(PickledTerm)).

%% @doc Create a URL path from the list of terms.  We'll add the
%% leading slash and de-duplicate extra slashes as well.
-spec url_path([term()]) -> string().
url_path(Parts) ->
  String = string:join(["/" | [ wf:to_list(P) || P <- Parts ]], "/"),
  re:replace(String, "//+", "/", [global, {return,list}]).

%% @doc Return the first value that is not 'undefined', 'null', <<>>,
%% or [].
-spec coalesce([term()]) -> term().
coalesce(List) ->
  hd(lists:dropwhile(fun(undefined) -> true;
                        (null)      -> true;
                        (<<>>)      -> true;
                        ([])        -> true;
                        (_)         -> false
                     end, List)).

%% @doc Return a UUIDv4 as a string.
uuid() ->
  uuid:to_string(uuid:uuid4()).

%% @doc Return true if a valid uuid, false otherwise.
is_uuid(UUID) ->
  try uuid:is_valid(to_list(UUID)) of
      true -> true;
      false -> false
  catch
    _:_ ->
       false
  end.

%% @doc Return true if the value is a list (but not a string).
%% There's one real failing here: lists of integers ([1,2,3]) cannot
%% be differentiated from strings (ha!) so user beware.
is_real_list(V) when is_list(V) ->
  not is_string(V);
is_real_list(_) ->
  false.

%%%%%%%%%%%%%%%%%
%% CONVERSIONS %%
%%%%%%%%%%%%%%%%%

to_list(X) -> nxo_convert:to_list(X).

to_atom(X) -> nxo_convert:to_atom(X).

to_existing_atom(X) -> nxo_convert:to_existing_atom(X).

to_binary(X) -> nxo_convert:to_binary(X).

to_integer(X) -> nxo_convert:to_integer(X).

to_float(X) -> nxo_convert:to_float(X).
