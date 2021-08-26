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
        , fa/1
        , pickle/1
        , depickle/1
        , url_path/1
        , coalesce/1
        , uuid/0
        , is_uuid/1
        , is_real_list/1
        , session_timeout/0
        , session_warning/0
        , global_auth_allowed/0
        , global_auth_enabled/0
        , event_handler/0
        , add_handler/1
        , add_handler/2
        , delete_handler/1
        , delete_handler/2
        , notify/1
        , user/0
        , is_authenticated/0
        , consult_file/3
        , random_password/1
        , encrypt_binary/1
        , encrypt_binary/2
        , decrypt_binary/1
        , decrypt_binary/2
        ]).

-define(EVENT, nxo_event_handler).

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
  Fun = fun(K, V, NewMap) -> maps:put(wf:to_binary(K), V, NewMap) end,
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
  try uuid:is_valid(wf:to_list(UUID)) of
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


%% @doc Return the session timeout in MS.
-spec session_timeout() -> integer().
session_timeout() ->
  wf:to_integer(application:get_env(nxo, session_timeout, 20)) * 60 * 1000.

%% @doc Return when the session timeout warning should fire, in MS.
-spec session_warning() -> integer().
session_warning() ->
  session_timeout() - (60 * 1000).

%% @doc Returns true if global_auth_required is true; false otherwise.
%%
%% When global_auth_required is true, all users must authenticate.
%% This is a tool for development or pre-release.
-spec global_auth_enabled() -> true | false.
global_auth_enabled() ->
  application:get_env(application(), global_auth_required, false).


%% @doc Returns true if access to the resource is not hindered by
%% global_auth.  This is primarily for use with events (see nxo_db).
-spec global_auth_allowed() -> true | false.
global_auth_allowed() ->
  case wf:in_request() of
    true ->
      %% check for websocket request
      WS = case wf_context:type() of
             postback_websocket -> true;
             postback_request   -> true;
             _                  -> false
           end,
      case {global_auth_enabled(), wf:user(), wf:page_module(), WS} of
        {_,     _, login, false} -> true;                  % access to login
        {false, _, _    , _    } -> true;                  % no global
        {_,     U, _    , _    } when U =/= undefined -> true; % user logged in
        {_,     _, _    , _    } -> false
      end;
    false ->
      true
  end.


%% @doc Returns the PID of the NXO event handler.
-spec event_handler() -> pid().
event_handler() ->
  nprocreg:get_pid(?EVENT).

%% @doc Send an event notification to the NXO event handler.
-spec notify(Msg :: any()) -> ok.
notify(Msg) ->
  gen_event:notify(event_handler(), Msg).

%% @doc Adds an event handler
add_handler(Module) ->
  add_handler(Module, []).

add_handler(Module, Args) ->
  gen_event:add_handler(event_handler(), Module, Args).

%% @doc Deletes an event handler
delete_handler(Module) ->
  delete_handler(Module, []).

delete_handler(Module, Args) ->
  gen_event:delete_handler(event_handler(), Module, Args).

%% @doc Safely execute wf:user(); return username or 'undefined'.
-spec user() -> any() | undefined.
user() ->
  case wf:in_request() of
    true -> wf:user();
    false -> undefined
  end.

%% @doc Test if user is authenticated.
-spec is_authenticated() -> boolean().
is_authenticated() ->
  not(user() == undefined).

%% @doc Consult a file.  First try the app's priv dir (with the
%% supplied sub directory and extension) then the NXO priv dir.
-spec consult_file(atom() | binary() | string(),
                   atom() | binary() | string(),
                   binary() | string()) ->
        any().
consult_file(File, SubDir, Ext) ->
  Filename = filename:join([wf:to_list(SubDir),
                            (wf:to_list(File) ++ wf:to_list(Ext))]),
  FileApp = filename:join([code:priv_dir(nxo:application()), Filename]),
  case file:consult(FileApp) of
    {ok, Terms} ->
      Terms;
    {error, enoent} ->
      FileNXO = filename:join([code:priv_dir(nxo), Filename]),
      case file:consult(FileNXO) of
        {ok, Terms} ->
          Terms;
        {error, _} ->
          logger:error("could not consult file: ~s", [Filename]),
          error(consult_file_failed)
      end
  end.


%% Generate a random-ish password of length Len.
random_password(Len) ->
    Chrs = list_to_tuple("ABCDEFGHIJKLMNOPQRSTUVWXYZ" ++
                           "abcdefghijklmnopqrstuvwxyz0123456789"),
    ChrsSize = size(Chrs),
    F = fun(_, R) -> [element(rand:uniform(ChrsSize), Chrs) | R] end,
    lists:foldl(F, "", lists:seq(1, Len)).



%%%%%%%%%%%%%%%%%%%%%%%%
%% ENCRYPTION HELPERS %%
%%%%%%%%%%%%%%%%%%%%%%%%
encrypt_binary(Bin) ->
  case application:get_env(nxo, enc_key, undefined) of
    undefined ->
      error("Neither enc_key configuration nor key specified");
    EncKey ->
      encrypt_binary(Bin, EncKey)
  end.

encrypt_binary(Bin, "file://" ++ File) ->
  {ok, KeyString} = file:read_file(File),
  encrypt_binary(Bin, KeyString);
encrypt_binary(Bin, KeyString) ->
  Key = crypto_key(KeyString),
  IV = crypto:strong_rand_bytes(16),
  Crypt = crypto:crypto_one_time(aes_128_ctr, Key, IV, Bin, true),
  base64:encode(<<IV/binary, Crypt/binary>>).

decrypt_binary(Bin) ->
  case application:get_env(nxo, enc_key, undefined) of
    undefined ->
      error("Neither enc_key configuration nor key specified");
    EncKey ->
      decrypt_binary(Bin, EncKey)
  end.

decrypt_binary(Bin, "file://" ++ File) ->
  {ok, KeyString} = file:read_file(File),
  decrypt_binary(Bin, KeyString);
decrypt_binary(Bin, KeyString) ->
  Key = crypto_key(KeyString),
  <<IV:16/bytes, Cyphertext/binary>> = base64:decode(Bin),
  crypto:crypto_one_time(aes_128_ctr, Key, IV, Cyphertext, false).

crypto_key(KeyString) ->
  KeyBinary = wf:to_binary(KeyString),
  Padded = <<KeyBinary/binary, 0:128>>,
  <<Padded:128/bits>>.
