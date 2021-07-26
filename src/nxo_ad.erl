-module(nxo_ad).
-include("nxo.hrl").

%% API exports
-export([
          authenticate/2
        , user_data/1
        , search_for_user/1
        ]).

-spec authenticate(string(), string()) -> boolean().
authenticate(User, Pass) ->
  case open_handle() of
    false -> false;
    _  -> is_user_authenticated(user_dn(User), Pass)
  end.

-spec user_data(string()) -> map().
user_data(User) ->
  get_user_data(User).

-spec search_for_user(string()) -> list().
search_for_user(LastName) ->
  Handle = open_search_handle(),
  SubstrFilter = 
    eldap:'or'([eldap:substrings("sn",             [{initial, LastName}]),
                eldap:substrings("mail",           [{initial, LastName}]),
                eldap:substrings("samaccountname", [{initial, LastName}])]),
  Filter = {filter, eldap:'and'([SubstrFilter,
                                 eldap:present("mail"),
                                 eldap:present("samaccountname")])},
  Attr = {attributes, ["displayname", "distinguishedName", "samaccountname",
                       "mail"]},
  {ok, {eldap_search_result, Entries, _}} =
    eldap:search(Handle, [base(), Filter, Attr]),
  entries_to_plists(Entries).

%% CONFIGURATION

ad_host() -> %% hosts may be singletons or a comma separated list
  [ wf:to_list(string:trim(H))
    || H <- string:split(nxo_settings:get(ad, ad_host), ",")].
ad_port() -> wf:to_integer(nxo_settings:get(ad, ad_port)).
ad_base() -> wf:to_list(nxo_settings:get(ad, ad_base)).
ad_bind() -> wf:to_list(nxo_settings:get(ad, ad_bind)).
ad_pass() -> read_pass_file(nxo_settings:get(ad, ad_pass_file)).

read_pass_file(Filename) ->
  {ok, Password} = file:read_file(Filename),
  binary_to_list(Password).

base() -> {base, ad_base()}.
port() -> {port, ad_port()}.

%% INTERNAL FUNCTIONS

%% LDAP results are encapsulated in an eldap_entry record.  Remove the
%% data part of that (it's a plist) and then for each plist value,
%% replace the original list with just the first value of the list.
%%
%% (We're using these data for an autocomplete dropdown, this
%% roughshod method of using just the first value should work fine.)
entries_to_plists(Entries) ->
  Convert = fun({K, V}) ->
                {list_to_binary(string:to_lower(K)), list_to_binary(hd(V))} end,
  ConvertPairs = fun(Entry) -> [ Convert(Pair) || Pair <- Entry ] end,
  ExtractEntries = fun() -> [ Entry || {eldap_entry, _, Entry} <- Entries ] end,
  lists:map(ConvertPairs, ExtractEntries()).

get_user(User) ->
  Handle = open_search_handle(),
  {ok, {eldap_search_result, Entries, _}} =
    eldap:search(Handle, [base(), username_or_email_filter(User)]),
  Entries.

get_user_data(User) ->
  case get_user(User) of
    [{eldap_entry, _, Data}] -> user_data_to_map(Data);
    _                        -> not_found
  end.

user_data_to_map(Data) ->
  Unspecified = [""],
  DataMap = maps:from_list(Data),
  #{
     first_name       => hd(maps:get("givenName", DataMap, Unspecified)),
     last_name        => hd(maps:get("sn", DataMap, Unspecified)),
     display_name     => hd(maps:get("displayName", DataMap, Unspecified)),
     phone            => hd(maps:get("telephoneNumber", DataMap, Unspecified)),
     description      => hd(maps:get("description", DataMap, Unspecified)),
     sam_account_name =>
       string:to_lower(hd(maps:get("sAMAccountName", DataMap))),
     email            =>
       string:to_lower(hd(maps:get("mail", DataMap)))
   }.

is_user_authenticated(not_found, _) ->
  false;
is_user_authenticated({ok, DN}, Pass) ->
  bind_handle(open_handle(), DN, Pass).

user_dn(User) ->
  case get_user(User) of
    [{eldap_entry, DN, _}] -> {ok, DN};
    _                      -> not_found
  end.

open_search_handle() ->
  Handle = open_handle(),
  true = bind_handle(Handle),
  Handle.

open_handle() ->
  case eldap:open(ad_host(), [port()]) of
    {ok, Handle} -> Handle;
    {error, _}   ->  false
  end.

bind_handle(Handle) ->
  bind_handle(Handle, ad_bind(), ad_pass()).

bind_handle(Handle, User, Pass) ->
  case eldap:simple_bind(Handle, User, Pass) of
    ok         -> true;
    {error, _} -> false
  end.

username_or_email_filter(Username) ->
  {filter, eldap:'or'([eldap:equalityMatch("sAMAccountName", Username),
                       eldap:equalityMatch("mail", Username)])}.
