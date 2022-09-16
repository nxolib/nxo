-module(nxo_directory).
-include("nxo.hrl").
-include_lib("eldap/include/eldap.hrl").

-export([
         %% functions to manage directory definitions
          delete/2
        , find/1
        , find/2
        , any_defined/0
        , directory_organizations/0
         %% functions to use the directory
        , search/3
        , authenticate/3
        , open/1
        ]).

delete(OrgAbbrv, Directory) ->
  nxo_db:q(delete_directory, [OrgAbbrv, Directory]).

find(OrgAbbrv, Directory) ->
  nxo_db:q(find_directory, [OrgAbbrv, Directory]).

find(OrgAbbrv) ->
  nxo_db:q(directories_for_org, [OrgAbbrv]).

any_defined() ->
  nxo_db:q(directories_defined).

%% The organizations that have directories assigned.
directory_organizations() ->
  nxo_db:q(directory_organizations).

search(account, Ident, OrgAbbrv) ->
  lists:flatten(search_all_directories(account, Ident, OrgAbbrv));

search(entry, Mail, OrgAbbrv) ->
  case lists:flatten(search_all_directories(entry, Mail, OrgAbbrv)) of
    [] -> not_found;
    Res -> {entry, Res}
  end.

authenticate(Mail, Password, OrgAbbrv) ->
  case search(entry, Mail, OrgAbbrv) of
    not_found ->
      false;
    {entry, Entries} ->
      case lists:search(fun (E) -> authenticate_entry(E, Password) end,
                        Entries) of
        false -> false;
        {value, _} -> true
      end
  end.

authenticate_entry(Entry, Password) ->
  {OrgAbbrv, Directory} = maps:get(directory, Entry),
  DN = maps:get(dn, Entry),
  case find(OrgAbbrv, Directory) of
    [] -> false;
    [DirMap] ->
      case open(DirMap) of
        {false, _} -> false;
         Handle -> bind(Handle, DN, Password)
      end
  end.

search_all_directories(Type, Ident, OrgAbbrv) ->
  [ directory_search(Type, Ident, DirMap) || DirMap <- find(OrgAbbrv) ].

directory_search(account, Ident, DirMap) ->
  case open(DirMap) of
    {false, _} ->
      [];
    Handle ->
      Filter = filter(account, Ident, DirMap),
      Attrs = attributes(account, DirMap),
      {ok, Res} =
        eldap:search(Handle, [{base, base(DirMap)}, Filter, Attrs]),
      Entries = Res#eldap_search_results.entries,
      result_to_map(account, Entries, DirMap)
  end;

directory_search(entry, Email, DirMap) ->
  case open(DirMap) of
    {false, _} ->
      [];
    Handle ->
      Filter = filter(entry, Email, DirMap),
      Attrs = attributes(account, DirMap),
      {ok, Res} =
        eldap:search(Handle, [{base, base(DirMap)}, Filter, Attrs]),
      Entries = Res#eldap_search_results.entries
      result_to_map(account, Entries, DirMap)
  end.

result_to_map(account, Result, DirMap) ->
  Directory = directory(DirMap),
  [UIDAttr, FNameAttr, LNameAttr, MailAttr] =
    [ attr(A, DirMap) || A <- [uid, fname, lname, mail] ],
  [ #{ uid =>   wf:to_binary(hd(proplists:get_value(UIDAttr, R,   [[]]))),
       mail =>  wf:to_binary(hd(proplists:get_value(MailAttr, R,  [[]]))),
       fname => wf:to_binary(hd(proplists:get_value(FNameAttr, R, [[]]))),
       lname => wf:to_binary(hd(proplists:get_value(LNameAttr, R, [[]]))),
       dn => wf:to_binary(DN),
       directory => Directory } || {eldap_entry, DN, R} <- Result ].

filter(entry, Ident, DirMap) ->
  {filter, eldap:equalityMatch(attr(mail, DirMap), Ident)};

filter(account, Ident, DirMap) ->
  F1 = eldap:'or'([eldap:substrings(attr(lname, DirMap), [{initial, Ident}]),
                   eldap:substrings(attr(mail, DirMap), [{initial, Ident}]),
                   eldap:substrings(attr(uid, DirMap), [{initial, Ident}])]),
  {filter, eldap:'and'([F1,
                        eldap:present(attr(mail, DirMap)),
                        eldap:present(attr(uid, DirMap))])}.

attributes(account, DirMap) ->
  {attributes, [ attr(A, DirMap) || A <- [uid, mail, fname, lname] ]}.

open(DirMap) ->
  case eldap:open([host(DirMap)], [{port, port(DirMap)}]) of
    {ok, Handle} ->
      maybe_start_tls(Handle, tls_versions(DirMap)),
      case bind(Handle, bind(DirMap), bind_pass(DirMap)) of
        true -> Handle;
        false -> {false, failed_to_bind}
      end;
    {error, Err} ->
      {false, Err}
  end.

maybe_start_tls(Handle, []) ->
  Handle;
maybe_start_tls(Handle, TLSVersions) ->
  case eldap:start_tls(Handle, [{versions, TLSVersions}]) of
    ok ->
      Handle;
    tls_already_started ->
      Handle;
    {_, Error} ->
      eldap:close(Handle),
      error(cannot_start_ldap_tls, [Error])
  end.

bind(_Handle, [], []) ->
  no_userpass_supplied;
bind(Handle, User, Pass) ->
  eldap:simple_bind(Handle, User, Pass) == ok.

%% Accessors
host(DirMap) ->
  wf:to_list(maps:get(<<"host">>, DirMap)).

port(DirMap) ->
  wf:to_integer(maps:get(<<"port">>, DirMap)).

bind(DirMap) ->
  wf:to_list(maps:get(<<"bind">>, DirMap)).

bind_pass(DirMap) ->
  wf:to_list(nxo:decrypt_binary(maps:get(<<"bind_pass">>, DirMap))).

tls_versions(DirMap) ->
  [ wf:to_atom(V) || V <- maps:get(<<"tls_versions">>, DirMap, []) ].

base(DirMap) ->
  wf:to_list(maps:get(<<"base">>, DirMap)).

attr(uid, DirMap) ->
  wf:to_list(maps:get(<<"attr_uid">>, DirMap));
attr(mail, DirMap) ->
  wf:to_list(maps:get(<<"attr_mail">>, DirMap));
attr(fname, DirMap) ->
  wf:to_list(maps:get(<<"attr_fname">>, DirMap));
attr(lname, DirMap) ->
  wf:to_list(maps:get(<<"attr_lname">>, DirMap)).

directory(DirMap) ->
  {maps:get(<<"org_abbrv">>, DirMap), maps:get(<<"directory">>, DirMap)}.
