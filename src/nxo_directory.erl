-module(nxo_directory).
-include("nxo.hrl").

-export([
         %% functions to manage directory definitions
          delete/2
        , find/1
        , find/2
        , any_defined/0
        , directory_organizations/0
         %% functions to use the directory
        , search/2
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

search(Term, OrgAbbrv) ->
  DirMaps = find(OrgAbbrv),



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

bind(Handle, [], []) ->
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
  wf:to_list(maps:get(<<"attr_fname">> DirMap));
attr(lname, DirMap) ->
  wf:to_list(maps:get(<<"attr_lname">> DirMap)).
