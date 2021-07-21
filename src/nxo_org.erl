-module(nxo_org).
-include("nxo.hrl").

-export([
          find/1
        , all/0
        , delete/1
        , upsert/3
        , format/1
        ]).


%% @doc Return organization details.
-spec find(string() | binary()) -> [#{binary() := binary() | atom()}].
find(ID) ->
  nxo_db:map_query(org_find, [ID]).

%% @doc Return brief info about all organizations.
-spec all() -> [#{binary() := binary() | atom()}].
all() ->
  nxo_db:q(all_orgs).

%% @doc Delete an organization.  Will return {ok, 1} if an
%% organization was deleted and {ok, 0} otherwise.
-spec delete(ID :: string() | binary()) -> ok.
delete(ID) ->
  case find(ID) of
    [] -> ok;
    [Org] ->
      nxo_db:query(org_delete, [ID]),
      logger:notice(#{ realm => organization,
                       action => delete,
                       target => ID,
                       actor  => nxo:user(),
                       details => Org })
  end.

%% @doc Add/Edit an organization.
%% See pages/org_form for how OrgDetails, OrgContact are built.
-spec upsert(binary() | string(), [term()], [term()]) -> binary().
upsert(ID, OrgDetails, OrgContact) ->
  OldOrgName = org_name(ID),
  Res = nxo_db:cascading_update([ {org_add, [ID | OrgDetails]},
                                  {org_contact_add, OrgContact} ]),
  nxo:notify({org, rename, Res, OldOrgName, org_name(Res)}),
  logger:notice(#{ realm => organization,
                   action => upsert,
                   target => Res,
                   actor => nxo:user() }),
  Res.

format(ID) ->
  format_org(find(ID)).

format_org([Org]) when is_map(Org) ->
  #template{ file=nxo:template("format_org.html") };

format_org(_) ->
  [].

org_name(ID) ->
  nxo_obj:detail(fun nxo_org:find/1, ID, org_name).
