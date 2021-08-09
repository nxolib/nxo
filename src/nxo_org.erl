-module(nxo_org).
-include("nxo.hrl").

-export([
          find/1
        , all/0
        , delete/1
        , upsert/2
        ]).

%% @doc Return organization details.
-spec find(string() | binary()) -> [#{binary() := binary() | atom()}].
find(OrgAbbrv) ->
  nxo_db:map_query(org_find, [OrgAbbrv]).

%% @doc Return brief info about all organizations.
-spec all() -> [#{binary() := binary() | atom()}].
all() ->
  nxo_db:q(all_orgs).

%% @doc Delete an organization.  Will return {ok, 1} if an
%% organization was deleted and {ok, 0} otherwise.
-spec delete(ID :: string() | binary()) -> ok.
delete(OrgAbbrv) ->
  nxo_db:query(org_delete, [OrgAbbrv]).

%% @doc Add/Edit an organization.
%% See pages/org_form for how OrgDetails, OrgContact are built.
upsert(OrgDetails, OrgContact) ->
  nxo_db:cascading_update([ {org_add, OrgDetails},
                            {org_contact_add, OrgContact} ]).
