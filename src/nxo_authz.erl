-module(nxo_authz).
-include("nxo.hrl").

-export([
          may/1
        , is/1
        , is_pending/1
        ]).

%% @doc Return true if the user is explicitly in this group.  We'll
%% allow some symbolic names here "superuser" for "administrator" for
%% instance.
-spec is(Group :: atom) -> true | false.
is(superuser) -> is(?ROLE_SUPERUSER);
is(Group) -> wf:role(Group).


%% @doc Return true if the user in context has this authz.
-spec may(Permission :: atom) -> true | false.

may(admin_something) ->
  any([?ROLE_SUPERUSER, ?ROLE_USERMGMT, ?ROLE_DATAMGMT]);

may(admin_everything) -> wf:role(?ROLE_SUPERUSER);

may(admin_users) ->
  any([?ROLE_SUPERUSER, ?ROLE_USERMGMT]);

may(admin_data) ->
  any([?ROLE_SUPERUSER, ?ROLE_DATAMGMT]);

may(view_hipaa) ->
  any([?ROLE_SUPERUSER, ?ROLE_DATAMGMT, ?ROLE_HIPAA]);

may(view_data) ->
  any([?ROLE_SUPERUSER, ?ROLE_DATAMGMT, ?ROLE_HIPAA, ?ROLE_USER]);

may(login) ->
  any([?ROLE_SUPERUSER, ?ROLE_USERMGMT, ?ROLE_DATAMGMT,
       ?ROLE_HIPAA, ?ROLE_USER]);

may(api) ->
  any([?ROLE_SUPERUSER, ?ROLE_API]).

any(Roles) ->
  lists:any(fun(Role) -> wf:role(Role) end, Roles).

is_pending(ID) ->
  case nxo_db:scalar_query(user_is_pending, [ID]) of
    0 -> false;
    _ -> true
  end.
       
