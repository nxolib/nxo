-module(nxo_page_security).
-include("nxo.hrl").
-export([
         page_security_check/0
        , page_security_check/2
        , postback_security_check/0
        , postback_security_check/2
        , element_security_check/3
        , element_security_check/4
        ]).

-define(CACHE, nxo_page_security_cache).
-define(EXPIRE_TIME, 60 * 60 * 1000).

page_security_check() ->
  case wf_context:page_module() of
    static_file -> {allow, spec, user, msg};
    Module ->
      page_security_check(Module, wf:user())
  end.

postback_security_check() ->
  postback_security_check(wf_context:event_module(), wf:user()).

%% @doc Ensure this user may access this page module.
%%
%% The module should have a -security() attribute; modules not
%% defining this attribute will be denied regardless of user.
%%
%% The values allowed include:
%%
%%    -security(none)
%%        allow all users, authenticated or not
%%
%%    -security(authenticated)
%%        allow all authenticated users
%%
%%    -security({group, some_group})
%%        allow authenticated users in the specified group
%%
%%    -security({groups, [group_1, group_2]})
%%        allow authenticated users is any of the specified groups
%%
%% Modules that define event/1 (for postbacks) also require a
%% -postback_security() attribute with the same values as above.
%%
%% There are occasions when events are posted back to third-party
%% modules.  In these cases we query -origin_security() for
%% authorization.  An example of this case is file upload which has
%% element_upload as its postback delegate.  Note that origin_security
%% will only be queried if there is no postback_security in place.
%%
%% (Although this behavior is defined in the handlers, postbacks will
%% require a user to pass both the page and postback security checks.
%% The implication is that postback security will always be at least
%% as strict -- and in the same ways -- as the page it's defined in.)

page_security_check(Module, User) ->
  security_check(object_spec(Module, security), User).

postback_security_check(Module, User) ->
  case security_check(object_spec(Module, postback_security), User) of
    {deny, undef, _, _} -> origin_security_check(User);
    Result -> Result
  end.

origin_security_check(User) ->
  security_check(object_spec(wf_context:page_module(), origin_security), User).


%% The element_security_check will return AllowElement if the check
%% passes or DenyElement if not.  Scope is one of 'page' or 'postback'
%% and uses the apparatus defined above to perform the checks.
element_security_check(Module, Scope, AllowElement) ->
  element_security_check(Module, Scope, AllowElement, []).

element_security_check(Module, Scope, AllowElement, DenyElement) ->
  {Result, _, _, _} = case Scope of
                        page -> page_security_check(Module, wf:user());
                        postback -> postback_security_check(Module, wf:user())
                      end,
  case Result of
    allow -> AllowElement;
    deny  -> DenyElement
  end.

object_spec(Module, Attribute) ->
  Attrs = Module:module_info(attributes),
  hd(proplists:get_value(Attribute, Attrs, [undef])).

security_check(Spec, User) ->
  case {Spec, User} of
    {undef, _} ->
      {deny, Spec, User, "no specification; always fails"};

    {none, _} ->
      {allow, Spec, User, "always allowed"};

    {authenticated, undefined} ->
      {deny, Spec, User, "not logged in"};

    {authenticated, _} ->
      {allow, Spec, User, "logged in"};

    {{group, _Group}, undefined} ->
      {deny, Spec, User, "not logged in; no groups"};

    {{group, Group}, User} ->
      {check_memberships(User, [Group]), Spec, User, ""};

    {{groups, _Groups}, undefined} ->
      {deny, Spec, User, "not logged in; no groups"};

    {{groups, Groups}, User} ->
      {check_memberships(User, Groups), Spec, User, ""};

    _ ->
      {deny, Spec, User, "fell through to default denial"}
  end.

check_memberships(User, Groups) ->
  case lists:any(fun(G) -> is_member(User, G) end, Groups) of
    true  -> allow;
    false -> deny
  end.

is_member(User, Group) ->
  case nxo_db:scalar_query(role_get_has_role, [User, Group]) of
    N when N > 0 -> true;
    _            -> false
  end.
