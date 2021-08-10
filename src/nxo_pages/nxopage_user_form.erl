-module(nxopage_user_form).
-include("nxo.hrl").

-export([ main/0
        , title/0
        , body/0
        , event/1
        , button/1
        , dropdown/1
        , org_form/2
        ]).

-security({groups, [administrators, usermgmt]}).
-postback_security({groups, [administrators, usermgmt]}).

-define(SELECTED_ORGS_DEFAULT,
        wf:state_default(selected_orgs, [])).

main() ->
  #template{ file=nxo:template("user_form.html") }.

title() ->
  "User Management".

body() ->
  User = case wf:path_info() of
           [] -> #{new_user => true};
           UserID -> hd(nxo_user:find(UserID))
         end,
  Data = maps:merge(User, #{ all_groups => nxo_group:all() }),
  #template{ text=nxo_template:pretty_render(user_form, Data) }.

event({org_form_submit, UserID}) ->
  submission(UserID);
event({new_org_form, UserID}) ->
  case wf:q(new_org) of
    [] ->
      ok;
    OrgAbbrv ->
      Form = org_form(OrgAbbrv, UserID),
      wf:replace(new_org_dropdown, dropdown({additional_orgs, UserID})),
      wf:insert_bottom(org_forms, Form)
  end.


button({submit, UserID}) ->
  {Text, ID} = case UserID of
                 [] -> {"Add User", nxo:uuid()};
                 _  -> {"Update User", UserID}
         end,
  #button{ id=org_form_submit,
           postback={org_form_submit, ID},
           delegate=?MODULE,
           class="btn btn-primary",
           text=Text }.

dropdown({additional_orgs, UserID}) ->
  ExistingOrgs = case nxo_user:find(UserID) of
                   [] -> [<<"global">>];
                   Users -> maps:get(<<"orgs">>, hd(Users))
                 end,
  SkipOrgs = ExistingOrgs ++ ?SELECTED_ORGS_DEFAULT,
  Options = lists:filtermap(fun(Group) ->
                                Abbrv = maps:get(<<"org_abbrv">>, Group),
                                case not lists:member(Abbrv, SkipOrgs) of
                                  true ->
                                    Text = maps:get(<<"org_name">>, Group),
                                    {true, #option{ text=Text, value=Abbrv}};
                                  false ->
                                    false
                                end
                            end, nxo_org:all()),
  case Options of
    [] ->
      wf:remove(new_org);
    _ ->
      #dropdown{ id=new_org_dropdown,
                 class="custom-select",
                 postback={new_org_form, UserID},
                 options=[ #option{} | lists:reverse(Options) ]}
  end.


submission(UserID) ->
  %% UserID might be from the DB or it might have been generated.
  %% XXX: This really requires further refinement.  Two things are weird:
  %%
  %%  1. I think user_add and user_set_roles should probably be a
  %%     transaction, and
  %%
  %%  2. We set the GlobalRoles twice.  This might not be all bad (I
  %%     suspect user_add will be used elsewhere).
  case nxo_validate:validate(user_form, #{user_id => UserID}) of
    true ->
      SimpleParams = nxo_datamap:apply(user_add),
      GlobalRoles = [ wf:to_binary(R) || R <- wf:qs(global_roles) ],
      Params = [UserID | SimpleParams] ++ [GlobalRoles],
      nxo_db:q(user_add, Params),
      OrgRoles =
        lists:map(fun(Abbrv) ->
                      Title = wf:q(wf:to_atom(Abbrv ++ "_title")),
                      Contact = wf:q(wf:to_atom(Abbrv ++ "_is_contact")),
                      Roles = [ wf:to_binary(R)
                                || R <- wf:qs(wf:to_atom(Abbrv ++ "_role")) ],
                      nxo_db:q(user_add_org, [UserID, Abbrv,
                                              wf:to_atom(Contact), Title]),
                      Roles
                  end, wf:qs(selected_new_org)),
      AllRoles = lists:flatten([GlobalRoles, OrgRoles]),
      nxo_db:q(user_set_roles, [UserID, AllRoles]),
      wf:redirect("/users");
    false ->
      nxo_view:report_validation_failure()
  end.

org_form("global", _) ->
  [];
org_form(OrgAbbrv, UserID) ->
  %% This is the "sub-form" that appears when the useradmin select a
  %% new organization for the user to subscribe to.
  wf:state(selected_orgs, [wf:to_binary(OrgAbbrv) | ?SELECTED_ORGS_DEFAULT]),
  Org = hd(nxo_org:find(OrgAbbrv)),
  User = case nxo_user:find(UserID) of
           [] -> #{new_user => true};
           Users -> hd(Users)
         end,
  UserOrg = case nxo_db:q(user_org, [UserID, OrgAbbrv]) of
              [] -> #{};
              UserOrgs -> hd(UserOrgs)
            end,
  Groups = nxo_group:all_with_role(OrgAbbrv),
  Data = maps:merge(User, #{ org => Org,
                             user_org =>  UserOrg,
                             non_global_groups => Groups }),
  #template{ text=nxo_template:pretty_render(user_org_form, Data) }.
