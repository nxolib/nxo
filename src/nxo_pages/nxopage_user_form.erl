-module(nxopage_user_form).
-include("nxo.hrl").

-export([ main/0
        , title/0
        , body/0
        , event/1
        , button/1
        , dropdown/1
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
           [] -> #{};
           UserID -> hd(nxo_user:find(UserID))
         end,
  Data = maps:merge(User, #{ all_groups => nxo_group:all() }),
  #template{ text=nxo_template:pretty_render(user_form, Data) }.

event(org_form_submit) ->
  submission();
event({new_org_form, UserID}) ->
  case wf:q(new_org) of
    [] -> ok;
    OrgAbbrv -> org_form(OrgAbbrv, UserID)
  end.

org_form(OrgAbbrv, UserID) ->
  wf:state(selected_orgs, [wf:to_binary(OrgAbbrv) | ?SELECTED_ORGS_DEFAULT]),
  Org = hd(nxo_org:find(OrgAbbrv)),
  User = case nxo_user:find(UserID) of
           [] -> #{};
           Users -> hd(Users)
         end,
  Groups = nxo_group:all_with_role(OrgAbbrv),
  Data = maps:merge(User, #{ org => Org,
                             non_global_groups => Groups }),
  Stanza = #template{ text=nxo_template:pretty_render(user_org_form, Data) },
  wf:replace(new_org_dropdown, dropdown({additional_orgs, UserID})),
  wf:insert_top(org_forms, Stanza).

button({submit, UserID}) ->
  Text = case UserID of
           [] -> "Add User";
           _  -> "Update User"
         end,
  #button{ id=org_form_submit,
           postback=org_form_submit,
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




submission() ->
  ?PRINT(wf:qs(global_group)),
  ok.
%%   case nxo_validate:validate(user_form, #{user_id => wf:state(objID)}) of
%%     true ->
%%       [UID, OID] = nxo_db:cascading_update(
%%                      [ {user_add, [ID | nxo_datamap:apply(user_add)]},
%%                        {user_org_add, nxo_datamap:apply(user_org_add)}
%%                      ]),
%% %      update_groups(UID, OID, selected_groups()),
%%       wf:redirect("/users");
%%     false ->
%%       nxo_view:report_validation_failure()
%%   end.


%% user_org_groups is a pretty fancy query.  It first deletes all of
%% the existing UID/OID associations in the table then inserts all of
%% the selected GIDs (possibly changing the OID to match the global
%% group if it's a global_only group). Note that the last three
%% parameters (UIDParam, OIDParam, GIDs) are arrays that are unnested
%% in the query and **must** be the same length (hence the duplicate).
%% update_groups(UID, OID, GIDs) ->
%%   BinGIDs = lists:map(fun wf:to_binary/1, GIDs),
%%   UIDParam = lists:duplicate(length(GIDs), UID),
%%   OIDParam = lists:duplicate(length(GIDs), OID),
%%   nxo_db:query(user_org_groups, [UID, OID, UIDParam, OIDParam, BinGIDs]).


%% % prepares the Organization and Group part of the form.  we did that
%% % part separately so that future versions could add multiple instances
%% % of this form segment (if the user is in more than one organization).
%% group_form() ->
%%   Data = #{orgs => nxo_org:all(),
%%            user => wf:state(obj),
%%            groups => nxo_group:all()},
%%   #template{ text=nxo_template:render(user_group, Data) }.

%% % returns a list of group_ids that have their checkboxes checked.
%% selected_groups() ->
%%   lists:filter(fun(G) -> wf:q(G) == G end, nxo_group:ids()).
