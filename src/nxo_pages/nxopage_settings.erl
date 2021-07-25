-module(nxopage_settings).
-include("nxo.hrl").
-export([
          main/0
        , title/0
        , body/0
        , event/1
        , display_settings/1
        , display_button/1
        ]).

-export([ type/1, description/1 ]).

-security({group, administrators}).
-postback_security({group, administrators}).


main() ->
  #template{ file=nxo:template("settings.html") }.

title() -> "Site Settings".

event({save, Group}) ->
  save_settings(Group),
  wf:wire(group_btn_id(Group), #add_class{class=disabled});
event(_) ->
  ok.


body() ->
  maps:fold(fun display_group_card/3, [], nxo_settings:list_groups()).

display_group_card(Group, Label, Acc) ->
  Card = #panel{
            class="card mt-5",
            body=[ #panel{
                      class="card-body",
                      body=[ #h4{ text=Label },
                             #panel{ class="mb-3 text-muted", text=Group },
                             display_settings(Group),
                             display_button(Group) ]} ]},
  [Card | Acc].

display_settings(Group) ->
  [ item(Item) || Item <- nxo_settings:get(Group) ].

save_settings(Group) ->
  [ nxo_settings:set(Group, name(Item), wf:q(form_id(Item)))
    || Item <- nxo_settings:get(Group) ].

display_button(Group) ->
  #button{ text="Save Settings",
           id=group_btn_id(Group),
           postback={save, Group},
           class="btn btn-sm btn-primary btn disabled" }.

item(Item) ->
  #panel{class="form-group",
         body=[#label{text=display(Item)},
               #textbox{class="form-control",
                        id=form_id(Item),
                        text=value(Item),
                        actions=#event{
                                   target=group_btn_id(group(Item)),
                                   type=input,
                                   actions=#remove_class{class=disabled}}},
               #span{class="form-text text-muted", text=name(Item)}]}.

group_btn_id(Group) ->
  wf:to_atom("group_save_btn_" ++ wf:to_list(Group)).

group(Item)       -> maps:get(<<"setting_group">>,   Item).
display(Item)     -> maps:get(<<"setting_display">>, Item).
name(Item)        -> maps:get(<<"setting_name">>,    Item).
type(Item)        -> maps:get(<<"setting_type">>,    Item).
value(Item)       -> maps:get(<<"setting_value">>,   Item, []).
description(Item) -> maps:get(<<"setting_desc">>,    Item, []).

form_id(Item) -> iolist_to_binary([group(Item), "::", name(Item)]).
