-module(nxopage_settings).
-include("nxo.hrl").
-export([
          main/0
        , title/0
        , body/0
        , event/1
        , button/1
        ]).

-security({group, administrators}).
-postback_security({group, administrators}).

main() ->
  #template{ file=nxo:template("settings.html") }.

title() -> "Site Settings".

body() ->
  Data = #{ groups => nxo_settings:list_groups() },
  #template{ text=nxo_template:render(settings, Data) }.

event({save, Group}) ->
  Changes =
    lists:foldl(fun(S, Updates) ->
                    Setting = wf:to_list(maps:get(<<"setting">>, S)),
                    OldValue = wf:to_list(maps:get(<<"setting_value">>, S)),
                    ID = Group ++ "_" ++ Setting,
                    case wf:q(ID) == OldValue of
                      true  -> Updates;
                      false -> [{Setting, wf:q(ID), OldValue} | Updates]
                    end end, [], nxo_settings:group_settings(Group)),
  lists:foreach(fun({Setting, Value, OldValue}) ->
                    nxo_settings:set(Group, Setting, Value),
                    nxo:notify({setting_change,
                                {Group, Setting, Value, OldValue, wf:user()}})
                end, Changes),
  wf:update(settingpage, body()).

button({save_group, SettingGroup}) ->
  #button{ text="Save Settings",
           postback={save, SettingGroup},
           class="btn btn-sm btn-primary" }.
