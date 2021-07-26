-module(nxopage_query_inspector).
-include("nxo.hrl").
-export([
          main/0
        , title/0
        , body/0
        , event/1
        ]).

-security({group, administrators}).
-postback_security({group, administrators}).

main() ->
  #template{ file=nxo:template("query_inspector.html") }.

title() ->
  "NXO: Query Inspector".

body() ->
  [ #h2{ text="NXO Query Inspector" },
    #panel{ id=dropdown, body=template_dropdown(), class="mb-4" },
    #panel{ id=template_text },
    #panel{ id=values_form }
  ].

event(template) ->
  Template = wf:q(sql_template_name),
  {File, SQL} = nxo_sql_cache:lookup(Template),
  LastValues = nxo_query_cache:lookup(wf:to_atom(Template)),
  {Count, _} = find_placeholders(SQL),
  wf:update(values_form, make_values_form(Count, LastValues)),
  wf:update(template_text, sql_card(Template, SQL, File));

event(search) ->
  Template = wf:q(sql_template_name),
  {_, SQL} = nxo_sql_cache:lookup(Template),
  {Count, _} = find_placeholders(SQL),
  Params = [ case wf:q(wf:to_list(I)) of
               "null" -> null;
               String -> unicode:characters_to_binary(String)
             end || I <- lists:seq(1, Count) ],
  QueryFn = case wf:q(qtype) of
            "plain"  -> query;
            "scalar" -> scalar_query;
            "list"   -> list_query;
            "map"    -> map_query
          end,
  Res = nxo_db:QueryFn(Template, Params),
  ?PRINT(Params),
  ?PRINT(Res)
  ;


event(_) ->
  ok.


sql_card(Title, Body, File) ->
  #panel{
     class="card",
     body=[ #panel{ class="card-header text-center h4", text=Title },
            #panel{ class="card-body",
                    body=#panel{ class="pre card-text", text=Body } },
            #panel{ class="card-footer text-muted text-center", text=File }]}.

template_dropdown() ->
  Options = [ #option{} |
              [ #option{ text=Tmpl, value=Tmpl } || Tmpl <- template_list() ]],
  #dropdown{ id=sql_template_name,
             options=Options,
             postback=template
           }.

template_list() ->
  maps:keys(nxo_sql_cache:dump()).

make_values_form(Count, LastValues) ->
  Submit = #button{class="btn btn-primary btn-sm m-3",
                   delegate=?MODULE,
                   postback=search,
                   text="Run Query"},
  QType = #dropdown{ id=qtype,
                     class="form-control form-control-sm m-3",
                     value=plain,
                     options=[
                              #option{ value=plain, text="Raw" },
                              #option{ value=scalar, text="Scalar" },
                              #option{ value=list, text="List" },
                              #option{ value=map, text="Map" } ]},

  Boxes = value_boxes(Count, LastValues),
  #panel{
     class="card",
     body=[ #panel{ class="card-header h4", text="Run Query" },
            #panel{ class="card-body",
                    body=#panel{ class="card-text",
                                 body=[QType, Submit, Boxes] }}]}.


value_boxes(Count, LastValues) ->
  [ #panel{
       class="mb-3 col-sm-5",
       body=[#span{ class="mx-2", text="$" ++ wf:to_list(N) },
             #textbox{id=wf:to_list(N),
                      class="form-control form-control-sm",
                      text=lists:nth(N, LastValues)}
            ]}
    || N <- lists:seq(1, Count) ].


find_placeholders(SQL) ->
  {ok, Regexp} = re:compile("\\$(\\d+(?:::[A-Z]+)?)", [multiline]),
  case re:run(SQL, Regexp, [global, {capture, all_but_first, list}]) of
    nomatch -> {0, #{}};
    {match, Res} ->
      UniqRes = sets:to_list(sets:from_list(Res)),
      parse_placeholders(UniqRes)
  end.

parse_placeholders(Ps) ->
  parse_placeholders(Ps, #{}).

parse_placeholders([], Res) ->
  Max = lists:max(maps:keys(Res)),
  {Max, Res};
parse_placeholders([[P]|Rest], Res) ->
  {Idx, Hint} = case re:split(P, "::", [{return, list}, {parts, 2}]) of
                  [Index] ->
                    {wf:to_integer(Index), none};
                  [Index, Type] ->
                    {wf:to_integer(Index), Type}
                end,
  parse_placeholders(Rest, maps:put(Idx, Hint, Res)).
