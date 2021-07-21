-module(nxopage_login_audit).
-include("nxo.hrl").
-include_lib("paginate/include/records.hrl").

-export([
          main/0
        , title/0
        , body/0
        , paginate_event/4
        ]).

-export([ records/3 ]).

-security({groups, [administrators, usermgmt]}).
-postback_security({groups, [administrators, usermgmt]}).

main() ->
  #template{ file=nxo:template("login_audit.html") }.

title() -> "Login Audit".

paginate_event(records, SearchText, PerPage, Page) ->
  {Count, Body} = records(SearchText, Page, PerPage),
  #paginate_event{items=Count, body=Body}.

body() ->
  PerPage = 100,
  {Count, Body} = records("", 1, PerPage),
  #panel{
     body=[#paginate{
              id=records,
              perpage=PerPage,
              tag=records,
              body=Body,
              num_items=Count,
              perpage_options=[5,25,100,500]}]}.

records(Search, Page, PerPage) ->
  Offset = PerPage * (Page - 1),
  Count = nxo_db:scalar_query(user_audit_lookup_count, [Search]),
  Records =
    nxo_db:map_query(user_audit_lookup, [Search, PerPage, Offset]),
  {Count, format_records(Records)}.

format_records(Records) ->
  Rows = format_records(Records, []),
  #table{class="table table-sm",
         rows=[#tablerow{cells=[#tableheader{text="Timestamp"},
                                #tableheader{text="Account"},
                                #tableheader{text="Activity"},
                                #tableheader{text="Success"},
                                #tableheader{text="Comment"}]} | Rows]}.

format_records([], Res) -> lists:reverse(Res);
format_records([H|T], Res) ->
  {{Y,M,D},{Hour,Min,Sec}} = maps:get(<<"action_dt">>, H),
  Date = io_lib:format("~b-~2.10.0b-~2.10.0b ~b:~2.10.0b:~4.1.0f",
                       [Y, M, D, Hour, Min, Sec]),
  RowClass = case maps:get(<<"success">>, H) of
               true -> [];
               false -> "table-danger"
             end,
  Row = #tablerow{
           class=RowClass,
           cells=[#tablecell{class="text-monospace", text=Date},
                  #tablecell{text=maps:get(<<"email">>, H)},
                  #tablecell{text=maps:get(<<"activity">>, H)},
                  #tablecell{text=maps:get(<<"success">>, H)},
                  #tablecell{text=maps:get(<<"comment">>, H)}]},
  format_records(T, [Row | Res]).
