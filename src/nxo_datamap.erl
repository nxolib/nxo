-module(nxo_datamap).
-include("nxo.hrl").

-export([
          apply/1
        , apply/2
        ]).

-export([
          to_rounded_int/1
        , emptystr/1
        , trim/1
        , trim_list/1
        , passwd/1
        , bool/1
        ]).

%% @doc Apply a datamap when there's a context to retrieve data from.
apply(Map) ->
  MapSpec = consult_spec(Map),
  Data = apply_map(MapSpec),
  Data.

%% @doc Apply a datamap against the params provided.  Note that
%% parameters are provided by wf:params/0 and is a proplist.
apply(Map, Params) ->
  MapSpec = consult_spec(Map),
  Data = apply_map_with_params(MapSpec, maps:from_list(Params)),
  Data.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% applying a map with passed parameters   %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
apply_map_with_params(MapSpec, ParamMap) ->
  apply_map(MapSpec, ParamMap, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% applying a map with a context %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
apply_map(MapSpec) ->
  apply_map(MapSpec, [], []).

apply_map([], _Params, Data) ->
  lists:reverse(Data);
apply_map([{FormField, Type, WashFns}|T], Params, Data) ->
  RawValue =
    case is_map(Params) of
      true ->  maps:get(wf:to_list(FormField), Params, []);
      false -> retrieve_parameter(FormField, Type)
    end,
  AllWashFns = default_wash_options(Type, WashFns),
  Value = lists:foldl(fun wash/2, RawValue, AllWashFns),
  apply_map(T, Params, [Value | Data]).

retrieve_parameter(FormField, Type) ->
  case lists:suffix("_list", wf:to_list(Type)) of
    true  -> wf:qs(FormField);
    false -> wf:q(FormField)
  end.

default_wash_options(Type, WashFns) ->
  case Type of
    string      -> [trim, emptystr | WashFns];
    emptystring -> [trim | WashFns ];
    passwd      -> [emptystr, passwd | WashFns];
    boolean     -> [bool | WashFns];
    integer     -> [trim, to_rounded_int | WashFns];
    string_list -> [trim_list | WashFns];
    _           -> WashFns
  end.
wash({M, F}, Val) ->
  apply(M, F, [Val]);
wash({M, F, A}, Val) ->
  apply(M, F, [Val | A]);
wash(F, Val) ->
  ?MODULE:F(Val).

consult_spec(Map) ->
  nxo:consult_file(Map, datamap, ".erl").

to_rounded_int(Str) ->
  case string:to_integer(Str) of
    {error, _} -> 0;
    {I, _} -> I
  end.

bool(true)        -> true;
bool("true")      -> true;
bool(<<"true">>)  -> true;
bool(false)       -> false;
bool("false")     -> false;
bool(<<"false">>) -> false;
bool(undefined)   -> false;
bool([])          -> false.


emptystr([]) -> undefined;
emptystr(Str) -> Str.

trim(Str) when is_list(Str) orelse is_binary(Str) ->
  string:trim(Str);
trim(NotStr) ->
  NotStr.

trim_list(NotList) when not is_list(NotList) ->
  [];
trim_list(List) ->
  [ trim(wf:to_binary(L)) || L <- List ].

passwd(Str) when length(Str) == 60 ->
  Str;
passwd(Str) ->
  erlpass:hash(Str).
