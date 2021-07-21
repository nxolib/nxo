-module(nxo_forms).
-include("nxo.hrl").

-export([
          find_obj/1
        , obj_defined/0
        , obj_id/0
        , form_value/1
        , bool_selected/2
        ]).

-define(NO_OBJ, #{}).

%% This module is an attempt at factoring out routine add/edit style
%% form presentation.  We assume that the object specifier (e.g., user
%% id or whatever) is specified by wf:path_info/0.

%% @doc Use the path_info as an argument to the function and attempt
%% to locate an object (a user, org, or whatever).  Store that object
%% in the wf:state.
-spec find_obj(FindFn :: function()) -> map().
find_obj(FindFn) ->
  wf:state(objID, wf:path_info()),
  wf:state(obj,
           case FindFn(wf:state(objID)) of
             [Obj] -> Obj;
             _     -> ?NO_OBJ
           end).

%% @doc Return true if the state holds an object and false otherwise.
-spec obj_defined() -> boolean().
obj_defined() ->
  not (wf:state(obj) == ?NO_OBJ orelse wf:state(obj) == undefined).

%% @doc Return the object ID (i.e., the path_info) that's been stored.
-spec obj_id() -> term().
obj_id() ->
  wf:state(objID).

%% @doc Given an object parameter, return the value of that paramter
%% or [] if the value is null or non-existent.  This is useful for
%% populating the value of forms for editing.
-spec form_value(Field :: atom() | binary()) -> term() | [].
form_value(password) ->
  maps:get(password, wf:state_default(obj, ?NO_OBJ), random_password(12));
form_value(Field) when is_atom(Field) ->
  form_value(wf:to_binary(Field));
form_value(Field) when is_binary(Field) ->
  case maps:get(Field, wf:state_default(obj, ?NO_OBJ), null) of
    null -> [];
    Value -> Value
  end.

%% supports form_value(password).
random_password(Len) ->
    Chrs = list_to_tuple("ABCDEFGHIJKLMNOPQRSTUVWXYZ" ++
                           "abcdefghijklmnopqrstuvwxyz0123456789"),
    ChrsSize = size(Chrs),
    F = fun(_, R) -> [element(rand:uniform(ChrsSize), Chrs) | R] end,
    lists:foldl(F, "", lists:seq(1, Len)).


%% @doc Provide a template form with an appropriate "selected" flag.
-spec bool_selected(Field :: term(), Value :: term()) -> selected | [].
bool_selected(Field, Value) ->
  case form_value(Field) =:= Value of
    true -> selected;
    false -> []
  end.
