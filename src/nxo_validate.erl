-module(nxo_validate).
-include("nxo.hrl").

-export([
          valid_feedback/1
        , invalid_feedback/1
        , validate/1
        , validate/2
        ]).

%% @doc A basic response if the validation passed.  Side effect only.
-spec valid_feedback(ID :: atom()) -> term().
valid_feedback(ID) ->
  wf:wire(ID, #remove_class{class="is-invalid"}),
  wf:wire(ID, #add_class{class="is-valid"}).

%% @doc A basic response if the validation didn't pass.  Both of the
%% default feedback functions assume Bootstrap forms and
%% invalid-feedback class divs explaining the issue.  Side effect only.
-spec invalid_feedback(ID :: atom()) -> term().
invalid_feedback(ID) ->
  wf:wire(ID, #remove_class{class="is-valid"}),
  wf:wire(ID, #add_class{class="is-invalid"}).

%% @doc Given a spec file (priv/validation/<SPEC FILE>.erl) run it in
%% the current Nitrogen context (which has access to the form fields).
validate(Spec) ->
  validate(Spec, #{}).

validate(Spec, Params) when is_atom(Spec) ->
  validate(consult_spec(Spec), Params);

validate(Spec, Params) ->
  validate_fields(
    Spec,
    maps:get(valid_feedback, Params, fun ?MODULE:valid_feedback/1),
    maps:get(invalid_feedback, Params, fun ?MODULE:invalid_feedback/1),
    true, % assume valid till proven otherwise
    Params).

consult_spec(SpecFile) ->
  Filename = wf:to_list(SpecFile) ++ ".erl",
  App = nxo:application(),
  File = filename:join([code:priv_dir(App), "validation", Filename]),
  {ok, Terms} = file:consult(File),
  Terms.

trimmed_data(ID) ->
  case wf:q(ID) of
    V when is_list(V) -> string:trim(V);
    V                 -> V
  end.

validate_fields([], _, _, Flag, _) ->
  Flag;
validate_fields([{ID, Rules}|T], ValidFn, InvalidFn, Flag, Params) ->
  NewFlag = case validate_single_field(trimmed_data(ID), Rules, Params) of
              false ->
                InvalidFn(ID),
                false;
              true ->
                ValidFn(ID),
                Flag
            end,
  validate_fields(T, ValidFn, InvalidFn, NewFlag, Params).

validate_single_field(ID, Rules, Params) ->
  validate_single_field(ID, Rules, true, Params).

validate_single_field(_, [], Flag, _Params) ->
  Flag;
validate_single_field(ID, [H|T], Flag, Params) ->
  NewFlag = case validate(ID, H, Params) of
              false -> false;
              true  -> Flag
            end,
  validate_single_field(ID, T, NewFlag, Params).


%% required
validate([], required, _Params) ->
  false;
validate(Q, required, _Params) when not is_list(Q) ->
  false;
validate(_, required, _Params) ->
  true;


%% no-dup validation.
%%
%% Can be specified as two or four params:
%%
%%  {nodup, TABLE, COLUMN}
%%  {nodup, TABLE, COLUMN, IDColumn, IDKey}
%%
%% The ID Value will be looked up in the Params map by the IDKey, if a
%% value is not found, the two param validate will be applied.  (Note
%% that the two param is stricter, so this seems to make sense.)

validate([], {nodup, _, _}, _Params) ->
  true;
validate(V, {nodup, Table, Column}, _Params) ->
  not nxo_db:check_dup(Table, Column, V);
validate(V, {nodup, Table, Column, IDColumn, IDKey}, Params) ->
  case maps:get(IDKey, Params, not_specified) of
    not_specified -> validate(V, {nodup, Table, Column}, Params);
    ID -> not nxo_db:check_dup(Table, Column, V, IDColumn, ID)
  end;

%% match
%%
%% ensure that the value of one field matches another (like passwd,
%% passwd2).
validate(V, {match, OtherParam}, _Params) ->
  V == wf:q(OtherParam);

%% required_if
%%
%% the predicates can be:
%%   eq
%%   ne
%%   present
%%   absent
%%
%% these are not comprehensive against all forms nor are they very
%% complex but it fills the immediate need.  note that values might be
%% present even if they're not required, we don't check for that.
validate(Value, {required_if, Predicates}, Params) ->
  case required_if(Value, Predicates) of
    true -> validate(Value, required, Params);
    false -> true
  end;

%% integer
validate([], integer, _Params) ->
  true;
validate(Int, integer, _Params) ->
  try wf:to_integer(Int) of
      _ -> true
  catch
    _:_ -> false
  end;

%% roudable
validate([], roundable, _Params) ->
  true;
validate(Value, roundable, _Params) ->
  try string:to_float(Value) of
      _ -> true
  catch
    _:_ -> false
  end;

%% email
validate(Value, email, _Params) ->
  case re:run(Value, ".+@.+\..+") of
    {match, _} -> true;
    _          -> false
  end;

%% phone
validate([], phone, _Params) ->
  true;
validate(Value, phone, _Params) ->
  % $48 == 0; $57 == 9
  % essentially count the numerals and ensure there are at least 10.
  length(lists:filter(fun(X) -> X >=48 andalso X =< 57 end, Value)) >= 10;

%% other fields
validate([], {required_unless, OtherFields}, _Params) ->
  lists:any(fun(Field) -> wf:q(Field) =/= [] end, OtherFields);
validate(_, {required_unless, _}, _Params) ->
  true;

%% min/max length
validate(Value, {min, Min}, _Params) ->
  length(Value) >= Min;

validate(Value, {max, Max}, _Params) ->
  length(Value) =< Max;

validate(_, Validation, _Params) ->
  ?PRINT("Validation not recognized."),
  ?PRINT(Validation),
  false.


%%% required_if
required_if(Value, PredicateList) ->
  required_if(Value, PredicateList, false).

required_if(_, [], Flag) ->
  Flag;
required_if(Value, [H|T], Flag) ->
  NewFlag = case req_if_test(Value, H) of
              true -> true;
              false -> Flag
            end,
  required_if(Value, T, NewFlag).

% three argument predicates
req_if_test(_, {Field, eq, TestValue}) ->
  wf:q(Field) =:= TestValue;

req_if_test(_, {Field, ne, TestValue}) ->
  wf:q(Field) =/= TestValue;

req_if_test(_, {Field, present}) ->
  validate(wf:q(Field), required);

req_if_test(_, {Field, absent}) ->
  not validate(wf:q(Field), required).
