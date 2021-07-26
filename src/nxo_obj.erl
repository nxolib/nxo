-module(nxo_obj).
-include("nxo.hrl").
-export([ detail/3
        , detail/4
        ]).

%% @doc See detail/4
detail(Fn, ID, Attribute) ->
  detail(Fn, ID, Attribute, undefined).

%% @doc Find an attribute value for an object.
%%
%% Fn is a "find" function of artity one.
%% ID is the argument to the Fn.
%% Attribute is the map field to return
%% Default is what's returned if the object or attribute isn't found.
%%
%% Example: detail(fun nxo_org:find/1, ID, org_name, undefined).
-spec detail(Fn :: function(),
             ID :: term(),
             Attribute :: term(),
             Default :: term()) -> term().
detail(Fn, ID, Attribute, Default) ->
  case apply(Fn, [ID]) of
    [] ->
      Default;
    [Obj] ->
      maps:get(wf:to_binary(Attribute), Obj, Default);
    _ ->
      Default
  end.
