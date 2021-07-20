-module(nxo_modal).
-include_lib("nxo.hrl").
-export([ modal/1, open/3, open/4, open/5, close/0, event/1 ]).

%% (reminder: based on ns_modal from the nolaSTAGE project)

modal(ID) ->
  #panel{ class="modal fade", html_id=ID, body=[
    #panel{ class="modal-dialog modal-xl", body=[
      #panel{ class="modal-content", body=[
        #panel{ class="modal-header", id=modal_header, body=[
          #h4{ id=modal_title }
        ]},
        #panel{ class="modal-body", id=modal_body, body=[] },
        #panel{ class="modal-footer", id=modal_footer, body=[
          #button{ class="btn btn-default", id=modal_cancel,
            data_fields=[{dismiss,modal}], text="Cancel" },
          #button{ class="btn btn-primary", id=modal_submit,
                   html_id=modal_submit, text="Save" }
        ] }
  ]} ]} ]}.

open(Postback, Title, Body) -> open(Postback, Title, Body, []).

open(Postback, Title, Body, Options) ->
  open(Postback, Title, Body, Options, undef).

open(Postback, Title, Body, Options, Delegate) ->
  wf:update(modal_title, Title),
  wf:update(modal_body, Body),
  lists:foreach(fun({ID, Content}) -> wf:update(ID, Content) end, Options),
  case Postback of
    {} -> ok;
    Pb ->
      Event = case Delegate of
                undef -> #event{type=click, postback=Pb};
                _     -> #event{type=click, postback=Pb, delegate=Delegate}
              end,
      wf:wire(#script{ script="objs('modal_submit').unbind('click')" }),
      wf:wire(modal_submit, Event)
  end,
  wf:wire(#script{ script="$('#modal').modal()" }),
  wf:flush().

close() ->
  wf:wire(#script{ script="$('#modal').modal('hide')" }),
  wf:flush().

event(close_modal) -> close().
