-module(nxopage_org_info).
-include("nxo.hrl").
-export([
          open_panel/1
        ]).

-security({groups, [administrators, usermgmt]}).
-postback_security({groups, [administrators, usermgmt]}).


open_panel(ID) ->
  [Org] = nxo_org:find(ID),
  Button = #button{class="btn btn-primary",
                   id=modal_cancel,
                   data_fields=[{dismiss,modal}], text="Close"},
  Body = nxo_template:pretty_render(organization, Org),
  nxo_modal:open({modal_postback, ID},
                 "Organization Details",
                 Body,
                 [{modal_footer, Button}]).
