-module(nxopage_user_info).
-include("nxo.hrl").
-export([
          open_panel/1
        ]).

-security({groups, [administrators, usermgmt]}).

open_panel(ID) ->
  [UserData] = nxo_auth_user:find(ID),
  Data = #{ user => UserData },
  Button = #button{ class="btn btn-primary",
                    id=modal_cancel,
                    data_fields=[{dismiss,modal}], text="Close" },
  Body = nxo_template:pretty_render(user, Data),
  nxo_modal:open({modal_postback, ID},
                 "User Details",
                 Body,
                 [{modal_footer, Button}]).
