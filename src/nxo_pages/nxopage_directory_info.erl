-module(nxopage_directory_info).
-include("nxo.hrl").
-export([ open_panel/2 ]).

-security({groups, [administrators, usermgmt]}).
-postback_security({groups, [administrators, usermgmt]}).

open_panel(OrgAbbrv, Directory) ->
  [Dir] = nxo_directory:find(OrgAbbrv, Directory),
  Button = #button{ class=?btn_primary,
                    id=modal_cancel,
                    data_fields=[{dismiss,modal}], text="Close"},
  Body = nxo_template:pretty_render(directory, Dir),
  nxo_modal:open({modal_postback, xxx},
                 "Directory Details",
                 Body,
                 [{modal_footer, Button}]).
