-include_lib("nitrogen_core/include/wf.hrl").

%% The name of this application.
-define(APP, nxo:application()).

%% The database pgpool is defined to use.
-define(DB, db).

%% The HTML templates directory.
-define(HTML_DIR, filename:join([code:priv_dir(?APP), "templates"])).

%% The DDL directory.
-define(DDL_DIR, filename:join([code:priv_dir(?APP), "ddl"])).

%% The SQL directory.
-define(SQL_DIR, filename:join([code:priv_dir(?APP), "sql"])).

%% The XSD directory.
-define(XSD_DIR, filename:join([code:priv_dir(?APP), "xsd"])).

%% Extension identifying a sql file.
-define(SQL_EXT, ".sql").

%% Valid content-types for API request/repsonses.
-define(API_CONTENT_TYPES, ["application/json", "application/xml"]).

%% The name of the NXO event handler.
-define(EVENT, nxo_event_handler).

%% The audit record.
-record(audit, {activity, user_id, target, result, comment}).

%%%%% MAIL STUFF
%%
%% Where the mail template are found (in /priv).
-define(TEMPLATE_DIR, "mail_templates").

%% Default name mail is sent from.
-define(MAIL_FROM_NAME, "NXO Mailer").

%%%%% CSS/Bootstrap Definitions
-define(btn_default,   "btn btn-sm btn-default").
-define(btn_primary,   "btn btn-sm btn-primary").
-define(btn_success,   "btn btn-sm btn-success").
-define(btn_danger,    "btn btn-sm btn-danger").
-define(btn_warning,   "btn btn-sm btn-warning").
-define(btn_secondary, "btn btn-sm btn-secondary").
-define(btn_info,      "btn btn-sm btn-info").
-define(btn_light,     "btn btn-sm btn-light").
-define(btn_dark,      "btn btn-sm btn-dark").
-define(btn_link,      "btn btn-link").
