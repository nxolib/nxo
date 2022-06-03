-module(nxo_mail).
-include("nxo.hrl").

-export([
          send/2
        , send/3
        , send_to_id/2
        , send_to_id/3
        , send_to_admin/1
        , send_to_admin/2
        ]).

-spec send(atom(), string()) -> ok.
send(Template, To) ->
  send(Template, To, #{}).

%% @doc Send an email to the address specified.  Note that Params, if
%% provided, will be a map with *atom* keys.  If there's a key
%% 'subject' that will be the subject line.
-spec send(atom(), string(), map()) -> ok.
send(TemplateFile, To, Params) ->
  Body = render_template(TemplateFile, Params),
  Subject = maps:get(subject, Params, ?MAIL_FROM_NAME),
  Mail = io_lib:format("Subject: ~s\r\n"
                       "From: ~s\r\n"
                       "To: ~s\r\n\r\n"
                       "~s", [Subject, ?MAIL_FROM_NAME, To, Body]),
  Message = {mail_from(), [To], Mail},
  Options = [{relay,mail_server()}, {port,mail_port()}],
  gen_smtp_client:send(Message, Options).


-spec send_to_id(atom(), uuid:uuid_string()) -> ok.
send_to_id(TemplateFile, ID) ->
  send_to_id(TemplateFile, ID, #{}).

%% @doc Like send/3 but pass an ID instead of an email address.
-spec send_to_id(atom(), uuid:uuid_string(), map()) -> ok.
send_to_id(TemplateFile, ID, Params) ->
  [UserData] = nxo_user:find(ID),
  send(TemplateFile, maps:get(<<"email">>, UserData), Params).

-spec send_to_admin(atom()) -> ok.
send_to_admin(TemplateFile) ->
  send_to_admin(TemplateFile, #{}).

%% @doc Like send/3 but send to the administrator address.
-spec send_to_admin(atom(), map()) -> ok.
send_to_admin(TemplateFile, Params) ->
  send(TemplateFile, admin_addr(), Params).


%%%%%%%%%%%%%%%%%%%%%%%%
%% INTERNAL FUNCTIONS %%
%%%%%%%%%%%%%%%%%%%%%%%%

render_template(TemplateFile, Params) ->
  Template = read_template(TemplateFile),
  bbmustache:render(Template, Params, [{key_type, atom}]).

read_template(TemplateFile) ->
  FilePath = template_path(TemplateFile),
  {ok, Template} = file:read_file(FilePath),
  Template.


%% TODO: This template stuff has an analogue in
%% nxo_template_name_cache.  It'd be nice to refactor it into a little
%% library routine.
template_path(Template) when is_binary(Template); is_atom(Template) ->
  template_path(wf:to_list(Template));
template_path(Template) ->
  find_template(filename:join(["**", Template]), mail_template_dirs()).

find_template(Template, []) ->
  error("Mail Template " ++ Template ++ " not found.");
find_template(Template, [H|T]) ->
  case filelib:wildcard(Template, H) of
    [X | _] -> filename:join([H, X]);
    []      -> find_template(Template, T)
  end.

mail_template_dirs() ->
  case application:get_env(nxo, mail_template_dir) of
    undefined ->
      error('mail_template_dir not defined');
    {ok, Paths} ->
      parse_paths(lists:reverse(Paths), [])
  end.

parse_paths([], Acc) ->
  lists:reverse(Acc);
parse_paths([{priv_dir, App, SubDir}|T], Acc) ->
  Path = filename:join(code:priv_dir(App), SubDir),
  parse_paths(T, [Path | Acc]);
parse_paths([{path, Path}|T], Acc) ->
  parse_paths(T, [Path | Acc]).

mail_server()    -> nxo_settings:get(mail, smtp_host).
mail_port()      -> wf:to_integer(nxo_settings:get(mail, smtp_port)).
mail_from()      -> nxo_settings:get(mail, smtp_from).
admin_addr()     -> nxo_settings:get(mail, admin_addr).
