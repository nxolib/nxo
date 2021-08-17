-module(nxo_directory).
-include("nxo.hrl").

-export([
         delete/2
        , find/2
        ]).

delete(OrgAbbrv, Directory) ->
  nxo_db:q(delete_directory, [OrgAbbrv, Directory]).

find(OrgAbbrv, Directory) ->
  nxo_db:q(find_directory, [OrgAbbrv, Directory]).
