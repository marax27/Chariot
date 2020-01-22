-module(server).
-export([start/0, service/3, reportIncident/3]). 

start() ->
   inets:start(),
   inets:start(httpd, [ 
      {modules, [ 
         mod_alias, 
         mod_auth, 
         mod_esi, 
         mod_actions, 
         mod_cgi, 
         mod_dir,
         mod_get, 
         mod_head, 
         mod_log, 
         mod_disk_log 
      ]}, 
      {port, 8080}, 
      {server_name, "Chariot"}, 
      {server_root, "./"}, 
      {document_root, "../website"},
      {error_log, "./logs/error.log"},
      {security_log, "./logs/security.log"},
      {transfer_log, "./logs/transfer.log"},
      {erl_script_alias, {"/api", [server]}},
      {mime_types, [
         {"json", "application/json"},
         {"html", "text/html"}, 
         {"css", "text/css"}, 
         {"js", "application/x-javascript"} 
         ]} 
   ]).

reportIncident(SessionID, _Env, _Input) ->
   mod_esi:deliver(SessionID, ["Report received\r\n"]).

%getFireTrucks(SessionId, _Env, _Input) ->
% trucks to json and send json as string