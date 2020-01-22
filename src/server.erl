-module(server).
-export([start_server/0, reportIncident/3, getFireTrucks/3]).

start_server() ->
    inets:start(),
    inets:start(httpd, [ 
        {modules, [ 
            mod_alias, 
            mod_auth, 
            mod_esi, 
            mod_actions, 
            mod_cgi, 
            mod_get, 
            mod_head, 
            mod_log, 
            mod_disk_log 
        ]}, 
        {port, 8080}, 
        {server_name, "Chariot"}, 
        {server_root, "./"}, 
        {document_root, "../website"},
        {directory_index, ["index.html"]},
        {error_log, "../logs/error.log"},
        {security_log, "../logs/security.log"},
        {transfer_log, "../logs/transfer.log"},
        {erl_script_alias, {"/api", [server]}},
        {mime_types, [
            {"json", "application/json"},
            {"html", "text/html"}, 
            {"css", "text/css"}, 
            {"js", "application/x-javascript"} 
            ]} 
    ]),
    receive
        _ -> init:stop()
    end.

reportIncident(SessionID, _Env, Input) ->
    gen_server:cast(central, {report_incident, Input}),
    mod_esi:deliver(SessionID, ["Report received\r\n"]).

getFireTrucks(SessionID, _Env, _Input) ->
    {ok, Vehicles} = gen_server:call(central, {get_vehicles}),
    Data = jsone:encode(Vehicles),
    mod_esi:deliver(SessionID, [Data]).
