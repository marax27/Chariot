% escript main.erl

-module(main).
-export([main/1]).
-import(domain, [create_state/1]).
-import(central, [start_link/1]).


main(_Args) ->
    InitialState = create_state(2),
    start_link(InitialState),
    gen_server:cast(central, {report_incident, {32}}),
    gen_server:cast(central, {report_incident, {40}}),
    gen_server:cast(central, {report_incident, {85}}),
    gen_server:cast(central, {report_incident, {99}}),
    gen_server:call(central, {get_vehicles}),
    timer:sleep(10000),
    io:format("Shutting down.").
