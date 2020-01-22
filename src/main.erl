% escript main.erl

-module(main).
-export([main/1]).
-import(domain, [create_state/1]).
-import(central, [start_link/1]).
-import(server, [start_server/0]).


main(_Args) ->
    InitialState = create_state(2),
    start_link(InitialState),
    spawn(server, start_server, []),
    receive
        _ -> init:stop()
    end.
