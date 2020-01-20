-module(main).
-compile([export_all]).
-import(firetrucks, [make/1, update/1]).
-import(misc, [stop/1]).


run_central(_Firetrucks) ->
    receive
        % ...
        {From, quit} ->
            From!{self(), ok}
    end.


main(_Args) ->
    Firetrucks = create_firetrucks(5),
    Central = spawn(main, run_central, [Firetrucks]),
    timer:sleep(500),
    stop_central(Central).


create_firetrucks(N) ->
    Seq = lists:seq(1, N),
    [make(X) || X <- Seq].


stop_central(Central) ->
    case stop(Central) of
        ok -> io:fwrite("Central process stopped gracefully.~n");
        Err -> io:format("Central process failed: ~p~n", [Err])
    end.


disp(Firetrucks) ->
    erlang:display(Firetrucks).
