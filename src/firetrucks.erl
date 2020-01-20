-module(firetrucks).
-compile([export_all]).

-record(firetruck, {
    id,
    status = ready,
    last_prepare_time = 0
}).

make(Identifier) ->
    #firetruck{id = Identifier}.


update(Truck) ->
    NewStatus = next_status(Truck),
    Truck#firetruck{status = NewStatus}.


next_status(Truck) ->
    Status = Truck#firetruck.status,
    case Status of
        ready -> dispatched;
        dispatched -> preparing;
        preparing -> ready;
        true -> erlang:error(Status)
    end.

