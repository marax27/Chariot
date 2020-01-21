-module(firetrucks).
-export([make/1, as_map/1, get_first_ready_id/1, update_vehicle_by_id/2]).

-record(firetruck, {
    id,
    status = ready
}).

as_map(#firetruck{id=Id, status=Status}) ->
    #{<<"id">>=>Id, <<"status">>=>Status}.

is_ready(#firetruck{status=Status}) ->
    Status == ready.

make(Identifier) ->
    #firetruck{
        id = Identifier
    }.

get_first_ready_id(Firetrucks) ->
    ReadyFiretrucks = lists:filter(fun is_ready/1, Firetrucks),
    Length = length(ReadyFiretrucks),
    case Length of
        0 ->
            nil;
        _ ->
            [#firetruck{id=Id} | _] = ReadyFiretrucks,
            Id
    end.

update_vehicle_by_id(_, []) -> [];
update_vehicle_by_id(Id, [#firetruck{id=Id, status=Status} = Firetruck | Tail]) ->
    UpdatedFiretruck = Firetruck#firetruck{status=next_status(Status)},
    [UpdatedFiretruck] ++ Tail;
update_vehicle_by_id(Id, [Firetruck | Tail]) ->
    [Firetruck] ++ update_vehicle_by_id(Id, Tail).

next_status(ready) -> dispatched;
next_status(dispatched) -> notready;
next_status(notready) -> ready.

