-module(firetrucks).
-export([make/1, as_map/1, choose_ready_id/1, update_vehicle_by_id/2]).

-record(firetruck, {
    id,
    status = ready,
    waiting_since = os:system_time()
}).

as_map(#firetruck{id=Id, status=Status}) ->
    #{<<"id">>=>Id, <<"status">>=>Status}.

is_ready(#firetruck{status=Status}) ->
    Status == ready.

make(Identifier) ->
    #firetruck{
        id = Identifier
    }.

choose_ready_id(Firetrucks) ->
    ReadyFiretrucks = lists:filter(fun is_ready/1, Firetrucks),
    Length = length(ReadyFiretrucks),
    case Length of
        0 ->
            nil;
        _ ->
            #firetruck{id=Id} = get_longest_waiting(ReadyFiretrucks),
            Id
    end.

update_vehicle_by_id(_, []) -> [];
update_vehicle_by_id(Id, [#firetruck{id=Id, status=Status, waiting_since=T} = Firetruck | Tail]) ->
    NewT = case Status of
        notready -> os:system_time();
        _Else -> T
    end,
    UpdatedFiretruck = Firetruck#firetruck{status=next_status(Status), waiting_since=NewT},
    [UpdatedFiretruck] ++ Tail;
update_vehicle_by_id(Id, [Firetruck | Tail]) ->
    [Firetruck] ++ update_vehicle_by_id(Id, Tail).

next_status(ready) -> dispatched;
next_status(dispatched) -> notready;
next_status(notready) -> ready.

get_longest_waiting([X]) -> X;
get_longest_waiting([#firetruck{waiting_since = T1} = F1, #firetruck{waiting_since = T2} = F2]) ->
    if T1 < T2 -> F1;
       true -> F2
    end;
get_longest_waiting([F1, F2 | Tail]) ->
    Choice = get_longest_waiting([F1, F2]),
    get_longest_waiting([Choice] ++ Tail).
