-module(firetrucks).
-export([make/1, as_map/1]).

-record(firetruck, {
    id,
    status = ready
}).

as_map(#firetruck{id=Id, status=Status}) ->
    #{<<"id">>=>Id, <<"status">>=>Status}.


make(Identifier) ->
    #firetruck{
        id = Identifier
    }.
