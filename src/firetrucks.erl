-module(firetrucks).
-export([make/1]).

-record(firetruck, {
    id,
    status = ready
}).


make(Identifier) ->
    #firetruck{
        id = Identifier
    }.
