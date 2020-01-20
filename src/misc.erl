-module(misc).
-compile([export_all]).


stop(ProcessToStop) ->
    ProcessToStop!{self(), quit},
    receive
        {ProcessToStop, Result} -> Result
    end.
