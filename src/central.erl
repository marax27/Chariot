-module(central).
-behaviour(gen_server).
-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).
-import(firetrucks, [make/1]).

-record(state, {vehicles}).

start_link(FiretruckCount) ->
    Vehicles = [make(Id) || Id <- lists:seq(1, FiretruckCount)],

    Return = gen_server:start_link({local, ?MODULE}, ?MODULE, Vehicles, []),
    io:format("start_link: ~p~n", [Return]),
    Return.

init(Vehicles) ->
    State = #state{vehicles = Vehicles},
    Return = {ok, State},
    io:format("init: ~p~n", [State]),
    Return.

handle_call(_Request, _From, State) ->
    Reply = ok,
    Return = {reply, Reply, State},
    io:format("handle_call: ~p~n", [Return]),
    Return.

handle_cast(_Msg, State) ->
    Return = {noreply, State},
    io:format("handle_cast: ~p~n", [Return]),
    Return.

handle_info(_Info, State) ->
    Return = {noreply, State},
    io:format("handle_info: ~p~n", [Return]),
    Return.

terminate(_Reason, _State) ->
    io:format("terminate~n"),
    ok.

code_change(_OldVsn, State, _Extra) ->
    Return = {ok, State},
    io:format("code_change: ~p~n", [Return]),
    Return.
