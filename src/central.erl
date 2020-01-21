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

% Call: a synchronous operation that returns a value.
handle_call({get_vehicles}, _From, #state{vehicles = Vehicles} = State) ->
    io:format("Vehicles: ~p~n", [Vehicles]),
    Reply = {ok, Vehicles},
    make_reply(Reply, State);
handle_call(Request, _From, State) ->
    error_logger:warning_msg("Bad call request: ~p~n", [Request]),
    make_reply({error, bad_request}, State).

% Cast: an asynchronous operation that does not return a value.
handle_cast({report_incident, Details}, State) ->
    io:format("Incident reported: ~p~n", [Details]),
    {noreply, State};
handle_cast(Message, State) ->
    error_logger:warning_msg("Bad cast request: ~p~n", [Message]),
    {noreply, State}.

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

%--- Utilities ---------------------------

make_reply(ReturnValue, State) ->
    {reply, ReturnValue, State}.
