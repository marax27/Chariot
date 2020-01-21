-module(central).
-behaviour(gen_server).
-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).
-import(firetrucks, [make/1, as_map/1]).

-record(state, {vehicles, enqueued_incidents = []}).

start_link(FiretruckCount) ->
    Vehicles = [make(Id) || Id <- lists:seq(1, FiretruckCount)],

    Return = gen_server:start_link({local, ?MODULE}, ?MODULE, Vehicles, []),
    io:format("start_link: ~p~n", [Return]),
    Return.

init(Vehicles) ->
    State = #state{vehicles = Vehicles},
    io:format("init: ~p~n", [State]),
    {ok, State}.

% Call: a synchronous operation that returns a value.
handle_call({get_vehicles}, _From, #state{vehicles = Vehicles} = State) ->
    Result = invoke_get_vehicles(Vehicles),
    io:format("get_vehicles: ~p~n", [Result]),
    make_reply({ok, Result}, State);
handle_call(Request, _From, State) ->
    error_logger:warning_msg("Bad call request: ~p~n", [Request]),
    make_reply({error, bad_request}, State).

% Cast: an asynchronous operation that does not return a value.
handle_cast({report_incident, Details}, State) ->
    NewState = invoke_report_incident(Details, State),
    io:format("Incident ~p enqueued. State: ~p~n", [Details, NewState]),
    {noreply, NewState};
handle_cast(Message, State) ->
    error_logger:warning_msg("Bad cast request: ~p~n", [Message]),
    {noreply, State}.

handle_info(Info, State) ->
    io:format("handle_info: ~p~n", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    io:format("terminate...~n"),
    ok.

code_change(_OldVsn, State, _Extra) ->
    io:format("code_change...~n"),
    {ok, State}.


%--- Invoke functions ----------------------------

invoke_get_vehicles(Vehicles) ->
    [as_map(V) || V <- Vehicles].


invoke_report_incident(Report, #state{enqueued_incidents = Queue} = State) ->
    State#state{enqueued_incidents = Queue ++ [Report]}.

%--- Utilities ---------------------------

make_reply(ReturnValue, State) ->
    {reply, ReturnValue, State}.
