-module(central).
-behaviour(gen_server).
-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).
-export([dispatch_firefighting_action/3]).
-import(firetrucks, [make/1, as_map/1, get_first_ready_id/1, update_vehicle_by_id/2]).

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
    io:format("get_vehicles: ~p~n", [State]),
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

handle_info({report_enqueued}, #state{enqueued_incidents=[Incident | _]} = State) ->
    io:format("Processing incident ~p...~n", [Incident]),
    NewState = invoke_report_enqueued(State),
    {noreply, NewState};
handle_info({action_finished, VehicleId}, State) ->
    io:format("A vehicle ~p has returned from action.~n", [VehicleId]),
    NewState = invoke_action_finished(VehicleId, State),
    {noreply, NewState};
handle_info(Info, State) ->
    error_logger:warning_msg("Bad info request: ~p~n", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    io:format("terminate...~n"),
    ok.

code_change(_OldVsn, State, _Extra) ->
    io:format("code_change...~n"),
    {ok, State}.


%--- Invoke functions ------------------

invoke_get_vehicles(Vehicles) ->
    [as_map(V) || V <- Vehicles].


invoke_report_incident(Report, #state{enqueued_incidents = Queue} = State) ->
    self() ! {report_enqueued},
    State#state{enqueued_incidents = Queue ++ [Report]}.

invoke_report_enqueued(#state{enqueued_incidents=[]} = State) ->
    io:format("No incidents in queue.~n"),
    State;
invoke_report_enqueued(#state{vehicles=Vehicles, enqueued_incidents=[Incident | Tail]} = State) ->
    case get_first_ready_id(Vehicles) of
        nil ->
            io:format("Cannot dispatch to incident ~p yet.~n", [Incident]),
            State;
        VehicleId ->
            io:format("Dispatching vehicle ~p to ~p~n", [VehicleId, Incident]),
            spawn(?MODULE, dispatch_firefighting_action, [VehicleId, self(), 10000]),
            State#state{
                vehicles = update_vehicle_by_id(VehicleId, Vehicles),
                enqueued_incidents = Tail
            }
    end.

invoke_action_finished(VehicleId, #state{vehicles = Vehicles} = State) ->
    % Skip notready.
    UpdatedVehicles = update_vehicle_by_id(VehicleId, update_vehicle_by_id(VehicleId, Vehicles)),
    NewState = State#state{vehicles = UpdatedVehicles},
    invoke_report_enqueued(NewState).

%--- Action mocks ----------------------

dispatch_firefighting_action(VehicleId, Pid, Duration) ->
    timer:sleep(Duration),
    Pid ! {action_finished, VehicleId}.

%--- Utilities -------------------------

make_reply(ReturnValue, State) ->
    {reply, ReturnValue, State}.
