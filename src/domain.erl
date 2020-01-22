-module(domain).
-export([create_state/1]).
-export([dispatch_firefighting_action/3, prepare_for_next_dispatch/3]).
-export([invoke_get_vehicles/1, invoke_report_incident/2,
    invoke_report_enqueued/1, invoke_action_finished/2,
    invoke_preparation_finished/2]).
-import(firetrucks, [make/1, as_map/1, get_first_ready_id/1, update_vehicle_by_id/2]).

-record(state, {vehicles, enqueued_incidents = []}).


create_state(VehicleCount) ->
    Vehicles = [make(Id) || Id <- lists:seq(1, VehicleCount)],
    #state{
        vehicles = Vehicles
    }.


%--- Invoke functions ------------------

invoke_get_vehicles(#state{vehicles = Vehicles}) ->
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
    UpdatedVehicles = update_vehicle_by_id(VehicleId, Vehicles),
    spawn(?MODULE, prepare_for_next_dispatch, [VehicleId, self(), 5000]),
    State#state{vehicles = UpdatedVehicles}.


invoke_preparation_finished(VehicleId, #state{vehicles = Vehicles} = State) ->
    UpdatedVehicles = update_vehicle_by_id(VehicleId, Vehicles),
    NewState = State#state{vehicles = UpdatedVehicles},
    invoke_report_enqueued(NewState).


%--- Action mocks ----------------------

dispatch_firefighting_action(VehicleId, Pid, Duration) ->
    timer:sleep(Duration),
    Pid ! {action_finished, VehicleId}.


prepare_for_next_dispatch(VehicleId, Pid, Duration) ->
    timer:sleep(Duration),
    Pid ! {preparation_finished, VehicleId}.
