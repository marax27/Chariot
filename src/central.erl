-module(central).
-behaviour(gen_server).
-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).
-import(domain, [
    dispatch_firefighting_action/3, prepare_for_next_dispatch/3,
    invoke_get_vehicles/1, invoke_report_incident/2,
    invoke_report_enqueued/1, invoke_dispatch_finished/2,
    invoke_preparation_finished/2]).


start_link(InitialState) ->
    Return = gen_server:start_link({local, ?MODULE}, ?MODULE, InitialState, []),
    io:format("start_link: ~p~n", [Return]),
    Return.

init(InitialState) ->
    io:format("init: ~p~n", [InitialState]),
    {ok, InitialState}.

% Call: a synchronous operation that returns a value.
handle_call({get_vehicles}, _From, State) ->
    Result = invoke_get_vehicles(State),
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

% Info: for internal purposes.
handle_info({report_enqueued}, State) ->
    NewState = invoke_report_enqueued(State),
    {noreply, NewState};
handle_info({dispatch_finished, VehicleId}, State) ->
    io:format("A vehicle ~p has returned from action.~n", [VehicleId]),
    NewState = invoke_dispatch_finished(VehicleId, State),
    {noreply, NewState};
handle_info({preparation_finished, VehicleId}, State) ->
    io:format("A vehicle ~p is now ready for another dispatch.~n", [VehicleId]),
    NewState = invoke_preparation_finished(VehicleId, State),
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


%--- Utilities -------------------------

make_reply(ReturnValue, State) ->
    {reply, ReturnValue, State}.
