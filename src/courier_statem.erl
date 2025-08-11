%% src/courier_statem.erl
-module(courier_statem).
-behaviour(gen_statem).

-export([start_link/1, assign_order/2]).
-export([init/1, callback_mode/0, terminate/3, code_change/4]).
-export([idle/3, going_to_pickup/3, delivering/3]).

-include("globals.hrl").

-define(TICK_INTERVAL_MS, 100). % עדכון מיקום כל 100 מילישניות
-define(TRAVEL_TIME_SECONDS, 5). % זמן נסיעה בדוי בשניות

%%%===================================================================
%%% API Functions
%%%===================================================================
start_link(CourierData) ->
    gen_statem:start_link({local, proplists:get_value(id, CourierData)}, ?MODULE, CourierData, []).

assign_order(PidOrName, Order) ->
    gen_statem:cast(PidOrName, {assign_order, Order}).

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================
init(CourierData) ->
    ID = proplists:get_value(id, CourierData),
    Location = proplists:get_value(location, CourierData),
    StateData = #courier{id = ID, location = Location},
    ets:insert(courier_locations, {ID, Location, idle}),
    {ok, idle, StateData}.

callback_mode() -> state_functions.

%% @doc המצב שבו השליח ממתין להזמנה
idle(cast, {assign_order, Order}, Data) ->
    #order{id = OrderId, pickup_loc = PickupLoc, dropoff_loc = DropoffLoc} = Order,
    CourierId = Data#courier.id,
    order_server:assign(OrderId, CourierId),
    
    {ok, Timer} = timer:send_interval(?TICK_INTERVAL_MS, tick),
    
    RouteData = #{
        start_loc => Data#courier.location,
        end_loc => PickupLoc,
        final_dest => DropoffLoc,
        start_time => erlang:monotonic_time(millisecond),
        timer => Timer
    },
    
    NewData = Data#courier{route_data = RouteData, current_order_id = OrderId},
    ets:insert(courier_locations, {CourierId, Data#courier.location, going_to_pickup}),
    {next_state, going_to_pickup, NewData};
idle(_EventType, _EventContent, Data) ->
    {keep_state, Data}.

%% @doc המצב שבו השליח בדרך לאיסוף
going_to_pickup(info, tick, Data) ->
    update_location(going_to_pickup, Data);
going_to_pickup(_EventType, _EventContent, Data) ->
    {keep_state, Data}.

%% @doc המצב שבו השליח בדרך ללקוח
delivering(info, tick, Data) ->
    update_location(delivering, Data);
delivering(_EventType, _EventContent, Data) ->
    {keep_state, Data}.

terminate(_Reason, _State, Data) ->
    ets:delete(courier_locations, Data#courier.id),
    ok.

code_change(_OldVsn, State, Data, _Extra) ->
    {ok, State, Data}.

%%%===================================================================
%%% Internal Helper Functions
%%%===================================================================

%% @private חישוב מיקום חדש וטיפול בהגעה
update_location(Status, Data) ->
    RouteData = Data#courier.route_data,
    #{start_loc := {X1, Y1}, end_loc := {X2, Y2}, start_time := StartTime, timer := Timer} = RouteData,
    CourierId = Data#courier.id,
    
    Now = erlang:monotonic_time(millisecond),
    ElapsedTime = Now - StartTime,
    TravelDuration = ?TRAVEL_TIME_SECONDS * 1000,
    
    if ElapsedTime >= TravelDuration ->
        timer:cancel(Timer),
        ets:insert(courier_locations, {CourierId, {X2, Y2}, arrived}),
        handle_arrival(Status, Data);
    true ->
        Ratio = ElapsedTime / TravelDuration,
        NewX = X1 + round((X2 - X1) * Ratio),
        NewY = Y1 + round((Y2 - Y1) * Ratio),
        NewLocation = {NewX, NewY},
        ets:insert(courier_locations, {CourierId, NewLocation, Status}),
        {keep_state, Data#courier{location = NewLocation}}
    end.

%% @private טיפול בלוגיקה לאחר הגעה ליעד
handle_arrival(going_to_pickup, Data) ->
    CourierId = Data#courier.id,
    OrderId = Data#courier.current_order_id,
    RouteData = Data#courier.route_data,
    
    order_server:pickup(OrderId),
    
    {ok, NewTimer} = timer:send_interval(?TICK_INTERVAL_MS, tick),
    NewRouteData = RouteData#{
        start_loc => maps:get(end_loc, RouteData),
        end_loc => maps:get(final_dest, RouteData),
        start_time => erlang:monotonic_time(millisecond),
        timer => NewTimer
    },
    
    ets:insert(courier_locations, {CourierId, maps:get(end_loc, RouteData), delivering}),
    {next_state, delivering, Data#courier{route_data = NewRouteData}};

handle_arrival(delivering, Data) ->
    CourierId = Data#courier.id,
    OrderId = Data#courier.current_order_id,
    RouteData = Data#courier.route_data,

    order_server:complete(OrderId),

    NewStats = maps:update_with(completed, fun(V) -> V + 1 end, 1, Data#courier.stats),
    Packages = [OrderId | (maps:get(packages, Data#courier.stats, []))],
    FinalStats = maps:put(packages, Packages, NewStats),
    
    FinalLocation = maps:get(end_loc, RouteData),
    ets:insert(courier_locations, {CourierId, FinalLocation, idle}),
    
    NewData = Data#courier{
        current_order_id = none,
        stats = FinalStats,
        route_data = #{}, % איפוס נתוני המסלול
        location = FinalLocation
    },
    {next_state, idle, NewData}.
