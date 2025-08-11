%% courier_statem.erl
-module(courier_statem).
-behaviour(gen_statem).

%% API
-export([start_link/1, assign_order/2]).

%% gen_statem callbacks
-export([init/1, callback_mode/0, terminate/3, code_change/4]).
-export([idle/3, going_to_pickup/3, delivering/3]). % פונקציה נפרדת לכל מצב

-include("globals.hrl").

%% נגדיר זמני נסיעה קבועים לצורך הדוגמה (באלפיות השנייה)
-define(TRAVEL_TIME, 5000). % 5 שניות

%%%===================================================================
%%% API Functions
%%%===================================================================

%% @doc מתחיל תהליך חדש של שליח
start_link(CourierData) ->
    gen_statem:start_link({local, proplists:get_value(id, CourierData)}, ?MODULE, CourierData, []).

%% @doc משייך הזמנה לשליח (אירוע חיצוני)
assign_order(PidOrName, Order) ->
    gen_statem:cast(PidOrName, {assign_order, Order}).


%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

init(CourierData) ->
    ID = proplists:get_value(id, CourierData),
    Location = proplists:get_value(location, CourierData),
    %% יצירת רשומה חדשה של שליח
    StateData = #courier{id = ID, location = Location},
    %% המצב ההתחלתי של השליח הוא 'idle'
    {ok, idle, StateData}.

%% @doc מגדיר את אופן הטיפול במצבים - פונקציה נפרדת לכל מצב
callback_mode() ->
    state_functions.

%% @doc פונקציית המצב 'idle' (השליח ממתין)
idle(cast, {assign_order, Order}, Data) ->
    #order{id = OrderId, pickup_loc = PickupLoc} = Order,
    CourierId = Data#courier.id,
    io:format("Courier ~p received order ~p. Going to ~p~n", [CourierId, OrderId, PickupLoc]),
    
    %% שולח הודעה לתהליך ההזמנה שהוא שויך אליו
    order_server:assign(OrderId, CourierId),

    %% *** התיקון כאן ***
    %% מעבר למצב הבא, עם עדכון הנתונים, והפעלת טיימר.
    %% הפעולה האחרונה היא רק המספר השלם של זמן ה-timeout.
    {next_state, going_to_pickup, Data#courier{current_order_id = OrderId, location = PickupLoc},
     ?TRAVEL_TIME};
idle(_EventType, _EventContent, Data) ->
    {keep_state, Data}.


%% @doc פונקציית המצב 'going_to_pickup' (בדרך לאיסוף)
going_to_pickup(timeout, _EventContent, Data) ->
    OrderId = Data#courier.current_order_id,
    CourierId = Data#courier.id,
    io:format("Courier ~p arrived at pickup location for order ~p.~n", [CourierId, OrderId]),

    %% מודיע לתהליך ההזמנה שהחבילה נאספה
    order_server:pickup(OrderId),

    %% *** התיקון כאן ***
    %% מעבר למצב הבא (משלוח), והפעלת טיימר.
    {next_state, delivering, Data, ?TRAVEL_TIME};
going_to_pickup(_EventType, _EventContent, Data) ->
    {keep_state, Data}.


%% @doc פונקציית המצב 'delivering' (בדרך ללקוח)
delivering(timeout, _EventContent, Data) ->
    OrderId = Data#courier.current_order_id,
    CourierId = Data#courier.id,
    io:format("Courier ~p delivered order ~p! Returning to base.~n", [CourierId, OrderId]),
    
    %% מודיע לתהליך ההזמנה שהמשלוח הושלם
    order_server:complete(OrderId),

    %% עדכון הסטטיסטיקות של השליח
    NewStats = maps:update_with(completed, fun(V) -> V + 1 end, 1, Data#courier.stats),
    Packages = [OrderId | (maps:get(packages, Data#courier.stats, []))],
    FinalStats = maps:put(packages, Packages, NewStats),

    %% חזרה למצב ההתחלתי, עם נתונים מעודכנים (ללא timeout חדש)
    {next_state, idle, Data#courier{current_order_id = none, stats = FinalStats}};
delivering(_EventType, _EventContent, Data) ->
    {keep_state, Data}.

%% @hidden
terminate(_Reason, _State, _Data) ->
    ok.

%% @hidden
code_change(_OldVsn, State, Data, _Extra) ->
    {ok, State, Data}.
