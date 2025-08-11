%% order_server.erl
-module(order_server).
-behaviour(gen_server).

%% API - הפונקציות שהעולם החיצון יכול לקרוא
-export([start_link/1, get_state/1, assign/2, pickup/1, complete/1]).

%% gen_server callbacks - הפונקציות שהתהליך צריך לממש
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% כלול את הגדרות ה-Records מהקובץ הגלובלי
-include("globals.hrl").

%%%===================================================================
%%% API Functions
%%%===================================================================

%% @doc מתחיל תהליך חדש של הזמנה
start_link(OrderData) ->
    gen_server:start_link({local, proplists:get_value(id, OrderData)}, ?MODULE, OrderData, []).

%% @doc מחזיר את המצב הנוכחי של ההזמנה (קריאה סינכרונית)
get_state(PidOrName) ->
    gen_server:call(PidOrName, get_state).

%% @doc משייך שליח להזמנה (קריאה אסינכרונית)
assign(PidOrName, CourierId) ->
    gen_server:cast(PidOrName, {assign, CourierId}).

%% @doc מעדכן שההזמנה נאספה
pickup(PidOrName) ->
    gen_server:cast(PidOrName, pickup).

%% @doc מעדכן שההזמנה הושלמה
complete(PidOrName) ->
    gen_server:cast(PidOrName, complete).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @hidden
%% @doc מאתחל את השרת עם נתוני ההזמנה
init(OrderData) ->
    ID = proplists:get_value(id, OrderData),
    Pickup = proplists:get_value(pickup_loc, OrderData),
    Dropoff = proplists:get_value(dropoff_loc, OrderData),
    %% יצירת רשומה חדשה של הזמנה עם הנתונים שהתקבלו
    State = #order{
        id = ID,
        pickup_loc = Pickup,
        dropoff_loc = Dropoff,
        timestamps = #{created => erlang:timestamp()}
    },
    {ok, State}.

%% @hidden
%% @doc מטפל בקריאות סינכרוניות לשרת
handle_call(get_state, _From, State) ->
    {reply, State, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%% @hidden
%% @doc מטפל בקריאות אסינכרוניות לשרת
handle_cast({assign, CourierId}, State) ->
    io:format("Order ~p assigned to courier ~p~n", [State#order.id, CourierId]),
    %% עדכון הסטטוס ל'שויכה' ושמירת מזהה השליח וחותמת זמן
    NewState = State#order{
        status = assigned,
        assigned_courier_id = CourierId,
        timestamps = maps:put(assigned, erlang:timestamp(), State#order.timestamps)
    },
    {noreply, NewState};
handle_cast(pickup, State) ->
    io:format("Order ~p was picked up~n", [State#order.id]),
    %% עדכון הסטטוס ל'במשלוח'
    NewState = State#order{status = in_transit},
    {noreply, NewState};
handle_cast(complete, State) ->
    io:format("Order ~p completed!~n", [State#order.id]),
    %% עדכון הסטטוס ל'הושלמה' ושמירת חותמת זמן
    NewState = State#order{
        status = completed,
        timestamps = maps:put(completed, erlang:timestamp(), State#order.timestamps)
    },
    {noreply, NewState};
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @hidden
handle_info(_Info, State) ->
    {noreply, State}.

%% @hidden
terminate(_Reason, _State) ->
    ok.

%% @hidden
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.