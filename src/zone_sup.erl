%% src/zone_sup.erl (קובץ חדש)
-module(zone_sup).
-behaviour(supervisor).

-export([start_link/1]).
-export([init/1]).
-export([start_courier/2]). % API להוספת שליח לאזור

%%%===================================================================
%%% API Functions
%%%===================================================================

start_link(ZoneId) ->
    %% אנו רושמים את ה-supervisor תחת שם ייחודי שמכיל את מזהה האזור,
    %% כדי שנוכל למצוא אותו בקלות. למשל: zone_sup_1
    SupervisorName = list_to_atom("zone_sup_" ++ integer_to_list(ZoneId)),
    supervisor:start_link({local, SupervisorName}, ?MODULE, ZoneId).

%% @doc פונקציית API שמאפשרת להוסיף שליח חדש לאזור ספציפי.
start_courier(SupPidOrName, CourierData) ->
    CourierId = proplists:get_value(id, CourierData),
    ChildSpec = #{
        id => CourierId, % מזהה ייחודי ל-child
        start => {courier_statem, start_link, [CourierData]},
        restart => permanent, % אם תהליך השליח קורס, הפעל אותו מחדש
        type => worker
    },
    supervisor:start_child(SupPidOrName, ChildSpec).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init(ZoneId) ->
    %% ה-child הראשון של ה-supervisor הזה הוא ה-Zone Manager.
    %% הוא זה שינהל את הלוגיקה של האזור.
    ZoneManagerName = list_to_atom("zone_manager_" ++ integer_to_list(ZoneId)),
    Children = [
        #{
            id => ZoneManagerName,
            start => {zone_manager_server, start_link, [ZoneManagerName]},
            restart => permanent,
            type => worker
        }
    ],

    %% אסטרטגיית פיקוח: rest_for_one.
    %% אם ה-Zone Manager קורס (התהליך החשוב ביותר),
    %% נפעיל מחדש אותו ואת כל התהליכים שאחריו (כל השליחים באזור).
    SupFlags = #{strategy => rest_for_one, intensity => 10, period => 10},

    {ok, {SupFlags, Children}}.