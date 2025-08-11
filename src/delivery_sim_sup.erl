%% src/delivery_sim_sup.erl (מעודכן)
-module(delivery_sim_sup).
-behaviour(supervisor).

-export([start_link/0, start_zone/1]). % הוספת פונקציית API
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% @doc פונקציית API המאפשרת להוסיף אזור חדש לסימולציה באופן דינמי.
%% ZoneId: מזהה ייחודי לאזור (למשל, 1, 2, 3...)
start_zone(ZoneId) ->
    %% מפרט ה-child (הילד) שאנחנו רוצים להוסיף.
    %% כל אזור ינוהל על ידי supervisor משלו, מסוג zone_sup.
    ChildSpec = #{
        id => {zone_sup, ZoneId}, % מזהה ייחודי ל-child בתוך ה-supervisor
        start => {zone_sup, start_link, [ZoneId]}, % הפונקציה שתפעיל את ה-child
        restart => permanent,
        type => supervisor % חשוב: אנחנו מצהירים שה-child הזה הוא supervisor בעצמו
    },
    %% הפקודה supervisor:start_child מוסיפה את ה-child החדש ל-supervisor שכבר רץ.
    supervisor:start_child(?SERVER, ChildSpec).

%% @doc פונקציית האתחול של ה-supervisor הראשי.
init([]) ->
    %% אסטרטגיית הפיקוח.
    %% one_for_one: אם child (במקרה שלנו, zone_sup) קורס, רק הוא יופעל מחדש.
    SupFlags = #{strategy => one_for_one, intensity => 1, period => 5},

    %% בשלב האתחול, אין לנו ילדים סטטיים. נוסיף אותם באופן דינמי.
    Children = [],

    {ok, {SupFlags, Children}}.