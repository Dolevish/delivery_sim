%% globals.hrl

%% הגדרת מבנה נתונים (Record) עבור הזמנה
-record(order, {
    id,                         % מזהה ייחודי של ההזמנה
    status = pending,           % הסטטוס הנוכחי של ההזמנה (pending, assigned, in_transit, completed)
    pickup_loc,                 % נקודת איסוף (בית העסק)
    dropoff_loc,                % נקודת מסירה (בית הלקוח)
    assigned_courier_id = none, % מזהה השליח ששויך להזמנה
    timestamps = #{}            % מפה לשמירת חותמות זמן (נוצרה, שויכה, נמסרה)
}).

%% הגדרת מבנה נתונים (Record) עבור שליח
-record(courier, {
    id,                         % מזהה ייחודי של השליח
    location,                   % המיקום הנוכחי של השליח (למשל, {X,Y})
    current_order_id = none,    % מזהה ההזמנה שהשליח מטפל בה כרגע
    stats = #{completed => 0,   % מפה לשמירת סטטיסטיקות (מספר משלוחים שהושלמו)
              packages => []},     % רשימת החבילות שהועברו
    route_data = #{}             % נתוני המסלול
}).