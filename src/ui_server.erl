%% src/ui_server.erl
-module(ui_server).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("wx/include/wx.hrl").

-define(SERVER, ?MODULE).

-record(state, {
    frame,
    main_sizer,
    map_panel,
    timer
}).

%%%===================================================================
%%% API
%%%===================================================================
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    wx:new(),
    Frame = wxFrame:new(wx:null(), ?wxID_ANY, "Delivery Simulator", [{size, {1200, 800}}]),
    MainPanel = wxPanel:new(Frame),
    MainSizer = wxBoxSizer:new(?wxHORIZONTAL),
    
    %% ===============================================================
    %%  הפאנל הימני (סטטוס שליחים)
    %% ===============================================================
    RightPanel = wxPanel:new(MainPanel, [{style, ?wxSUNKEN_BORDER}]),
    RightSizer = wxStaticBoxSizer:new(?wxVERTICAL, RightPanel, [{label, "Couriers Status"}]),
    wxPanel:setSizer(RightPanel, RightSizer),
    
    %% ===============================================================
    %%  הפאנל המרכזי (מפה וכפתורים)
    %% ===============================================================
    CenterPanel = wxPanel:new(MainPanel),
    CenterSizer = wxBoxSizer:new(?wxVERTICAL),

    %% --- כפתורי שליטה ---
    ControlSizer = wxBoxSizer:new(?wxHORIZONTAL),
    wxButton:new(CenterPanel, ?wxID_ANY, [{label, "Reset Simulation"}]),
    wxButton:new(CenterPanel, ?wxID_ANY, [{label, "Pause Order Generator"}]),
    wxButton:new(CenterPanel, ?wxID_ANY, [{label, "Pause Simulation"}]),
    
    %% *** התיקון כאן ***
    %% הפרדת ה-flag וה-border לשני tuples נפרדים.
    wxSizer:add(CenterSizer, ControlSizer, [{flag, ?wxEXPAND bor ?wxALL}, {border, 5}]),

    %% --- פאנל המפה ---
    MapPanel = wxPanel:new(CenterPanel, [{style, ?wxSUNKEN_BORDER}]),
    wxPanel:setBackgroundColour(MapPanel, {240, 240, 240}),
    wxSizer:add(CenterSizer, MapPanel, [{proportion, 1}, {flag, ?wxEXPAND}]),

    wxPanel:setSizer(CenterPanel, CenterSizer),
    
    %% ===============================================================
    %%  הפאנל השמאלי (סטטוס הזמנות)
    %% ===============================================================
    LeftPanel = wxPanel:new(MainPanel, [{style, ?wxSUNKEN_BORDER}]),
    LeftSizer = wxStaticBoxSizer:new(?wxVERTICAL, LeftPanel, [{label, "Orders Status"}]),
    wxPanel:setSizer(LeftPanel, LeftSizer),

    %% ===============================================================
    %% הרכבת החלון הראשי
    %% ===============================================================
    wxSizer:add(MainSizer, LeftPanel,  [{proportion, 1}, {flag, ?wxEXPAND}]),
    wxSizer:add(MainSizer, CenterPanel,[{proportion, 3}, {flag, ?wxEXPAND}]),
    wxSizer:add(MainSizer, RightPanel, [{proportion, 1}, {flag, ?wxEXPAND}]),

    wxPanel:setSizer(MainPanel, MainSizer),

    wxFrame:show(Frame),

    State = #state{frame = Frame, main_sizer = MainSizer, map_panel = MapPanel},
    {ok, State}.

handle_call(_Msg, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    wx:destroy(),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
