%% src/ui_server.erl
-module(ui_server).
-behaviour(gen_server).

-export([start_link/0, get_state/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("wx/include/wx.hrl").

-define(SERVER, ?MODULE).
-define(REFRESH_INTERVAL_MS, 40).

-record(state, {
    frame,
    map_panel,
    timer,
    couriers = []
}).

%%%===================================================================
%%% API
%%%===================================================================
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

get_state() ->
    gen_server:call(?SERVER, get_state).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    wx:new(),
    Frame = wxFrame:new(wx:null(), ?wxID_ANY, "Delivery Simulator", [{size, {1200, 800}}]),
    MainPanel = wxPanel:new(Frame),
    MainSizer = wxBoxSizer:new(?wxHORIZONTAL),
    
    RightPanel = wxPanel:new(MainPanel, [{style, ?wxSUNKEN_BORDER}]),
    RightSizer = wxStaticBoxSizer:new(?wxVERTICAL, RightPanel, [{label, "Couriers Status"}]),
    wxPanel:setSizer(RightPanel, RightSizer),
    
    CenterPanel = wxPanel:new(MainPanel),
    CenterSizer = wxBoxSizer:new(?wxVERTICAL),

    ControlSizer = wxBoxSizer:new(?wxHORIZONTAL),
    wxButton:new(CenterPanel, ?wxID_ANY, [{label, "Reset Simulation"}]),
    wxButton:new(CenterPanel, ?wxID_ANY, [{label, "Pause Order Generator"}]),
    wxButton:new(CenterPanel, ?wxID_ANY, [{label, "Pause Simulation"}]),
    wxSizer:add(CenterSizer, ControlSizer, [{flag, ?wxEXPAND bor ?wxALL}, {border, 5}]),

    MapPanel = wxPanel:new(CenterPanel, [{style, ?wxSUNKEN_BORDER}]),
    wxPanel:setBackgroundColour(MapPanel, {240, 240, 240}),
    wxSizer:add(CenterSizer, MapPanel, [{proportion, 1}, {flag, ?wxEXPAND}]),
    wxPanel:setSizer(CenterPanel, CenterSizer),
    
    LeftPanel = wxPanel:new(MainPanel, [{style, ?wxSUNKEN_BORDER}]),
    LeftSizer = wxStaticBoxSizer:new(?wxVERTICAL, LeftPanel, [{label, "Orders Status"}]),
    wxPanel:setSizer(LeftPanel, LeftSizer),

    wxSizer:add(MainSizer, LeftPanel,  [{proportion, 1}, {flag, ?wxEXPAND}]),
    wxSizer:add(MainSizer, CenterPanel,[{proportion, 3}, {flag, ?wxEXPAND}]),
    wxSizer:add(MainSizer, RightPanel, [{proportion, 1}, {flag, ?wxEXPAND}]),

    wxPanel:setSizer(MainPanel, MainSizer),

    %% *** התיקון כאן ***
    %% שימוש בתחביר הנכון של tuple: {userData, self()}
    wxPanel:connect(MapPanel, paint, [{callback, fun(_Evt, ServerPid) -> handle_paint(ServerPid) end},
                                      {userData, self()}]),
    
    {ok, Timer} = timer:send_interval(?REFRESH_INTERVAL_MS, tick),
    wxFrame:show(Frame),
    State = #state{frame = Frame, map_panel = MapPanel, timer = Timer},
    {ok, State}.

handle_call(get_state, _From, State) ->
    {reply, {ok, State}, State};
handle_call(_Msg, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(tick, State = #state{map_panel = MapPanel}) ->
    NewCouriers = ets:tab2list(courier_locations),
    NewState = State#state{couriers = NewCouriers},
    
    wxPanel:refresh(MapPanel),
    wxWindow:update(MapPanel),
    
    {noreply, NewState};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{timer = Timer}) ->
    timer:cancel(Timer),
    wx:destroy(),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Event Handlers
%%%===================================================================

handle_paint(ServerPid) ->
    {ok, #state{map_panel=MapPanel, couriers=Couriers}} = gen_server:call(ServerPid, get_state),
    DC = wxBufferedPaintDC:new(MapPanel),
    
    {_W, _H} = wxWindow:getSize(MapPanel),
    BGBrush = wxBrush:new(wxWindow:getBackgroundColour(MapPanel)),
    wxDC:setBackground(DC, BGBrush),
    wxDC:clear(DC),
    
    wxDC:setBrush(wxBrush:new({100, 200, 100})),
    wxDC:drawRectangle(DC, {50, 60, 10, 10}),

    [draw_courier(DC, C) || C <- Couriers],
    
    wxBufferedPaintDC:destroy(DC).

draw_courier(DC, {Id, {X, Y}, Status}) ->
    Color = case Status of
        idle -> {0, 150, 0};
        going_to_pickup -> {200, 100, 0};
        delivering -> {0, 0, 200};
        _ -> {100, 100, 100}
    end,
    wxDC:setBrush(wxBrush:new(Color)),
    wxDC:drawCircle(DC, {X, Y, 5}),
    wxDC:drawText(DC, atom_to_list(Id), {X+10, Y-10}).
