% breadth-first version, avoids infinite recursion

:- module(planner,[calculate_plan/3, apply_action/3]).
:- use_module(story_data).

calculate_plan(InitialState, BlockedActions, Plan) :-
    is_list(BlockedActions),
    format('Calculating plan with blocked actions: ~w~n', [BlockedActions]),
    ord_empty(ClosedSet),
    state_plan_node(InitialState, [], Node),
    process_queue([Node], BlockedActions, ClosedSet, NodeOut),
    !,
    Plan = NodeOut.plan.

state_plan_node(State, Plan, node{ state:State, plan:Plan }).

process_queue(OpenList, _, ClosedSet, _) :-
    % this clause just writes to the debug monitor
    length(OpenList, OpenLen),
    length(ClosedSet, ClosedLen),
    debug(planner(process_queue), 'open ~w closed ~w', [OpenLen, ClosedLen]),
    fail.
process_queue([HeadNode|_], _, _, HeadNode) :-
    state_plan_node(HeadState, _, HeadNode),
    done(HeadState),
    !.
process_queue([HeadNode|TailNodes], BlockedActions, ClosedSet, NodeOut) :-
    findall(Node, take_action(HeadNode, BlockedActions, ClosedSet, Node), OutcomeNodes),
    append(TailNodes, OutcomeNodes, Queue),
    state_plan_node(HeadState, _, HeadNode),
    close_state(ClosedSet, HeadState, ClosedList0),
    process_queue(Queue, BlockedActions, ClosedList0, NodeOut).

done(State) :-
    findall(G, member(goal(G), State), Goals),
    intersection(Goals, State, Goals).

% take an action; check its outcome against the closed list; and add its action term to the plan
take_action(NodeIn, BlockedActions, ClosedSet, NodeOut) :-
    % only proceed if the current node's state and plan are both viable
    state_plan_node(StateIn, PlanIn, NodeIn),
    viable_state(StateIn),
    viable_plan(PlanIn),
    % action/1 is defined in the story data
    action(ActionStep, ActionDict),
    \+ memberchk(ActionStep, BlockedActions),
    apply_action(StateIn, ActionDict, StateOut),
    state_not_closed(StateOut, ClosedSet),
    append(PlanIn, [ActionStep], PlanOut),
    state_plan_node(StateOut, PlanOut, NodeOut).

viable_state(State) :-
    memberchk(funds(Funds), State), 
    Funds >= 0,
    memberchk(time(Time), State), 
    Time =< 24.

viable_plan(Plan) :-
    length(Plan, Length),
    Length < 20.

apply_action(StateIn, ActionDict, StateOut) :-
    subtract(StateIn, ActionDict.negprereqs, StateIn),
    intersection(ActionDict.prereqs, StateIn, ActionDict.prereqs),
    subtract(StateIn, ActionDict.removes, S0),
    append(S0, ActionDict.adds, S1),
    apply_action_cost(S1, ActionDict, S2),
    apply_action_duration(S2, ActionDict, StateOut).

apply_action_cost(StateIn, ActionDict, StateOut) :-
    action{ cost: Cost } :< ActionDict,
    memberchk(funds(Funds), StateIn),
    !,
    Cost =< Funds,
    RemainingFunds is Funds - Cost,
    select(funds(Funds), StateIn, funds(RemainingFunds), StateOut).
apply_action_cost(_,_,_).

apply_action_duration(StateIn, ActionDict, StateOut) :-
    action{ duration: Duration, openTime: OpenTime, closeTime: CloseTime } :< ActionDict,
    memberchk(time(TimeIn), StateIn),
    !,
    TimeOut is TimeIn + Duration,
    (   action{ openTime: OpenTime } :< ActionDict
    ->  TimeIn >= OpenTime
    ;   true
    ),
    (   action{ closeTime: CloseTime } :< ActionDict
    ->  TimeOut =< CloseTime
    ;   true
    ),
    select(time(TimeIn), StateIn, time(TimeOut), StateOut).
apply_action_duration(_,_,_).

state_not_closed(State, ClosedSet) :-
    list_to_ord_set(State, StateOrdSet),
    \+ ord_memberchk(StateOrdSet, ClosedSet).

close_state(ClosedSetIn, State, ClosedSetOut) :-
    list_to_ord_set(State, StateOrdSet),
    ord_add_element(ClosedSetIn, StateOrdSet, ClosedSetOut).
