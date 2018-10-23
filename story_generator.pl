:- module(executor,[generate_story/3]).
:- use_module(story_data).
:- use_module(planner).

generate_story(State, BlockedActions, StoryOut) :-
    calculate_plan(State, BlockedActions, Plan),
    apply_plan(State, Plan, BlockedActions, StoryOut).

apply_plan(State, Plan, BlockedActions, _Story) :-
    % this clause just writes to the debug monitor
    debug(story_generator(apply_plan, state), 'State: ~w', [State]),
    debug(story_generator(apply_plan, plan), 'Plan: ~w', [Plan]),
    debug(story_generator(apply_plan, blocked_actions), 'Blocked Actions: ~w', [BlockedActions]),
    fail.
apply_plan(_, [], _, []).
apply_plan(State, [Action|_], BlockedActions, TailOut) :-
    reject_action(Action),
    !,
    calculate_plan(State, [Action|BlockedActions], NewPlan),
    apply_plan(State, NewPlan, [Action|BlockedActions], TailOut).
apply_plan(State, [Action|T], BlockedActions, [Action|TailOut]) :-
    action(Action, ActionDict), 
    apply_action(State, ActionDict, NewState),
    apply_plan(NewState, T, BlockedActions, TailOut).

% reject_action(wash_hands(bathroom)).
% reject_action(_) :- !, fail.
reject_action(Action) :-
    format('Take Action: ~w? [y/n]~n', [Action]),
    repeat,
    get_char(Resp),
    memberchk(Resp, [y,n]),
    !,
    Resp = n.
