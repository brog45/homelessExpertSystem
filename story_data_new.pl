 % story data

:- module(story_data,[init/4, action/2, event/4]).

%! init(+Config:dict, -State:story_state)
%
%   Given some initial information about the user
%   (in a dict) we create the initial state for the planner
%
init(_, State) :-
    State = [
        % things we automatically gather
        time(8.0),
        weather(cold),

        % data from wherever
        player_in(mens_shelter),
        have(tent),
        have(sleeping_bag),
        funds(4.50),
        hungry,  % we have not_hungry, hungry and very_hungry

        % goals
        goal(time(24.0)),
/*        goal(sleep_at_night),
        always(not(cold)),
        always(not(bored)),
        goal(do_laundry),
        goal(morning_meal),
        goal(evening_meal)   */
        goal(visited(elf)),
        goal(visited(laundromat))
            ].

% ! bus(-Dir:atom, -From:atom, -To:atom, -Start:time, -End:time,
%!      -Cost:float, Duration:hours) is nondet
%
%   bus lines
%
%   Dir is one of bidi (both directgions) or one (one way)
%
bus(one, splort, mens_shelter, 16.50, 17.50, 0.0, 0.25).
bus(one, mens_shelter, splort, 08.0, 08.5, 0.0, 0.25).
bus(bidi, From, To, Start, End, 1.0, Dur) :-
    cbus(From, To, Start, End, Dur).

%!  cbus(-From:atom, -To:atom, -Start:time, -End:time,
%!       -Dur:hours) is nondet
%
cbus(splort, library, 07.0, 22.0, 0.5).
cbus(park, library, 9.0, 17.0, 0.5).
cbus(laundromat, library, 9.0, 22.0, 0.25).

%!  walk(-From:atom, -To:atom, -Dur:hours) is nondet
%
%   Walking is always free and can be done at any time
%
walk(elf, park, 0.25).
walk(library, womens_shelter, 0.5).

connected_to(A, B, S, E, Cost, Dur) :- bus(_, A, B, S, E, Cost, Dur).
connected_to(A, B, S, E, Cost, Dur) :- bus(bidi, B, A, S, E, Cost, Dur).
connected_to(A, B, 0.0, 24.0, 0.0, Dur) :- walk(A, B, Dur).
connected_to(A, B, 0.0, 24.0, 0.0, Dur) :- walk(B, A, Dur).

% I think grouping each action with its corresponding
% events makes this easier to read.
:- discontiguous action/2, event/4.

% move from place to place
action(move(CurrentLocation, Location), action{
        prereqs: [player_in(CurrentLocation), funds(Funds), time(T)],
        negprereqs: [],
        removes: [player_in(CurrentLocation), funds(_), time(_)],
        adds: [player_in(Location), funds(NewFunds), time(NewTime)]
    }) :-
    connected_to(CurrentLocation, Location, S, E, Cost, Dur),
    NewTime is T + Dur,
    T >= S,
    NewTime =< E,
    NewFunds is Funds - Cost,
    NewFunds >= 0.0 .

