 % story data

:- module(story_data,[init/2, action/2]).

%! init(+Config:dict, -State:story_state)
%
%   Given some initial information about the user
%   (in a dict) we create the initial state for the planner
%
init(_, State) :-
    State = [
        % things we automatically gather
          time(8.0)
        , weather(cold)

        % data from wherever
        , player_in(mens_shelter)
        , have(tent)
        , have(sleeping_bag)
        , funds(4.50)
        , hungry  % we have not_hungry, hungry and very_hungry

        % goals
        %, goal(time(24.0)),
        %, goal(sleep_at_night),
        %, always(not(cold)),
        %, always(not(bored)),
        %, goal(do_laundry),
        %, goal(morning_meal),
        %, goal(evening_meal)
        , goal(visited(elf))
        , goal(visited(laundromat))
        , goal(player_in(mens_shelter))
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
walk(library, mens_shelter, 0.5).

walkable(A, B, Duration) :- walk(A, B, Duration).
walkable(A, B, Duration) :- walk(B, A, Duration).
walkable(A, B, Duration) :- 
    busable(A, B, _, _, _, BusDuration),
    Duration is 2.5 * BusDuration.

busable(A, B, S, E, Cost, Dur) :- bus(_, A, B, S, E, Cost, Dur).
busable(A, B, S, E, Cost, Dur) :- bus(bidi, B, A, S, E, Cost, Dur).

% walk
action(walk(CurrentLocation, Location), action{
        prereqs: [player_in(CurrentLocation)],
        negprereqs: [],
        removes: [player_in(CurrentLocation), visited(Location)],
        adds: [player_in(Location), visited(Location)],
        duration: Duration
    }) :-
    walkable(CurrentLocation, Location, Duration).

% take the bus
action(bus_to(CurrentLocation, Location), action{
        prereqs: [player_in(CurrentLocation)],
        negprereqs: [],
        removes: [player_in(CurrentLocation), visited(Location)],
        adds: [player_in(Location), visited(Location)],
        duration: Duration,
        cost: Cost,
        openTime: StartTime,
        closeTime: EndTime
    }) :-
    busable(CurrentLocation, Location, StartTime, EndTime, Cost, Duration).
