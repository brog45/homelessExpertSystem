:- module(story_dcg_text, [story//1]).

story([H|T]) --> step(H), "\n", story(T).
story([]) --> [].

atom(Object) --> {atom_codes(Object, Codes)}, Codes.
time(T) --> {format(string(String), '~w', [T])}, String.
money(N) --> {format(string(String), '$~2f', [N])}, String.

step(wait(Duration)) --> "Wait for ", atom(Duration), " hours.".
step(walk(A,B)) --> "Walk from the ", atom(A), " to the ", atom(B), ".".
step(bus_to(A,B)) --> "Take the bus from the ", atom(A), " to the ", atom(B), ".".
step(time(T)) --> "The time is now ", time(T), ".".
step(funds(F)) --> "You have ", money(F), ".".
