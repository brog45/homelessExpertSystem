:- module(story_dcg_text, [story//1]).

story([H|T]) --> step(H), "\n", story(T).
story([]) --> [].

atom(Object) --> {atom_codes(Object, Codes)}, Codes.
time(T) --> {format(string(String), '~w', [T])}, String.

step(wait(Duration)) --> "Wait for ", atom(Duration), " hours.".
step(move(A,B)) --> "Walk from the ", atom(A), " to the ", atom(B), ".".
step(time(T)) --> "The time is now ", time(T), ".".
