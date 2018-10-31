:- module(story_dcg, [story//1]).

story([H|T]) --> step(H), "\n", story(T).
story([]) --> [].

atom(Object) --> {atom_codes(Object, Codes)}, Codes.

step(move(A,B)) --> "Walk from the ", atom(A), " to the ", atom(B), ".".
