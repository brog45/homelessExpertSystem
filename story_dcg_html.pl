:- module(story_dcg_html, [story//1]).

story(L) --> 
    { phrase(story_(L), StoryHtml) },
    [ ul([class(story)], StoryHtml) ].

story_([H|T]) --> step(H), story_(T).
story_([]) --> [].

atom(Object) --> [Object].
time(T) --> {format(atom(A), '~w', [T])}, [A].
money(N) --> {format(atom(A), '$~2f', [N])}, [A].

step(Step) --> 
    { phrase(step_(Step), StepHtml) },
    [ li(StepHtml) ].

step_(walk(A,B)) --> ['Walk from the '], atom(A), [' to the '], atom(B), ['.'].
step_(bus_to(A,B)) --> ['Take the bus from the '], atom(A), [' to the '], atom(B), ['.'].
step_(time(T)) --> ['The time is now '], time(T), ['.'].
step_(wait(Duration)) --> ['Wait for '], atom(Duration), " hours.".
step_(funds(F)) --> ['You have '], money(F), ['.'].
