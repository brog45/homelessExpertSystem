:- module(story_dcg_html, [story//1]).

story(L) --> 
    { phrase(story_(L), StoryHtml) },
    [ ul([class(story)], StoryHtml) ].

story_([H|T]) --> step(H), story_(T).
story_([]) --> [].

atom(Object) --> [Object].
time(T) --> {format(atom(A), '~w', [T])}, [A].

step(Step) --> 
    { phrase(step_(Step), StepHtml) },
    [ li(StepHtml) ].

step_(move(A,B)) --> ['Walk from the '], atom(A), [' to the '], atom(B), ['.'].
step_(time(T)) --> ['The time is now '], time(T), ['.'].
