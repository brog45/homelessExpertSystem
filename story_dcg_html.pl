:- module(story_dcg, [story//1]).

story(L) --> 
    { phrase(story_(L), StoryHtml) },
    [ ul([class(story)], StoryHtml) ].

story_([H|T]) --> step(H), story_(T).
story_([]) --> [].

atom(Object) --> [Object].

step(Step) --> 
    { phrase(step_(Step), StepHtml) },
    [ li(StepHtml) ].

step_(move(A,B)) --> ['Walk from the '], atom(A), [' to the '], atom(B), ['.'].
