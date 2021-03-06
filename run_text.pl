:- use_module(planner).
:- use_module(story_data).
:- use_module(story_dcg_text).
:- use_module(story_generator).

go :-
    ConfigDict = config{ name: 'Brian', pet: 'Murray', animal: 'cat' },
    init(ConfigDict, State),
    BlockedActions = [],
    generate_story(State, BlockedActions, Story),
    writeln(Story),
    phrase(story(Story), StoryCodes),
    !,
    string_codes(StoryString, StoryCodes),
    writeln(StoryString).

% vim: et ts=4 sw=4 ai
