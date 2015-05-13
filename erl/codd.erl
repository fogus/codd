-module(codd).
-export([test/2]).
-import(maps, [new/0]).
-import(sets, [from_list/1]).

test(Name1, Name2) ->
    sets:from_list([#{"person" => Name1}, #{"person" => Name2}]).

