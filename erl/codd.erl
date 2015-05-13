-module(codd).
-export([test/1]).
-import(maps, [new/0]).

test(Name) ->
    io:format("Hello, ~s!~n", [Name]).

