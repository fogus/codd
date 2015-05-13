-module(codd).
-export([test/2, is_truthy/1]).
-import(maps, [new/0]).
-import(sets, [from_list/1]).

% Courtesy of Juan Facorro
% 
is_truthy(X) ->
     X =/= false andalso undefined =/= X.

test(Name1, Name2) ->
    sets:from_list([#{"person" => Name1}, #{"person" => Name2}]).

