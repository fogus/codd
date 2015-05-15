-module(codd).
-export([test/2, is_truthy/1, select_keys/1, project/2]).
-import(maps, [new/0]).
-import(lists, [foldl/3]).
-import(sets, [from_list/1]).

% Courtesy of Steve Vinoski
% 
is_truthy(false) -> false;                      % false
is_truthy(undefined) -> false;                  % undefined
is_truthy([]) -> false;                         % empty list
is_truthy({}) -> false;                         % empty tuple
is_truthy(<<>>) -> false;                       % empty binary
is_truthy(#{}) -> false;                        % empty map
is_truthy(N) when is_number(N) -> N /= 0;
is_truthy(Pid) when is_pid(Pid) -> is_process_alive(Pid);
is_truthy(_) -> true.

test(Name1, Name2) ->
    sets:from_list([#{"person" => Name1}, #{"person" => Name2}]).

select_keys([]) -> fun(V) -> V end;
select_keys({}) -> fun(V) -> V end;
select_keys(Keys) -> 
    fun(Relation) ->
	    lists:foldl(fun(K, Acc) -> 
				case maps:find(K, Relation) of
				    {ok, V} -> maps:put(K, V, Acc);
				    _ -> Acc
				end
			end,
			#{}, 
			Keys)
    end.

project(Table, Keys) ->
    sets:from_list(lists:map(select_keys(Keys), sets:to_list(Table))).


