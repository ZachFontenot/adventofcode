-module(day01).
-export([solutionA/0, solutionB/0]).

read_to_lines() ->
    File = "../inputs/day01.txt",
    file:read_file(File).
    
parse_elves() ->
    {ok, Input_string} = read_to_lines(),
    Elf = binary:split(Input_string, <<"\n\n">>, [global]),
    lists:map(fun (X) -> 
                      Split = binary:split(X, <<"\n">>, [global]),
                      Nums = lists:map(
                        fun (<<>>) -> 0;
                            (E) -> 
                                binary_to_integer(E) end, Split),
                      lists:sum(Nums) end, 
              Elf).
    
solutionA() ->
    Elves = parse_elves(),
    %O(n) for the foldl
    io:format("~p~n", [lists:foldl(fun (Acc, Head) -> max(Acc, Head) end, 0, Elves)]).
solutionB() ->
    Elves = parse_elves(),
    %O(nlogn) for this computation
    Top_elves = lists:sublist(lists:sort(fun (A, B) -> A > B end, Elves), 1, 3),
    io:format("~p~n", [lists:sum(Top_elves)]).
