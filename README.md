# Advent of Code #
So far this is just 2021 and just in Racket. 
Trying to get better at Lispin'

I'm thinking of maybe going back and doing some days in Erlang.
Seems fun.

REALIZING THAT MY ORGANIZATION IS BAD AND I NEED TO CHANGE IT.

Going to move language folders to the top level, so I can define a single project
per lang and have the years nested inside or something.
```erlang
-module(hello).

hello(Msg) ->
  io:fwrite("Funky syntax and all!\n~s\n", [Msg]).
```
