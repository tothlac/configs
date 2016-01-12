#!/usr/bin/env escript -c

-module(generate_tags).
-export([main/1]).

-define(ROOT, "../../..").

main(_) ->
    Pwd = file:get_cwd(),
    file:set_cwd(?ROOT),
    Cmd = "find . -name \*.[eh]rl | ~/mbin/erltags -s -f -o tags",
    Res = os:cmd(Cmd),
    io:format("~p ~p Res: '~p' ~n", [?MODULE, ?LINE, Res]),
    file:set_cwd(Pwd).

