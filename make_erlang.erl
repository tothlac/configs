#!/usr/bin/env escript
%% -*- erlang -*-
%% -sname make_erlang verbose

-mode(compile).

-define(MAKEFILE_NAME, "erlangmake.erl").
-define(MK_PATH(PATH), filename:join(["/", filename:join(PATH)])).

main([Dir, File]) ->
    io:format("~p ~p Dir: '~p' ~n", [?MODULE, ?LINE, Dir]),
    io:format("~p ~p File: '~p' ~n", [?MODULE, ?LINE, File]),
    case find_project_dir(Dir) of
        {ok, ScriptPath} ->
            execute_makefile(Dir, ScriptPath, File);
        {error, Reason} ->
            io:format("~p ~p Reason: '~p' ~n", [?MODULE, ?LINE, Reason])
    end.

find_project_dir(Dir) ->
    find_project_dir1(string:tokens(Dir, "/")).

find_project_dir1([]) ->
    {error, not_found};
find_project_dir1(Path) ->
    io:format("~p ~p Path: '~p' ~n", [?MODULE, ?LINE, Path]),
    case is_makefile_present(?MK_PATH(Path)) of
        true ->
            {ok, ?MK_PATH(Path)};
        false ->
            find_project_dir1(cut_last_el(Path))
    end.

cut_last_el(L) ->
    lists:reverse(tl(lists:reverse(L))).

is_makefile_present(Dir) ->
    io:format("~p ~p Dir: '~p' ~n", [?MODULE, ?LINE, Dir]),
    {ok, Files} = file:list_dir(Dir),
    lists:member(?MAKEFILE_NAME, Files).

execute_makefile(Path, ScriptPath, File) ->
    io:format("~p ~p File: '~p' ~n", [?MODULE, ?LINE, File]),
    io:format("~p ~p Path: '~p' ~n", [?MODULE, ?LINE, Path]),
    Cmd = ScriptPath ++ "/" ++ ?MAKEFILE_NAME ++ " " ++ Path ++ " " ++ File,
    io:format("~p ~p Cmd: '~p' ~n", [?MODULE, ?LINE, Cmd]),
    Result = os:cmd(Cmd),
    io:format("~p ~p Result: '~p' ~n", [?MODULE, ?LINE, Result]),
    Result.

    


