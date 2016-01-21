#!/usr/bin/env escript

-include_lib("kernel/include/file.hrl").

main(_) ->
    {ok, Cwd} = file:get_cwd(),
    Path = lists:reverse(string:tokens(Cwd, "/")),
    Dir =
        case find_dir(Path, [{regular, "rebar.config"}, {regular, "Makefile"}]) of
            false ->
                Cwd;
            Result ->
                get_dir(Result)
        end,
    io:format("Project dir : ~p~n", [Dir]),
    file:set_cwd(Dir),
    os:cmd("vim-erlang-tags.erl"),
    file:set_cwd(Cwd).

find_dir([], _Files) ->
    false;
find_dir(Path, Files) ->
    case is_right_dir(Path, Files) of
        true ->
            Path;
        false ->
            find_dir(tl(Path), Files)
    end.

is_right_dir(_Path, []) ->
    true;
is_right_dir(Path, [{Type, Name} | T]) ->
    case is_file_present(Path, Type, Name) of
        true ->
            is_right_dir(Path, T);
        false ->
            false
    end.

is_file_present(Path, Type, Name) ->
    Dir = get_dir(Path),
    {ok, Files} = file:list_dir(Dir),
    case lists:member(Name, Files) of
        true ->
            check_type(Path, Type, Name);
        false ->
            false
    end.

check_type(Path, Type, Name) ->
    FullName = get_dir(Path) ++ "/" ++ Name,
    case file:read_file_info(FullName) of
        {ok, #file_info{type = Type}} ->
            true;
        _ ->
            false
    end.

get_dir(List) ->
    "/" ++ string:join(lists:reverse(List), "/").
