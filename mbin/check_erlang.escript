#!/usr/bin/env escript -c
% Dialyzer enhanced flymake checker for Erlang
% Copyright (c) 2011, bkil.hu
% This program is free software and can be distributed under the terms of
% the GNU General Public License v2,
% see COPYING for detailed licensing terms.

-module(check_erlang).
-export([main/1]).
-compile([export_all]).

-define(DEPS, "../../../deps/").
-define(APPS, "../../").

main([FileName]) ->
        parse(FileName).

parse(FileName) ->
    Opts = get_config(),
    correct_modname(FileName),
    Includes = get_includes(),
    BeamDirs = get_beam_dirs(),
    [code:add_patha(D) || D <- BeamDirs],
    Result =
        compile:file(
          FileName,
          [strong_validation, report,
           {warn_format,2}, warn_export_all,
           warn_export_vars, warn_shadow_vars,
           warn_obsolete_guard, warn_unused_import, 
           bin_opt_info, verbose] ++ Includes),
    case Result of
        error ->
            Result;
        _ ->
	    case proplists:get_value(dialyze, Opts) of
		true ->
		    check_dia(FileName);
		false ->
		    halt(0)
	    end
    end.

get_config() ->
    ScriptName = escript:script_name(),
    DirName = filename:dirname(ScriptName),
    ConfigFile = filename:join([DirName, "check_erlang.config"]),
    {ok, Config} = file:consult(ConfigFile),
    Config.
    
check_dia(FileName) ->
    case dia(FileName) of
	C when C==0; C==2 ->
	    halt(0);
	_ ->
	    halt(0)
    end.

get_includes() ->
    try
        [{i, "../include"}, {i, ?APPS}, {i, ?DEPS}] ++ 
         [{i, Path} || Path <- get_apps()] ++
         [{i, Path} || Path <- get_deps()]
    catch _:_ ->
        []
    end.

get_deps() ->
    {ok, Dirs} = file:list_dir(?DEPS),
    [?DEPS ++ D ++ "/include" || D <- Dirs, filelib:is_dir(?DEPS ++ "/" ++ D)].

get_apps() ->
    {ok, Dirs} = file:list_dir(?APPS),
    [?APPS ++ D ++ "/include" || D <- Dirs, filelib:is_dir(?APPS ++ "/" ++ D)].

get_beam_dirs() ->
    {ok, Dirs} = file:list_dir(?DEPS),
    Deps = [?DEPS ++ D ++ "/ebin" || D <- Dirs, filelib:is_dir(?DEPS ++ "/" ++ D)],
    {ok, AppDirs} = file:list_dir(?APPS),
    Apps = [?APPS ++ D ++ "/ebin" || D <- AppDirs, filelib:is_dir(?APPS ++ "/" ++ D)],
    Apps ++ Deps.

get_all_beam_dirs() ->
    {ok, Dirs} = file:list_dir(?DEPS),
    {ok, AppDirs} = file:list_dir(?APPS),
    Deps =
        lists:foldl(fun(D, A) ->
                case filelib:is_dir(?DEPS ++ "/" ++ D) of
                    true -> [" -pa ", ?DEPS ++ D ++ "/ebin " | A];
                    false -> A
                end
            end, [], Dirs),
    Apps =
        lists:foldl(fun(D, A) ->
                case filelib:is_dir(?APPS ++ "/" ++ D) of
                    true -> [" -pa ", ?APPS ++ D ++ "/ebin " | A];
                    false -> A
                end
            end, [], AppDirs),
    Deps ++ Apps.


correct_modname(FileName)->
    NewMod = filename:rootname(filename:basename(FileName)),
    {ok,Binary} = file:read_file(FileName),
    New = re:replace(Binary,
                     "^ *-module *\\([^)]*\\) *\. *$",
                     "-module("++NewMod++").",
                    [multiline]),
    ok = file:write_file(FileName,New).

dia(FileName)->
    PLTName = ".dialyzer_prj.plt",
    Var = case os:type() of
              {win32,_} -> "USERPROFILE"; %% @todo test
              {unix,_}  -> "HOME";
              vxworks   -> "HOME" %% @todo find out
          end,
    OTP_PLT = filename:join(os:getenv(Var),PLTName),
    % # takes a few hours:
    % nice dialyzer --build_plt \
    %  --output_plt ~/.dialyzer_otp.plt \
    %  -r /usr/lib/erlang/lib/*/ebin
    % # takes much less time, but still considerable:
    % nice dialyzer --add_to_plt \
    %  --plt ~/.dialyzer_otp.plt \
    %  --output_plt ~/.dialyzer_prj.plt \
    %  -r /my_project/lib/*/ebin
    % # then you periodically (daily or once in a while) run:
    % nice dialyzer --check_plt \
    %  --plt ~/.dialyzer_prj.plt
    dia_plt(FileName,OTP_PLT).

exec_timeout() ->
    100000.

max_line_length()->
    255.

dia_plt(FileName,_OTP_PLT)->
    io:format("~p ~p dia_plt: '~p' ~n", [?MODULE, ?LINE, dia_plt]),
    AllBeamDirs = get_all_beam_dirs(),
    Args = [FileName, "--quiet", "--no_check_plt", "--src",
            "-I", filename:join("..","include"),
            "-I", filename:join("..","..")] ++
            AllBeamDirs ++ [%%"-Wunmatched_returns", 
			    "-Werror_handling",   %%  "-Wspecdiffs",
			    "-Wno_return", 
			    "-Wrace_conditions", 
			    "-pa" " ../../../deps/*/ebin",
			    "-c", "--plts", "~/.dialyzer_plt", 
			    "~/.project.plt"],
    os_cmd("dialyzer",Args,print_line_filename(FileName)).

print_line_filename(FileName)->
    fun("")->
            ok;
       (OldLine)->
            case splitaround(fun(C)->C/=$:end,4,OldLine) of
                [_,":",Idx,":",[$ |Msg]] ->
                    io:format("~s~n",[[FileName,$:,Idx,": Warning: ",Msg]]);
                _ ->
                    io:format("~s~n",[OldLine])
            end
    end.
                                              
splitaround(F,N,L) when is_function(F,1), is_integer(N), N>=0, is_list(L) ->
    splitaround_(F,fun(X)->not F(X)end,N,L).
splitaround_(_,_,0,L) ->
    [L];
splitaround_(F,NF,N,L) ->
    {A,B} = lists:splitwith(F,L),
    [A | splitaround_(NF,F,N-1,B)].

os_cmd(Cmd,Args,F)-> % os:cmd/1 is buggy in most versions of Erlang
    RelExec = os:find_executable(Cmd), % spawn failed on w1n without this
    CmdLine = string:join([RelExec|Args]," "),
    Opts = [stderr_to_stdout, exit_status, in, hide,
            stream, {line,max_line_length()}],
    Port = open_port({spawn,CmdLine},Opts),
    %R13+: ({spawn_executable,Exec},[{args,Args}|Opts])
    output_data(Port,F,"").

output_data(Port,F,Old) ->
    receive
        {Port,{data,{Eol,New}}} ->
            Aggr = [New|Old],
            case Eol of
                eol ->
                    F(lists:flatten(lists:reverse(Aggr))),
                    output_data(Port,F,[]);
                noeol ->
                    output_data(Port,F,Aggr)
            end;
        {Port,{exit_status,Status}} ->
            Status
    after
        exec_timeout() ->
            io:format("error: timout!~n"),
            127
    end.

%% it would be this simple if option check_plt was available here:
newdia_plt(FileName,OTP_PLT)->
    Warnings =
        dialyzer:run(
          [{analysis_type,succ_typings},
           {check_plt,false}, %UNSUPPORTED!!!
           {warnings,[error_handling, unmatched_returns, specdiffs]},
           {init_plt,OTP_PLT},
           {include_dirs,[filename:join("..","include"),
                          filename:join("..","..")]},
           {from,src_code},
           {files,[FileName]}]),
    [begin
         Line = dialyzer:format_warning(Warning),
         NoEOL = string:strip(Line,right,$\n),
         (print_line_filename(FileName))(NoEOL)
     end || Warning <- Warnings ].
