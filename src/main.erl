#!/usr/bin/env escript

-module(main).

-import(io_module, []).
-import(math_module, []).

-export([]).

% Основная функция
main(Args) -> argparse:run(Args, cli(), #{progname => fp_lab3}).

% Обработка аргументов при помощи argparse
cli() ->
    #{
        arguments =>
            [
                #{name => method,
                    short => $m,
                    action => append,
                    type => {atom, [linear, lagrange, gauss]}
                }
                #{name => step,
                    default => 0.2,
                    type => float,
                    short => $s,
                    help => "Step"
                },
                #{name => window,
                    default => 5,
                    type => float,
                    short => $w,
                    help => "Window size"
                },
            ],
        handler =>
            fun(#{m := Methods, s := Step, w := WindowLen} = Args) ->
                io:format("~p~n", [Args]),
                OutputPid = output:start(),
                link(OutputPid),
                Workers = lists:map(
                    fun(Method) ->
                        case Method of
                            linear -> 
                                Pid = linear:start(OutputPid, Step),
                                link(Pid),
                                Pid;
                            lagrange ->
                                Pid = lagrange:start(OutputPid, Step, WindowLen),
                                link(Pid),
                                Pid;
                            gauss ->
                                Pid = gauss:start(OutputPid, Step, WindowLen),
                                link(Pid),
                                Pid;
                            _ ->
                                io:format("~p~n", [Method])
                        end
                    end,
                    Methods),
                    wait(Workers, input:start(Workers), OutputPid)
            end
    }.

% Ожидание процессов
wait(Workers, IPid, OPid) ->
    case {erlang:process_info(IPid), erlang:process_info(OPid)} of
        {undefined, undefined} -> exit;
        _ -> wait(Workers, IPid, OPid)
    end.
