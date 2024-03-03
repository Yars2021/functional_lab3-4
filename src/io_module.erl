-module(io_module).

-export([start_input/1, start_output/0]).
-export([loop_input/1, loop_output/0]).
-export([parse_point/1, print_number/1]).

% Начало ввода
start_input(Workers) -> spawn(io_module, loop_input, [Workers]).

% Начало вывода
start_output() -> spawn(io_module, loop_output, []).

% Цикл ввода
loop_input(Workers) ->
    case io:get_line("") of
        eof ->
            [Pid ! {stop, nil, self()} || Pid <- Workers];
        Line ->
            [Pid ! {point, parse_point(Line), self()} || Pid <- Workers]
    end,
    loop_input(Workers).

% Цикл вывода
loop_output() ->
    receive
        {result, {Name, [X, Y]}, _} ->
            io:format("~p~n", [Name]),
            print_number(X),
            print_number(Y);
        Msg ->
            io:format("~p~n", [Msg])
    end,
    loop_output().

% Обработка точки
parse_point(Line) ->
    [case string:to_float(Num) of
        {error, no_float} -> list_to_integer(Num);
        {Float, _} -> Float
    end || Num <- string:tokens(string:trim(Line), " ")].

% Вывод чисел
print_number(Nums) ->
    io:format("~s~n", [lists:join("\t", 
                       lists:map(fun(N) -> erlang:float_to_list(float(N), [{decimals, 2}]) end,
                       Nums))]).
