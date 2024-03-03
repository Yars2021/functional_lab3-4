-module(math_module).

-export([float_range/3,
         start_linear/2,
         start_lagrange/3,
         start_newton/3]).

% Шаг и 2 точки -> отрезок (1-dim)
float_range(Step, First, Last) when First - Step < Last -> [First | float_range(Step, First + Step, Last)];
float_range(_, _, _) -> [].

% Начало процесса линейной интерполяции
start_linear(OutputPid, Step) ->
    spawn(fun() -> loop_linear(Step, queue:from_list([]), OutputPid) end).

% Шаг и 2 точки -> отрезок (2-dim)
lin_int(Step, [[X1, Y1], [X2, Y2]]) ->
    K = (Y2 - Y1) / (X2 - X1),
    B = Y1 - K * X1,
    XRange = float_range(Step, X1, X2),
    [XRange, lists:map(fun(X) -> K * X + B end, XRange)].

% Обработка точки (линейная)
lin_process(Points, Step, OutputPid) ->
    OutputPid ! {result, {"Linear", lin_int(Step, queue:to_list(Points))}, self()},
    Points.

% Цикл линейной интерполяции
loop_linear(Step, Points, OutputPid) ->
    NewPoints =
        receive
            {point, Point, _} ->
                case queue:len(Points) of
                    2 ->
                        lin_process(queue:drop(queue:in(Point, Points)),
                                    Step,
                                    OutputPid);
                    1 ->
                        lin_process(queue:in(Point, Points), Step, OutputPid);
                    0 ->
                        loop_linear(Step, queue:in(Point, Points), OutputPid)
                end;
            {stop, _} ->
                exit(ok);
            Message ->
                io:format("~p~n", [Message]),
                loop_linear(Step, Points, OutputPid)
        end,
    loop_linear(Step, NewPoints, OutputPid).

% ********************************************************

% Начало процесса интерполяции Лагранжа
start_lagrange(OutputPid, Step, Window) ->
    spawn(fun() -> loop_lagrange(Step, Window, queue:from_list([]), OutputPid) end).

% Формула многочлена Лагранжа
lag_prod(X, Xi, Points) -> lag_prod(X, Xi, Points, 1).

lag_prod(_, _, [], Acc) -> Acc;
lag_prod(X, Xi, [[Xi, _] | Tail], Acc) -> lag_prod(X, Xi, Tail, Acc);
lag_prod(X, Xi, [[Xj, _] | Tail], Acc) -> Acc * ((X - Xj) / (Xi - Xj)) * lag_prod(X, Xi, Tail, Acc).

lag_poly(X, Points) -> lag_poly(X, Points, Points, 0).

lag_poly(_, _, [], Acc) -> Acc;
lag_poly(X, Points, [[Xi, Yi] | Tail], Acc) ->
    Acc + Yi * lag_prod(X, Xi, Points) + lag_poly(X, Points, Tail, Acc).

% Шаг и 2 точки -> отрезок (2-dim)
lag_int(Step, [[X1, Y1] | Tail]) ->
    XRange = float_range(Step, X1, lists:nth(1, lists:last(Tail))),
    [XRange, lists:map(fun(X) -> lag_poly(X, [[X1, Y1] | Tail]) end, XRange)].

% Обработка точки (Лагранж)
lag_process(Points, Step, OutputPid) ->
    OutputPid ! {result, {"Lagrange", lag_int(Step, queue:to_list(Points))}, self()},
    Points.

% Цикл интерполяции Лагранжа
loop_lagrange(Step, Window, Points, OutputPid) ->
    WindowPrev = Window - 1,
    queue:len(Points),
    NewPoints =
        receive
            {point, Point, _} ->
                case queue:len(Points) of
                    Window ->
                        lag_process(queue:drop(queue:in(Point, Points)),
                                    Step,
                                    OutputPid);
                    WindowPrev ->
                        lag_process(queue:in(Point, Points), Step, OutputPid);
                    _ ->
                        loop_lagrange(Step, Window, queue:in(Point, Points), OutputPid)
                end;
            {stop, _} ->
                exit(ok);
            Message ->
                io:format("~p~n", [Message]),
                loop_lagrange(Step, Window, Points, OutputPid)
        end,
    loop_lagrange(Step, Window, NewPoints, OutputPid).

% ********************************************************

% Начало процесса интерполяции Ньютона
start_newton(OutputPid, Step, Window) ->
    spawn(fun() -> loop_newton(Step, Window, queue:from_list([]), OutputPid) end).

% Шаг и 2 точки -> отрезок (2-dim)
new_int(Step, Window, [[X1, Y1], [X2, Y2]]) ->
    K = (Y2 - Y1) / (X2 - X1),
    B = Y1 - K * X1,
    XRange = float_range(Step, X1, X2),
    [XRange, lists:map(fun(X) -> K * X + B end, XRange)].

% Обработка точки (Ньютон)
new_process(Points, Step, Window, OutputPid) ->
    OutputPid ! {result, {"Newton", new_int(Step, Window, queue:to_list(Points))}, self()},
    Points.

% Цикл интерполяции Ньютона
loop_newton(Step, Window, Points, OutputPid) ->
    NewPoints =
        receive
            {point, Point, _} ->
                case queue:len(Points) of
                    2 ->
                        new_process(queue:drop(queue:in(Point, Points)),
                                    Step,
                                    Window,
                                    OutputPid);
                    1 ->
                        new_process(queue:in(Point, Points), Step, Window, OutputPid);
                    0 ->
                        loop_newton(Step, Window, queue:in(Point, Points), OutputPid)
                end;
            {stop, _} ->
                exit(ok);
            Message ->
                io:format("~p~n", [Message]),
                loop_newton(Step, Window, Points, OutputPid)
        end,
    loop_newton(Step, Window, NewPoints, OutputPid).
