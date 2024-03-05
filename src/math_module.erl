-module(math_module).

-export([float_range/3,
         start_linear/2,
         start_lagrange/3,
         start_newton/3]).

% Шаг и 2 точки -> отрезок (1-dim)
float_range(Step, First, Last) when First - Step < Last ->
    [First | float_range(Step, First + Step, Last)];
float_range(_, _, _) -> [].

% Получение координаты точки
coord(x, [X, _]) -> X;
coord(y, [_, Y]) -> Y.

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
                Message,
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

% Шаг и точки -> отрезок (2-dim)
lag_int(Step, [[X1, Y1] | Tail]) ->
    XRange = float_range(Step, X1, coord(x, lists:last(Tail))),
    [XRange, lists:map(fun(X) -> lag_poly(X, [[X1, Y1] | Tail]) end, XRange)].

% Обработка точки (Лагранж)
lag_process(Points, Step, OutputPid) ->
    OutputPid ! {result, {"Lagrange", lag_int(Step, queue:to_list(Points))}, self()},
    Points.

% Цикл интерполяции Лагранжа
loop_lagrange(Step, Window, Points, OutputPid) ->
    WindowPrev = Window - 1,
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
                Message,
                loop_lagrange(Step, Window, Points, OutputPid)
        end,
    loop_lagrange(Step, Window, NewPoints, OutputPid).

% ********************************************************

% Начало процесса интерполяции Ньютона
start_newton(OutputPid, Step, Window) ->
    spawn(fun() -> loop_newton(Step, Window, queue:from_list([]), OutputPid) end).

% Разделенные разности f(x0, x1, ... xk)
diff(_, [], _) -> 0;
diff(Index, Points, 0) -> coord(y, lists:nth(Index, Points));
diff(Index, Points, Order) ->
    (diff(Index + 1, Points, Order - 1) - diff(Index, Points, Order - 1)) /
    (coord(x, lists:nth(Index + Order, Points)) - coord(x, lists:nth(Index, Points))).

% Произведенгие (x - xi) при i от 1 до K (индексация с 1)
new_prod(X, K, Points) -> new_prod(X, K, 1, Points, 1).

new_prod(X, K, K, Points, Acc) -> Acc * (X - coord(x, lists:nth(K, Points)));
new_prod(X, K, Curr, Points, Acc) ->
    new_prod(X, K, Curr + 1, Points, Acc * (X - coord(x, lists:nth(Curr, Points)))).

% Формула многочлена Ньютона
new_poly(X, Len, Points) -> new_poly(X, Len, Points, 1, 0).

new_poly(_, 0, Points, _, Acc) -> Acc + diff(1, Points, 0);
new_poly(X, Len, Points, K, Acc) ->
    new_poly(X, Len - 1, Points, K + 1, Acc + diff(1, Points, K) * new_prod(X, K, Points)).

% Шаг и точки -> отрезок (2-dim)
new_int(Window, Step, [[X1, Y1] | Tail]) ->
    XRange = float_range(Step, X1, coord(x, lists:last(Tail))),
    [XRange, lists:map(fun(X) -> new_poly(X, Window - 1, [[X1, Y1] | Tail]) end, XRange)].

% Обработка точки (Ньютон)
new_process(Points, Window, Step, OutputPid) ->
    OutputPid ! {result, {"Newton", new_int(Window, Step, queue:to_list(Points))}, self()},
    Points.

% Цикл интерполяции Ньютона
loop_newton(Step, Window, Points, OutputPid) ->
    WindowPrev = Window - 1,
    NewPoints =
        receive
            {point, Point, _} ->
                case queue:len(Points) of
                    Window ->
                        new_process(queue:drop(queue:in(Point, Points)),
                                    Window,
                                    Step,
                                    OutputPid);
                    WindowPrev ->
                        new_process(queue:in(Point, Points), Window, Step, OutputPid);
                    _ ->
                        loop_newton(Step, Window, queue:in(Point, Points), OutputPid)
                end;
            {stop, _} ->
                exit(ok);
            Message ->
                Message,
                loop_newton(Step, Window, Points, OutputPid)
        end,
    loop_newton(Step, Window, NewPoints, OutputPid).
