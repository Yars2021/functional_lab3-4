# Лабораторная работа №3

Цель: получить навыки работы с вводом/выводом, потоковой обработкой данных, командной строкой.

В рамках лабораторной работы вам предлагается повторно реализовать лабораторную работу по предмету "Вычислительная математика" посвящённую аппроксимации (в разные годы это лабораторная работа 3 или 4) со следующими дополнениями:

- обязательно должна быть реализована линейная интерполяция (отрезками, [link](https://en.wikipedia.org/wiki/Linear_interpolation));
- настройки алгоритма аппроксимирования и выводимых данных должны задаваться через аргументы командной строки:
   - какие алгоритмы использовать (в том числе два сразу);
   - частота дискретизации результирующих данных;
   - и т.п.;
- входные данные должны задаваться в текстовом формате на подобии ".csv" (к примеру `x;y\n` или `x\ty\n`) и подаваться на стандартный ввод, входные данные должны быть отсортированы по возрастанию x;
- выходные данные должны подаваться на стандартный вывод;
- программа должна работать в потоковом режиме (пример -- `cat | grep 11`), это значит, что при запуске программы она должна ожидать получения данных на стандартный ввод, и, по мере получения достаточного количества данных, должна выводить рассчитанные точки в стандартный вывод;

Приложение должно быть организовано следующим образом:

```text
    +---------------------------+
    | обработка входного потока |
    +---------------------------+
            |
            | поток / список / последовательность точек
            v
    +------------------------+      +------------------------------+
    | алгоритм аппроксимации |<-----| генератор точек, для которых |
    +------------------------+      | необходимо вычислить         |
            |                       | аппроксимированное значение   |
            |                       +------------------------------+
            |
            | поток / список / последовательность рассчитанных точек
            v
    +------------------------+
    | печать выходных данных |
    +------------------------+
```

Потоковый режим для алгоритмов, работающих с группой точек должен работать следующим образом:

```text
o o o o o o . . x x x
  x x x . . o . . x x x
    x x x . . o . . x x x
      x x x . . o . . x x x
        x x x . . o . . x x x
          x x x . . o . . x x x
            x x x . . o o o o o o EOF
```

где:

- каждая строка -- окно данных, на основании которых производится расчёт алгоритма;
- строки сменяются по мере поступления в систему новых данных (старые данные удаляются из окна, новые -- добавляются);
- `o` -- рассчитанные данные, можно видеть:
   - большинство окон используется для расчёта всего одной точки, так как именно в "центре аппроксимации" результат наиболее точен;
   - первое и последнее окно используются для расчёта большого количества точек, так лучших данных для расчёта у нас не будет.
- `x` -- точки, расчёт которых для "окон" не требуется.

Общие требования:

- программа должна быть реализована в функциональном стиле;
- ввод/вывод должен быть отделён от алгоритмов аппроксимации;
- требуется использовать идиоматичный для технологии стиль программирования.

Содержание отчёта:

- титульный лист;
- требования к разработанному ПО, включая описание алгоритма;
- ключевые элементы реализации с минимальными комментариями;
- ввод/вывод программы;
- выводы (отзыв об использованных приёмах программирования).

Общие рекомендации по реализации. Не стоит писать большие и страшные автоматы, управляющие поведением приложения в целом. Если у вас:

- Язык с ленью -- используйте лень.
- Языки с параллельным программированием и акторами -- используйте их.
- Язык без всей этой прелести -- используйте генераторы/итераторы/и т.п.


---

## Выполнение
Приложение состоит из нескольких модулей:
- io_module содержит функции, ответственные за ввод и вывод
- math_module содержит математические функции
- main содержит обработку аргументов командной строки и отправку сообщений на другие модули

Выбранные методы интерполяции:
- Линейная
- Лагранжа
- Ньютона

Пример работы программы:
Вводятся данные функции sin(x)
![image](https://github.com/Yars2021/functional_lab3-4/assets/79992244/4adf16a5-586e-414f-be6e-1cec29c3d97c)

Основные функции воода-вывода:
```
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
            Name,
            print_number(X),
            print_number(Y);
        Msg ->
            io:format("~p~n", [Msg])
            Msg
    end,
    loop_output().
```

Основные функции интерполяции:
```
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
                % io:format("~p~n", [Message]),
                loop_linear(Step, Points, OutputPid)
        end,
    loop_linear(Step, NewPoints, OutputPid).

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
                % io:format("~p~n", [Message]),
                loop_lagrange(Step, Window, Points, OutputPid)
        end,
    loop_lagrange(Step, Window, NewPoints, OutputPid).

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
                % io:format("~p~n", [Message]),
                loop_newton(Step, Window, Points, OutputPid)
        end,
    loop_newton(Step, Window, NewPoints, OutputPid).
```

Основная функция в main:
```
% Основная функция
main(Args) ->
    ArgsTuple = get_args_tuple(Args),
    Window = element(1, ArgsTuple),
    Step = element(2, ArgsTuple),
    Methods = element(3, ArgsTuple),
    OutputPid = io_module:start_output(),
    link(OutputPid),
    Methods,
    Workers = lists:map(
        fun(Method) ->
            AtomMethod = list_to_atom(Method),
            case AtomMethod of
                linear ->
                    Pid = math_module:start_linear(OutputPid, Step),
                    link(Pid),
                    Pid;
                lagrange ->
                    Pid = math_module:start_lagrange(OutputPid, Step, Window),
                    link(Pid),
                    Pid;
                newton ->
                    Pid = math_module:start_newton(OutputPid, Step, Window),
                    link(Pid),
                    Pid;
                _ ->
                    AtomMethod
            end
        end,
        Methods),
    wait(Workers, io_module:start_input(Workers), OutputPid).
```

## Выводы
- В ходе выполонения работы было создано модульное приложение, производящее интерполяцию по данным, принимаемым из потока ввода.
- Разделение на модули позволило исполнять различные части в разных процессах, обменивающихся сообщениями.
