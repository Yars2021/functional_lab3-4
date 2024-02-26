-module(nodes).

-export([worker_message_handler/0,
         send_to_worker/3,
         recv_from_worker/1,
         send_and_recv_worker/3,
         spawn_workers/1,
         execute_func/2,
         kill_workers/1,
         kill_by_pid/2,
         get_first/2,
         cut_first/2,
         split_funcs/2,
         execute_packs/2,
         execute_funcs/3,
         test_1/0,
         test_2/0,
         test_3/0,
         test_4/0]).

% Структура сообщения исполнителю:
% {Pid исполнителя, Функция от кортежа аргументов, Кортеж аргументов}
% Func при ошибке должна возвращать {exec_error}, а при успехе - {exec_success, Result}

% Структура сообщения менеджеру:
% {Pid исполнителя, {recv_success}} - подтверждение доставки
% {Pid исполнителя, {Статус исполнения, Результат}} - результат исполнения функции 

% ---------------------------------------------------------------------------------------------
%   Функции для узла-исполнителя
% ---------------------------------------------------------------------------------------------

% Перехватчик сообщений, исполняющий функции
worker_message_handler() ->
    receive
        {Sender, Func, Args} ->
            Sender ! {self(), {recv_success}},
            try Sender ! {self(), {exec_success, Func(Args)}}
            catch error:Error -> Sender ! {self(), {exec_error, Error}} end,
            worker_message_handler();
        _ -> {recv_error, format}
    end.

% Отправка сообщения исполнителю
send_to_worker(Pid, Func, Args) -> Pid ! {self(), Func, Args}.

% Прием ответа исполнителя
recv_from_worker(Pid) ->
    receive
        {Pid, {recv_success}} ->
            receive
                {Pid, {exec_error, Error}} -> {exec_error, Error};
                {Pid, {exec_success, Result}} -> Result
            end;
        _ -> {recv_error}
    end.

% Отправка сообщения исполнителю и прием ответа
send_and_recv_worker(Pid, Func, Args) ->
    send_to_worker(Pid, Func, Args),
    recv_from_worker(Pid).

% ---------------------------------------------------------------------------------------------
%   Функции для узла-мастера
% ---------------------------------------------------------------------------------------------

% Создание N процессов-исполнителей и возврат в виде списка
spawn_workers(0) -> [];
spawn_workers(N) when N > 0 ->
    [spawn(nodes, worker_message_handler, []) | spawn_workers(N - 1)];
spawn_workers(_) -> [].

% Отправка функций на исполнение кластеру и обработка ответа (если функций больше, чем процессов, "лишние" функции исполнены не будут)
execute_func([], _) -> [];
execute_func(_, []) -> []; 
execute_func([{Func, Args} | FuncTail], [Pid | PidTail]) ->
    send_to_worker(Pid, Func, Args),
    [recv_from_worker(Pid) | execute_func(FuncTail, PidTail)].

% Остановка всех исполнителей
kill_workers([]) -> [];
kill_workers([Pid | Tail]) ->
    exit(Pid, normal),
    kill_workers(Tail).

% Остановка одного исполнителя по Pid
kill_by_pid(_, []) -> [];
kill_by_pid(Pid, [Pid | Tail]) ->
    exit(Pid, normal),
    Tail;
kill_by_pid(Pid, [Head | Tail]) -> [Head | kill_by_pid(Pid, Tail)].

% Взять первые Size элементов списка
get_first(_, 0) -> [];
get_first([Func | Tail], Size) when Size > 0 -> [Func | get_first(Tail, Size - 1)];
get_first(_, _) -> [].

% Взять список без Size первых элементов
cut_first(List, 0) -> List;
cut_first([_ | Tail], Size) when Size > 0 -> cut_first(Tail, Size - 1); 
cut_first(List, _) -> List. 

% Деление списка функций на пакеты размера Size
split_funcs([], _) -> [];
split_funcs(Funcs, 0) -> Funcs;
split_funcs(Funcs, Size) ->
    [get_first(Funcs, Size) | split_funcs(cut_first(Funcs, Size), Size)].

% Исполнение списка пакетов на кластере
execute_packs([], _) -> [];
execute_packs([Pack | Tail], Pids) -> [execute_func(Pack, Pids) | execute_packs(Tail, Pids)].

% Исполнение списка функций на кластере размера Size (деление на пакеты размера Size + исполнение пакетов)
execute_funcs(Funcs, Pids, Size) when Size > 0 -> execute_packs(split_funcs(Funcs, Size), Pids);
execute_funcs(_, _, _) -> [].

% Экспериментальный тест (4 функции на кластере из 3 узлов, группировка по 3 элемента в пакете)
test_1() ->
    nodes:execute_funcs([{fun({A}) -> -A end, {65}},
                         {fun({A, B, C}) -> A * B * C end, {7, 8, 9}},
                         {fun({A, B}) -> [A | B] end, {"ABCD", [883883]}},
                         {fun({A}) -> A * A * A end, {-10}}],
                         nodes:spawn_workers(3),
                         3).

% Экспериментальный тест (4 функции на кластере из 3 узлов, группировка по 2 элемента в пакете)
test_2() ->
    nodes:execute_funcs([{fun({A}) -> -A end, {65}},
                        {fun({A, B, C}) -> A * B * C end, {7, 8, 9}},
                        {fun({A, B}) -> [A | B] end, {"ABCD", [883883]}},
                        {fun({A}) -> A * A * A end, {-10}}],
                        nodes:spawn_workers(3),
                        2).

% Экспериментальный тест (4 функции на кластере из 3 узлов, группировка по 2 элемента в пакете, некоторые из функций ошибочны)
test_3() ->
    nodes:execute_funcs([{fun({A}) -> -A end, {65}},
                        {fun({A, B, C}) -> A * B * C end, {"ABC", "DDD", 9}},   % Пытаемся умножать строки
                        {fun({A, B}) -> [A | B] end, {"ABCD", [883883]}},
                        {fun({A}) -> A * A * A end, {}}],                       % Не передаем аргументов
                        nodes:spawn_workers(3),
                        2).

% Экспериментальный тест (4 функции на кластере из 3 узлов, группировка по 5 элементов в пакете, что больше, чем количестов исполнителей)
test_4() ->
    nodes:execute_funcs([{fun({A}) -> -A end, {65}},
                        {fun({A, B, C}) -> A * B * C end, {7, 8, 9}},
                        {fun({A, B}) -> [A | B] end, {"ABCD", [883883]}},
                        {fun({A}) -> A * A * A end, {-10}}],
                        nodes:spawn_workers(3),
                        5).
