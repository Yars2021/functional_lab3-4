-module(nodes).

-export([worker_message_handler/0,
         send_to_worker/3,
         recv_from_worker/1,
         send_and_recv_worker/3,
         spawn_workers/1,
         find_free_workers/1,
         set_status/3,
         clear_workers/1,
         execute_func/2]).

% Структура сообщения:
% {Pid исполнителя, Функция от кортежа аргументов, Кортеж аргументов}
% Func при ошибке должна возвращать {exec_error}, а при успехе - {exec_success, Result}

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
    [{free, spawn(nodes, worker_message_handler, [])} | spawn_workers(N - 1)];
spawn_workers(_) -> [].

% Поиск свободных исполнителей
find_free_workers([]) -> [];
find_free_workers([{busy, _} | Tail]) -> find_free_workers(Tail);
find_free_workers([{free, Pid} | Tail]) -> [Pid | find_free_workers(Tail)].

% Установка статуса исполнителя
set_status(_, _, []) -> [];
set_status(Pid, Status, [{_, Pid} | Tail]) -> [{Status, Pid} | Tail];
set_status(Pid, Status, [Head | Tail]) -> [Head | set_status(Pid, Status, Tail)].

% Завершение всех свободных процессов
clear_workers([]) -> [];
clear_workers([{busy, Pid} | Tail]) -> [{busy, Pid} | clear_workers(Tail)];
clear_workers([{free, Pid} | Tail]) ->
    exit(Pid, normal),
    clear_workers(Tail).

% Отправка функции на исполнение кластеру и обработка ответа
execute_func({Func, Args}, FreePid) -> 0.
