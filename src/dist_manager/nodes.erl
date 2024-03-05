-module(nodes).

-export([worker_message_handler/0,
         send_to_worker/3,
         recv_from_worker/1,
         send_and_recv_worker/3,
         spawn_workers/1,
         execute_task/2,
         kill_workers/1,
         kill_by_pid/2,
         get_first/2,
         cut_first/2,
         group_list/2,
         execute_packs/2,
         execute_tasks/3,
         get_min/2,
         list_size/1,
         form_tasks_single/2,
         form_tasks_double/2,
         normalize_list/1,
         execute_reduce/3,
         execute_map/3]).

% Структура сообщения исполнителю:
% {Pid исполнителя, Функция от кортежа аргументов, Кортеж аргументов}
% Task при ошибке должна возвращать {exec_error}, а при успехе - {exec_success, Result}

% Структура сообщения менеджеру:
% {Pid исполнителя, {recv_success}} - подтверждение доставки
% {Pid исполнителя, {Статус исполнения, Результат}} - результат исполнения функции

% ---------------------------------------------------------------------------------------------
%   Функции для узла-исполнителя
% ---------------------------------------------------------------------------------------------

% Перехватчик сообщений, исполняющий функции. После таймаута посылает особый сигнал
worker_message_handler() ->
    receive
        {Sender, Task, Args} ->
            Sender ! {self(), {recv_success}},
            try Sender ! {self(), {exec_success, Task(Args)}}
            catch error:Error -> Sender ! {self(), {exec_error, Error}} end,
            worker_message_handler();
        _ -> {recv_error, format}
    after
        50 -> {recv_timeout}
    end.

% Отправка сообщения исполнителю
send_to_worker(Pid, Task, Args) -> Pid ! {self(), Task, Args}.

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
send_and_recv_worker(Pid, Task, Args) ->
    send_to_worker(Pid, Task, Args),
    recv_from_worker(Pid).

% ---------------------------------------------------------------------------------------------
%   Функции для узла-мастера
% ---------------------------------------------------------------------------------------------

% Создание N процессов-исполнителей и возврат в виде списка
spawn_workers(0) -> [];
spawn_workers(N) when N > 0 ->
    [spawn(nodes, worker_message_handler, []) | spawn_workers(N - 1)];
spawn_workers(_) -> [].

% Отправка функций на исполнение кластеру и обработка ответа
% (если функций больше, чем процессов, "лишние" функции исполнены не будут)
execute_task([], _) -> [];
execute_task(_, []) -> [];
execute_task([{Task, Args} | FuncTail], [Pid | PidTail]) ->
    send_to_worker(Pid, Task, Args),
    [recv_from_worker(Pid) | execute_task(FuncTail, PidTail)].

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
get_first([Task | Tail], Size) when Size > 0 -> [Task | get_first(Tail, Size - 1)];
get_first(_, _) -> [].

% Взять список без Size первых элементов
cut_first(List, 0) -> List;
cut_first([_ | Tail], Size) when Size > 0 -> cut_first(Tail, Size - 1);
cut_first(List, _) -> List.

% Деление списка функций на пакеты размера Size
group_list([], _) -> [];
group_list(Tasks, 0) -> Tasks;
group_list(Tasks, Size) ->
    [get_first(Tasks, Size) | group_list(cut_first(Tasks, Size), Size)].

% Исполнение списка пакетов на кластере
execute_packs([], _) -> [];
execute_packs([Pack | Tail], Pids) ->
    [execute_task(Pack, Pids) | execute_packs(Tail, Pids)].

% Исполнение списка функций на кластере размера Size
% (деление на пакеты размера Size + исполнение пакетов)
execute_tasks(Tasks, Pids, Size) when Size > 0 -> execute_packs(group_list(Tasks, Size), Pids);
execute_tasks(_, _, _) -> [].

% Минимум из двух
get_min(A, B) when A > B -> B;
get_min(A, _) -> A.

% Длина списка
list_size([]) -> 0;
list_size([_ | Tail]) -> 1 + list_size(Tail).

% Формирование задач для исполнителей из списка элементов
form_tasks_single(_, []) -> [];
form_tasks_single(Func, [[Element] | Tail]) ->
    [{Func, {Element}} | form_tasks_single(Func, Tail)].

% Формирование задач для исполнителей из списка пар элементов
form_tasks_double(_, []) -> [];
form_tasks_double(Func, [[First] | Tail]) ->
    [{fun({A}) -> A end, {First}} | form_tasks_double(Func, Tail)];
form_tasks_double(Func, [[First | [Second]] | Tail]) ->
    [{Func, {First, Second}} | form_tasks_double(Func, Tail)].

% Формирование обычного списка из вложенного
normalize_list([]) -> [];
normalize_list([[First] | Tail]) -> [First | normalize_list(Tail)];
normalize_list([InnerList | Tail]) -> InnerList ++ normalize_list(Tail).

% Reduce на кластере. Func({Element, AccIn}) -> AccOut.
execute_reduce(_, [], _) -> 0;
execute_reduce(_, [Result], _) -> Result;
execute_reduce(Func, List, Pids) ->
    PackSize = get_min(list_size(Pids), list_size(List) / 2 / list_size(Pids)),
    TaskArgs = group_list(List, 2),
    execute_reduce(Func,
        normalize_list(
            execute_tasks(form_tasks_double(Func, TaskArgs), Pids, PackSize)), Pids).

% Map на кластере. Func({Element}) -> Result.
execute_map(Func, List, Pids) ->
    PackSize = get_min(list_size(Pids), list_size(List) / list_size(Pids)),
    TaskArgs = group_list(List, 1),
    normalize_list(execute_tasks(form_tasks_single(Func, TaskArgs), Pids, PackSize)).
