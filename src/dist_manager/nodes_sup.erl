-module(nodes_sup).
-behaviour(supervisor).

-export([start_link/0,
         init/1,
         create_node/2,
         create_node_spec/2,
         spawn_workers/2,
         kill_worker/2,
         execute_task/3,
         execute_pack/2,
         get_first/2,
         cut_first/2,
         group_list/2,
         execute_packs/2,
         execute_tasks/3,
         get_min/2,
         list_size/1,
         form_tasks_single/2,
         form_tasks_double/2,
         linearize_list/1,
         execute_reduce/3,
         execute_map/3]).


% Запуск супервизора
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, {}).


% Инициализация супервизора
init({}) ->
    SupervisorSpec =
        #{startegy => one_for_all,
          intensity => 5,
          period => 10
         },
    {ok, {SupervisorSpec, []}}.


% Создание узла
create_node(Supervisor, ID) ->
    NodeSpec =
        #{id => list_to_atom("node_" ++ integer_to_list(ID)), 
           start => {nodes_worker, start_link, []},
           restart => permanent,
           shutdown => 5000,
           type => worker,
           modules => [nodes_worker]
         },
    supervisor:start_child(Supervisor, NodeSpec).


% Создание узла с полной настройкой
create_node_spec(Supervisor, NodeSpec) ->
    supervisor:start_child(Supervisor, NodeSpec).


% Создание N узлов исполнителей
spawn_workers(_, 0) -> [];

spawn_workers(Supervisor, N) ->
    {ok, Pid} = create_node(Supervisor, N),
    [Pid | spawn_workers(Supervisor, N - 1)].


% Завершение исполнителя по Pid
kill_worker(_, []) -> [];

kill_worker(Pid, [Pid | Tail]) ->
    gen_server:stop(Pid),
    Tail;

kill_worker(Pid, [Head | Tail]) ->
    [Head | kill_worker(Pid, Tail)].


% Посылка задачи исполнителю по Pid
execute_task(_, _, []) -> {error, process_does_not_exist};

execute_task(Task, Pid, [Pid | _]) ->
    gen_server:call(Pid, {exec, Task}, 1000);

execute_task(Task, Pid, [_ | Tail]) ->
    execute_task(Task, Pid, Tail).


% Посылка пакета задач исполнителям
execute_pack(Tasks, Pids) ->
    execute_pack(Tasks, Pids, Pids).

execute_pack([], _, _) -> [];

execute_pack(_, [], _) -> [];

execute_pack([Task | TaskTail], [Pid | PidTail], Pids) ->
    [execute_task(Task, Pid, Pids) | execute_pack(TaskTail, PidTail, Pids)].


% Взять первые Size элементов списка
get_first(_, 0) -> [];

get_first([Head | Tail], Size) when Size > 0 ->
    [Head | get_first(Tail, Size - 1)];

get_first(_, _) -> [].


% Взять список без Size первых элементов
cut_first(List, 0) -> List;

cut_first([_ | Tail], Size) when Size > 0 ->
    cut_first(Tail, Size - 1); 

cut_first(List, _) -> List. 


% Деление списка на пакеты размера Size
group_list([], _) -> [];

group_list(List, 0) -> List;

group_list(List, Size) ->
    [get_first(List, Size) | group_list(cut_first(List, Size), Size)].


% Исполнение списка пакетов на кластере
execute_packs([], _) -> [];

execute_packs([Pack | Tail], Pids) ->
    [execute_pack(Pack, Pids) | execute_packs(Tail, Pids)].


% Исполнение списка функций на кластере размера Size (деление на пакеты размера Size + исполнение пакетов)
execute_tasks(Tasks, Pids, Size) when Size > 0 ->
    execute_packs(group_list(Tasks, Size), Pids);

execute_tasks(_, _, _) -> [].


% Минимум из двух
get_min(A, B) when A > B -> B;
get_min(A, _) -> A.


% Длина списка
list_size([]) -> 0;
list_size([_ | Tail]) ->
    1 + list_size(Tail).


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
linearize_list([]) -> [];

linearize_list([[First] | Tail]) ->
    [First | linearize_list(Tail)];

linearize_list([InnerList | Tail]) ->
    InnerList ++ linearize_list(Tail).


% Reduce на кластере. Func({Element, AccIn}) -> AccOut.
execute_reduce(_, [], _) -> 0;

execute_reduce(_, [Result], _) -> Result; 

execute_reduce(Func, List, Pids) ->
    PackSize = get_min(list_size(Pids), list_size(List) / 2 / list_size(Pids)),
    TaskArgs = group_list(List, 2),
    execute_reduce(Func,
                   linearize_list(execute_tasks(form_tasks_double(Func, TaskArgs), Pids, PackSize)), Pids).


% Map на кластере. Func({Element}) -> Result.
execute_map(Func, List, Pids) ->
    PackSize = get_min(list_size(Pids), list_size(List) / list_size(Pids)),
    TaskArgs = group_list(List, 1),
    linearize_list(execute_tasks(form_tasks_single(Func, TaskArgs), Pids, PackSize)).
