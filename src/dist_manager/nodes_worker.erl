-module(nodes_worker).
-behaviour(gen_server).

-export([start_link/0,
         init/1,
         handle_call/3,
         handle_cast/2]).


% Запуск исполнителя
start_link() ->
    gen_server:start_link(?MODULE, {}, []).


% Инициализация узла
init({}) -> {ok, undefined}.


% Вычисление функции
handle_call({exec, {Func, Args}}, _From, State) ->
    try {reply, Func(Args), State}
    catch error:Error -> {reply, {error, Error}, State} end;


% Неизвестное сообщение
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.


% Обработка остальных вызовов
handle_cast(_Msg, State) ->
    {noreply, State}.
