-module(main).

-export([main/1]).

% Разложение спика аргументов в кортеж
get_args_tuple([]) -> {};
get_args_tuple([Window | [Step | Methods]]) ->
    WindowVal = string:to_integer(Window),
    StepVal = string:to_float(Step),
    {element(1, WindowVal), element(1, StepVal), Methods}.

% Основная функция
main(Args) ->
    {Window, Step, Methods} = get_args_tuple(Args),
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

% Ожидание процессов
wait(Workers, IPid, OPid) ->
    case {erlang:process_info(IPid), erlang:process_info(OPid)} of
        {undefined, undefined} -> exit;
        _ -> wait(Workers, IPid, OPid)
    end.
