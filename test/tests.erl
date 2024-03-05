-module(tests).

-include_lib("eunit/include/eunit.hrl").

% Тесты распределенного вычислителя

% Экспериментальный тест (4 функции на кластере из 3 узлов, группировка по 3 элемента в пакете)
test1() ->
    nodes:execute_tasks([{fun({A}) -> -A end, {65}},
                         {fun({A, B, C}) -> A * B * C end, {7, 8, 9}},
                         {fun({A, B}) -> [A | B] end, {"ABCD", [883883]}},
                         {fun({A}) -> A * A * A end, {-10}}],
                         nodes:spawn_workers(3),
                         3).

% Экспериментальный тест (4 функции на кластере из 3 узлов, группировка по 2 элемента в пакете)
test2() ->
    nodes:execute_tasks([{fun({A}) -> -A end, {65}},
                        {fun({A, B, C}) -> A * B * C end, {7, 8, 9}},
                        {fun({A, B}) -> [A | B] end, {"ABCD", [883883]}},
                        {fun({A}) -> A * A * A end, {-10}}],
                        nodes:spawn_workers(3),
                        2).

% Экспериментальный тест
% (4 функции на кластере из 3 узлов, группировка по 2 элемента в пакете,
% некоторые из функций ошибочны)
test3() ->
    nodes:execute_tasks([{fun({A}) -> -A end, {65}},
                        {fun({A, B, C}) -> A * B * C end, {"ABC", "DDD", 9}},
                        {fun({A, B}) -> [A | B] end, {"ABCD", [883883]}},
                        {fun({A}) -> A * A * A end, {}}],
                        nodes:spawn_workers(3),
                        2).

% Экспериментальный тест
% (4 функции на кластере из 3 узлов, группировка по 5 элементов в пакете,
% что больше, чем количестов исполнителей)
test4() ->
    nodes:execute_tasks([{fun({A}) -> -A end, {65}},
                        {fun({A, B, C}) -> A * B * C end, {7, 8, 9}},
                        {fun({A, B}) -> [A | B] end, {"ABCD", [883883]}},
                        {fun({A}) -> A * A * A end, {-10}}],
                        nodes:spawn_workers(3),
                        5).
