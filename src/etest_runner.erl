-module (etest_runner).
-compile (export_all).


% Macro printing the given message to stderr.
-define(stderr(Msg, Args),
    io:put_chars(standard_error, io_lib:format(Msg, Args))).

-define(stderr(Msg), ?stderr(Msg, [])).

-define(cred,   "\e[31m").
-define(cgreen, "\e[32m").
-define(cclean, "\e[0m").


% The runner will be called without arguments in case no tests were found .
% Print a descriptive error message, then exit.
run_all() ->
    ?stderr("etest: No tests found~n"),
    erlang:halt().


run_all(Modules) ->
    % Init statistics.
    [put(K, 0) || K <- [errors, tests]],
    % Init stacktrace store
    put(stacktrace, []),

    StartAt = erlang:now(),
    lists:foreach(fun run/1, Modules),
    TimeDiff = timer:now_diff(erlang:now(), StartAt) / 1000000,

    print_errors(),

    Color = case get(errors) of
        0 -> ?cgreen;
        _ -> ?cred
    end,

    io:format("~n~nFinished in ~.2f seconds~n", [TimeDiff]),
    io:format("~s~p examples, ~p failures~s~n", [Color, get(tests), get(errors), ?cclean]),

    erlang:halt(get(errors)).


run(Module) ->
    Funs = testfuns(Module),
    FunsWithCallbacks = apply_callbacks(Module, Funs),

    BeforeSuite = maybe_fun(Module, before_suite),
    AfterSuite = maybe_fun(Module, after_suite),

    ToRun = lists:flatten([BeforeSuite, FunsWithCallbacks, AfterSuite]),
    TryTest = fun (Test) ->
        try
            Test(),
            io:format("~s.~s", [?cgreen, ?cclean])
        catch
            _:Error ->
                io:format("~sF~s", [?cred, ?cclean]),
                inc(errors),
                CleanTrace = clean_trace(erlang:get_stacktrace()),
                append_to_list(stacktrace, {Error, CleanTrace})
        end
    end,
    lists:foreach(TryTest, ToRun).



testfuns(Module) ->
    Exports = try
        Module:module_info(exports)
    catch
        _:_ ->
            ?stderr("etest: ~p: No such module~n", [Module]),
            erlang:halt(1)
    end,

    IsFocus = fun({FunName, _}) ->
        nomatch =/= re:run(atom_to_list(FunName), "^focus_test_")
    end,

    TestFuns = case lists:filter(IsFocus, Exports) of
        [] ->
            IsTest = fun({FunName, _}) ->
                nomatch =/= re:run(atom_to_list(FunName), "^test_")
            end,
            lists:filter(IsTest, Exports);
        FocusTests -> FocusTests
    end,

    MakeApplicative = fun({FunName, _}) ->
        fun() ->
            inc(tests),
            Module:FunName()
        end
    end,
    lists:map(MakeApplicative, shuffle_list(TestFuns)).


% slow shuffling list
shuffle_list(L) ->
    random:seed(now()),
    [X || {_, X} <- lists:sort([{random:uniform(), N} || N <- L])].


print_errors() ->
    io:format("~n"),
    Stacktraces = lists:reverse(get(stacktrace)),
    print_errors(Stacktraces, 1).

print_errors([], _CurrentTest) -> ok;
print_errors([H | Stacktraces], CurrentTest) ->
    print_error(H, CurrentTest), print_errors(Stacktraces, CurrentTest + 1).

print_error({Error, Stacktrace}, CurrentTest) ->
    ErrorDescription = case Error of
        {Assertion, Info} ->
            {Assertion, maps:from_list(Info)};
        _ ->
            Stacktrace
    end,
    io:format("~n"),
    io:format("  ~p) ~s", [CurrentTest, prettify_error(ErrorDescription)]),
    io:format("~n").


prettify_error({Assertion, #{module := Module, line := Line} = Error}) ->
    io_lib:format("~s.erl:~p~n~s", [Module, Line, ?cred]) ++
    prettify_error_assertion(Assertion, Error) ++
    io_lib:format("~s", [?cclean]);
prettify_error(Stacktrace) ->
    io_lib:format("~p", Stacktrace).

prettify_error_assertion(assert_match, #{pattern := P, value := V}) ->
    io_lib:format("      Pattern: ~p~n", [P]) ++
    io_lib:format("          Got: ~p",   [V]);
prettify_error_assertion(assert_not_match, #{pattern := P, value := V}) ->
    io_lib:format("      Pattern: ~p~n", [P]) ++
    io_lib:format("          Got: ~p",   [V]);
prettify_error_assertion(assert_exception, #{expression := E, unexpected_success := S}) ->
    io_lib:format("    Exception: ~p~n", [E]) ++
    io_lib:format(" Success with: ~p",   [S]);
prettify_error_assertion(assert_exception, #{expression := E, unexpected_exception := Ex}) ->
    io_lib:format("    Exception: ~p~n", [E]) ++
    io_lib:format("  Except with: ~p",   [Ex]);
prettify_error_assertion(assert_not_exception, #{expression := E, unexpected_exception := Ex}) ->
    io_lib:format("    Exception: ~p~n", [E]) ++
    io_lib:format("  Except with: ~p",   [Ex]);
prettify_error_assertion(_, #{expected := E, value := V}) ->
    io_lib:format("     Expected: ~p~n", [E]) ++
    io_lib:format("          Got: ~p",   [V]);
prettify_error_assertion(_, V) ->
    io_lib:format("   ~p~n", [maps:to_list(V)]).


apply_callbacks(Module, Funs) ->
    Before = maybe_fun(Module, before_test),
    After = maybe_fun(Module, after_test),
    [
        fun() ->
            Before(),
            try Fun() after After() end
        end
        || Fun <- Funs
    ].


maybe_fun(Module, FunName) ->
    case has_fun(Module, FunName) of
        true  -> fun() -> Module:FunName() end;
        false -> fun() -> ok end
    end.


has_fun(Module, FunName) ->
    Exports = Module:module_info(exports),
    proplists:is_defined(FunName, Exports).


clean_trace(Trace0) ->
    % Remove the lower etest stack.
    {_ETestTrace, TraceR} = lists:split(5, lists:reverse(Trace0)),
    lists:reverse(TraceR).

append_to_list(Name, Value) ->
    put(Name, [Value | get(Name)]).

inc(Name) ->
    put(Name, get(Name) + 1).
