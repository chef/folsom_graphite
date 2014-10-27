-module(folsom_graphite_sup_tests).

-include_lib("eunit/include/eunit.hrl").

maybe_start_worker_no_config_test() ->
    application:set_env(folsom_graphite, included_applications, []),
    {ok, {{one_for_one, _, _}, ChildSpecs}} = folsom_graphite_sup:init([]),
    ?assertEqual([], ChildSpecs).

maybe_start_worker_with_config_test() ->
    application:set_env(folsom_graphite, foo, bar),
    {ok, {{one_for_one, _, _}, ChildSpecs}} = folsom_graphite_sup:init([]),
    ?assertMatch([_,_], ChildSpecs),
    application:unset_env(folsom_graphite, foo).

start_application_no_config_test() ->
    application:unset_env(folsom_graphite, included_applications),
    application:unset_env(folsom_graphite, foo),
    application:start(folsom),
    ?assertMatch(ok, application:start(folsom_graphite)),
    application:stop(folsom_graphite).

start_application_config_test() ->
    try
        FakePid = spawn(fun() -> receive _ -> ok end end),
        application:set_env(folsom_graphite, foo, bar),
        meck:new([folsom_graphite_sender, folsom_graphite_worker]),
        meck:expect(folsom_graphite_sender, start_link, 2, {ok, FakePid}),
        meck:expect(folsom_graphite_worker, start_link, 3, {ok, FakePid}),
        application:start(folsom),
        ?assertMatch(ok, application:start(folsom_graphite)),
        ?assertEqual(2, length(supervisor:which_children(folsom_graphite_sup))),
        application:stop(folsom_graphite)
    after
        meck:unload([folsom_graphite_sender, folsom_graphite_worker]),
        application:unset_env(folsom_graphite, included_applications)
    end.
