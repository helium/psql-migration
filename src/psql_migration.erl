-module(psql_migration).

%% API exports
-export([main/1]).

%%====================================================================
%% API functions
%%====================================================================

-define(OPTS,
        [
         {help, $h, "help", undefined, "Print this help text"},
         {dir, $d, "dir", {string, "migrations"}, "Migration folder"},
         {env, $e, "env", {string, ".env"}, "Environment file to search for DATABASE_URL"}
        ]).

%% escript Entry point
main(Args) ->
    handle_command(getopt:parse(?OPTS, Args)).

usage(ExitCode) ->
    getopt:usage(?OPTS, "psql_migration",
                 "<command>",
                 [
                  {"new <name>", "Create a new migration"},
                  {"list", "List migrations indicating which have been applied"},
                  {"run", "Run all migrations"},
                  {"revert", "Revert the last migration"}
                  ]),
    halt(ExitCode).

handle_command({error, Error}) ->
    io:format("~p~n~n", [Error]),
    usage(1);
handle_command({ok, {[help | _], _}}) ->
    usage(0);
handle_command({ok, {Args, ["list"]}}) ->
    Applied = applied_migrations(Args),
    Available = available_migrations(Args),
    IsApplied = fun(Mig) ->
                        case lists:member(Mig, Applied) of
                            true -> "X";
                            _ -> " "
                        end
                end,
    io:format("Migrations:~n"),
    [io:format(" [~s] ~s~n", [IsApplied(Entry), Entry]) || {Entry, _} <- Available];
handle_command({ok, {Args, ["new", Name]}}) ->
    Dir = target_dir(Args),
    Timestamp = erlang:system_time(seconds),
    MigrationName = [integer_to_list(Timestamp), "-", Name, ".sql"],
    Filename = filename:join(Dir, MigrationName),
    C = ["-- ", Filename, "\n",
         "-- :up\n",
         "-- Up migration\n\n",
         "-- :down\n",
         "-- Down migration\n"
        ],
    file:write_file(Filename, list_to_binary(C), [exclusive]),
    io:format("Created migration: ~s~n", [Filename]);
handle_command({ok, {Args, ["run"]}}) ->
    Available = available_migrations(Args),
    case open_connection(Args) of
        {ok, Conn} ->
            Applied = applied_migrations(Conn),
            ToApply = lists:filter(fun ({Mig, _}) -> not lists:member(Mig, Applied) end, Available),
            Results = apply_migrations(up, ToApply, Conn),
            report_migrations(up, Results);
        {error, Error} ->
            io:format("Failed to connect to database: ~p~n", [Error]),
            halt(1)
    end;
handle_command({ok, {Args, ["revert"]}}) ->
    Available = available_migrations(Args),
    case open_connection(Args) of
        {ok, Conn} ->
            case applied_migrations(Conn) of
                [] ->
                    io:format("No applied migrations to revert"),
                    halt(1);
                Applied ->
                    LastApplied = lists:last(Applied),
                    case lists:keyfind(LastApplied, 1, Available) of
                        false ->
                            io:format("Migration ~p can not be found~n", [LastApplied]),
                            halt(1);
                        Migration ->
                            Results = apply_migrations(down, [Migration], Conn),
                            report_migrations(down, Results)
                    end
            end;
        {error, Error} ->
            io:format("Failed to connect to database: ~p~n", [Error]),
            halt(1)
    end;
handle_command({ok, {_, _}}) ->
    usage(1).



%% Utils

open_connection(Args) ->
    envloader:load(dot_env(Args)),
    URL = os:getenv("DATABASE_URL"),
    ParseOpts = [{scheme_defaults, [{postgres, 5432}, {postgresql, 5432}]}],
    case http_uri:parse(URL, ParseOpts) of
        {error, Error} ->
            {error, Error};
        {ok, {_, UserPass, Host, Port, Database, _}} ->
            {User, Pass} = case string:split(UserPass, ":") of
                               [[]] -> {"postgres", ""};
                               [U] -> {U, ""};
                               [[], []] -> {"postgres", ""};
                               [U, P] ->  {U, P}
                           end,
            OpenOpts = #{ port => Port,
                          username => User,
                          password => Pass,
                          host =>Host,
                          database => string:slice(Database, 1)},
            epgsql:connect(OpenOpts)
    end.


target_dir(Args) ->
    case lists:keyfind(dir, 1, Args) of
        false -> ".";
        {dir, Dir} ->
            filelib:ensure_dir(Dir),
            Dir
    end.

dot_env(Args) ->
    case lists:keyfind(env, 1, Args) of
        false -> ".env";
        {env, DotEnv} -> DotEnv
    end.

-spec report_migrations(up | down, [{Version::string(), ok | {error, term()}}]) -> ok.
report_migrations(_, L) when length(L) == 0 ->
    io:format("No migrations were run~n");
report_migrations(up, Results) ->
    [io:format("Applied ~s: ~p~n", [V, R]) || {V, R} <- Results];
report_migrations(down, Results) ->
    [io:format("Reverted ~s: ~p~n", [V, R]) || {V, R} <- Results].

-define(DRIVER, epgsql).

record_migration(up, Conn, V) ->
    ?DRIVER:equery(Conn, "INSERT INTO __migrations (id) VALUES ($1)", [V]);
record_migration(down, Conn, V) ->
    ?DRIVER:equery(Conn, "DELETE FROM __migrations WHERE id = $1", [V]).

apply_migrations(Type, Migrations, Conn) ->
    Results = lists:foldl(fun(_, [{_, {error, _}} | _]=Acc) ->
                                  Acc;
                             (Migration={Version, _}, Acc) ->
                                  case apply_migration(Type, Migration, Conn) of
                                      ok -> [{Version, ok} | Acc];
                                      {error, Error} -> [{Version, {error, Error}}]
                                  end
                          end, [], Migrations),
    lists:reverse(Results).

apply_migration(Type, {Version, Migration}, Conn) ->
    Query = eql:get_query(Type, Migration),
    ?DRIVER:squery(Conn, "BEGIN;"),
    case if_ok(?DRIVER:squery(Conn, Query)) of
        ok ->
            record_migration(Type, Conn, Version),
            ?DRIVER:squery(Conn, "COMMIT;"),
            ok;
        {error, Error} ->
            ?DRIVER:squery(Conn, "ROLLBACK;"),
            {error, Error}
    end.

if_ok(Rs) when is_list(Rs) ->
    Result = lists:map(fun(R) -> if_ok(R) end, Rs),
    case lists:keyfind(error, 1, Result) of
        false -> ok;
        Error -> Error
    end;
if_ok({ok, _}) -> ok;
if_ok({ok, _, _}) -> ok;
if_ok({ok, _, _, _}) -> ok;
if_ok({error, {error,error,_,_,Descr,_}}) -> {error, Descr};
if_ok(Error) -> {error, Error}.

available_migrations(Args) ->
    Dir = target_dir(Args),
    {ok, Files} = file:list_dir(Dir),
    lists:map(fun(Filename) ->
                      {ok, Migs} = eql:compile(filename:join([Dir, Filename])),
                      {filename:rootname(Filename), Migs}
              end, lists:usort(Files)).

applied_migrations(Args) when is_list(Args) ->
    case open_connection(Args) of
        {ok, Conn} ->
            applied_migrations(Conn);
        {error, Error} ->
            {error, Error}
    end;
applied_migrations(Conn) ->
    case ?DRIVER:squery(Conn, "SELECT id FROM __migrations ORDER by id ASC") of
        {ok, _, Migs} -> [binary_to_list(Mig) || {Mig} <- Migs];
        {error, {error, error, <<"42P01">>, _, _, _}} ->
            %% init migrations and restart
            {ok, _, _} = ?DRIVER:squery(Conn,
                                        "CREATE TABLE __migrations ("
                                        "id VARCHAR(255) PRIMARY KEY,"
                                        "datetime TIMESTAMP DEFAULT CURRENT_TIMESTAMP)"),
            applied_migrations(Conn)
    end.
