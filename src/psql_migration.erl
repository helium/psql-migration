-module(psql_migration).

%% API exports
-export([main/1, open_connection/1, connection_opts/1]).

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
                  {"revert", "Revert the last migration"},
                  {"reset", "Resets your database by dropping the database in your DATABASE_URL and then runs `setup`"},
                  {"setup", "Creates the database specified in your DATABASE_URL, and runs any existing migrations."}
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
    handle_command_result({ok, "Created migration: ~s~n", [Filename]});
handle_command({ok, {Args, ["run"]}}) ->
    Available = available_migrations(Args),
    Result = with_connection(Args,
                             fun(Conn) ->
                                     Applied = applied_migrations(Conn),
                                     ToApply = lists:filter(fun ({Mig, _}) -> not lists:member(Mig, Applied) end, Available),
                                     Results = apply_migrations(up, ToApply, Conn),
                                     report_migrations(up, Results)
                             end),
    handle_command_result(Result);
handle_command({ok, {Args, ["revert"]}}) ->
    Available = available_migrations(Args),
    Result = with_connection(Args,
                             fun(Conn) ->
                                     case applied_migrations(Conn) of
                                         [] ->
                                             {error, "No applied migrations to revert"};
                                         Applied ->
                                             LastApplied = lists:last(Applied),
                                             case lists:keyfind(LastApplied, 1, Available) of
                                                 false ->
                                                     {error, "Migration ~p can not be found~n", [LastApplied]};
                                                 Migration ->
                                                     Results = apply_migrations(down, [Migration], Conn),
                                                     report_migrations(down, Results)
                                             end
                                     end
                             end),
    handle_command_result(Result);
handle_command({ok, {Args, ["reset"]}}) ->
    {ok, Opts} = connection_opts(Args),
    case maps:take(database, Opts) of
        error ->
            handle_command_result({error, "No database to reset~n"});
        {Database, Opts1} ->
            case with_connection(Opts1#{database => "postgres"},
                                 fun(Conn) ->
                                         if_ok(epgsql:squery(Conn, "drop database if exists " ++ Database))
                                 end) of
                ok ->
                    handle_command({ok, {Args, ["setup"]}});
                Other ->
                    handle_command_result(Other)
            end
    end;
handle_command({ok, {Args, ["setup"]}}) ->
    {ok, Opts} = connection_opts(Args),
    case maps:take(database, Opts) of
        error ->
            handle_command_result({error, "No database to reset~n"});
        {Database, Opts1} ->
            case with_connection(Opts1#{database => "postgres"},
                                 fun(Conn) ->
                                         if_ok(epgsql:squery(Conn, "create database " ++ Database))
                                 end) of
                ok ->
                    handle_command({ok, {Args, ["run"]}});
                Other ->
                    handle_command_result(Other)
            end
    end;
handle_command({ok, {_, _}}) ->
    usage(1).



%% Utils


-type command_result() ::ok |
                         {ok, io:format(), [term()]} |
                         {error, string()} |
                         {error, io:format(), [term()]}.

-spec handle_command_result(command_result()) -> no_return().
handle_command_result(ok) ->
    halt(0);
handle_command_result({ok, Fmt, Args}) ->
    io:format(Fmt, Args),
    halt(0);
handle_command_result({error, Str}) ->
    handle_command_result({error, Str, []});
handle_command_result({error, Fmt, Args}) ->
    io:format(Fmt, Args),
    halt(1).


-spec with_connection(list() | map(), fun((epgsql:connnection()) -> command_result())) -> command_result().
with_connection(Args, Fun) ->
    case open_connection(Args) of
        {ok, Conn} ->
            Fun(Conn);
        {error, Error} ->
            {error, "Failed to connect to database: ~p~n", [Error]}
    end.

connection_opts(Args) ->
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
            {ok, #{ port => Port,
                    username => User,
                    password => Pass,
                    host =>Host,
                    database => string:slice(Database, 1)}}
    end.


-spec open_connection(list() | map()) -> {ok, epgsql:connection()} | {error, term()}.
open_connection(Args) when is_list(Args) ->
    {ok, Opts} = connection_opts(Args),
    open_connection(Opts);
open_connection(Opts) ->
    epgsql:connect(Opts).

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
    [io:format("Applied ~s: ~p~n", [V, R]) || {V, R} <- Results],
    ok;
report_migrations(down, Results) ->
    [io:format("Reverted ~s: ~p~n", [V, R]) || {V, R} <- Results],
    ok.

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
if_ok({error, {error,error,_,_,Descr,_}}) -> {error, binary_to_list(Descr)};
if_ok(Error) -> {error, Error}.

available_migrations(Args) ->
    Dir = target_dir(Args),
    Files = filelib:wildcard(filename:join(Dir, "*.sql")),
    lists:map(fun(Filename) ->
                      {ok, Migs} = eql:compile(filename:join([Dir, Filename])),
                      {filename:rootname(Filename), Migs}
              end, lists:usort(Files)).

applied_migrations(Args) when is_list(Args) ->
    with_connection(Args,
                    fun(Conn) ->
                            applied_migrations(Conn)
                    end);
applied_migrations(Conn) when is_pid(Conn) ->
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
