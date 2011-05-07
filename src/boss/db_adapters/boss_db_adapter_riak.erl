-module(boss_db_adapter_riak).
-behaviour(boss_db_adapter).
-export([start/0, start/1, stop/1, find/2, find/7]).
-export([count/3, counter/2, incr/2, incr/3, delete/2, save_record/2]).

start() ->
    start([]).

start(Options) ->
    Host = proplists:get_value(db_host, Options, "localhost"),
    Port = proplists:get_value(db_port, Options, 8087),
    {ok, Conn} = riakc_pb_socket:start_link(Host, Port),
    _ = application:start(crypto),
    {ok, Conn}.

stop(Conn) ->
    riakc_pb_socket:stop(Conn),
    ok.

find(Conn, Id) ->
    {Type, Bucket, Key} = infer_type_from_id(Id),
    {ok, RiakDoc} = riakc_pb_socket:get(Conn, Bucket, Key),
    [Value|_] = riakc_obj:get_values(RiakDoc),
    Data = binary_to_term(Value),
    DummyRecord = apply(Type, new, lists:seq(1, proplists:get_value(new,
                         Type:module_info(exports)))),
    Record = apply(Type, new, lists:map(fun (AttrName) ->
                    proplists:get_value(AttrName, Data)
            end, DummyRecord:attribute_names())),
    Record:id(Id).

find(Conn, Type, Conditions, Max, Skip, Sort, SortOrder) ->
    {ok, Keys} = riakc_pb_socket:list_keys(Conn, type_to_bucket_name(Type)),
    [find(Conn, Id) || Id <- Keys].

count(Conn, Type, Conditions) ->
    {error, notimplented}.

counter(Conn, Id) ->
    {error, notimplemented}.

incr(Conn, Id) ->
    incr(Conn, Id, 1).
incr(Conn, Id, Count) ->
    {error, notimplemented}.


delete(Conn, Id) ->
    {Type, Bucket, Key} = infer_type_from_id(Id),
    riakc_pb_socket:delete(Conn, Bucket, Key).

save_record(Conn, Record) ->
    Type = element(1, Record),
    Bucket = type_to_bucket_name(Type),
    PropList = [{K, V} || {K, V} <- Record:attributes(), K =/= id],
    Key = case Record:id() of
        id ->
            unique_id_62();
        DefinedId when is_list(DefinedId) ->
            DefinedId
    end,
    Object = riakc_obj:new(list_to_binary(Bucket), list_to_binary(Key),
                           term_to_binary(PropList)),
    case riakc_pb_socket:put(Conn, Object) of
        ok -> {ok, Record:id(atom_to_list(Type) ++ "-" ++ Key)};
        {ok, NewKey} -> {ok, Record:id(atom_to_list(Type) ++ "-" ++ NewKey)}
    end.

is_id_attr(AttrName) ->
    lists:suffix("_id", atom_to_list(AttrName)).

infer_type_from_id(Id) when is_list(Id) ->
    [Type, BossId] = string:tokens(Id, "-"),
    {list_to_atom(Type), type_to_bucket(Type), list_to_binary(BossId)}.

% Find bucket name from Boss type
type_to_bucket(Type) ->
    list_to_binary(type_to_bucket_name(Type)).

type_to_bucket_name(Type) when is_atom(Type) ->
    type_to_bucket_name(atom_to_list(Type));
type_to_bucket_name(Type) when is_list(Type) ->
    inflector:pluralize(Type).

% Unique key generator (copy&pasted from riak_core_util.erl)
% see https://github.com/basho/riak_core/blob/master/src/riak_core_util.erl#L131
% for details.

%% @spec integer_to_list(Integer :: integer(), Base :: integer()) ->
%% string()
%% @doc Convert an integer to its string representation in the given
%% base. Bases 2-62 are supported.
integer_to_list(I, 10) ->
    erlang:integer_to_list(I);
integer_to_list(I, Base)
  when is_integer(I), is_integer(Base),Base >= 2, Base =< 1+$Z-$A+10+1+$z-$a ->
    if I < 0 ->
            [$-|integer_to_list(-I, Base, [])];
       true ->
            integer_to_list(I, Base, [])
    end;
integer_to_list(I, Base) ->
    erlang:error(badarg, [I, Base]).

%% @spec integer_to_list(integer(), integer(), string()) -> string()
integer_to_list(I0, Base, R0) ->
    D = I0 rem Base,
    I1 = I0 div Base,
    R1 = if D >= 36 ->
                [D-36+$a|R0];
            D >= 10 ->
                [D-10+$A|R0];
            true ->
                [D+$0|R0]
         end,
    if I1 =:= 0 ->
            R1;
       true ->
            integer_to_list(I1, Base, R1)
    end.

%% @spec unique_id_62() -> string()
%% @doc Create a random identifying integer, returning its string
%% representation in base 62.
unique_id_62() ->
    Rand = crypto:sha(term_to_binary({make_ref(), now()})),
    <<I:160/integer>> = Rand,
    integer_to_list(I, 62).
