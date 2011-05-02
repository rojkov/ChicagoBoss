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
    {ok, Conn}.

stop(Conn) ->
    riakc_pb_socket:stop(Conn),
    ok.

find(Conn, Id) ->
    {Type, Bucket, Key} = infer_type_from_id(Id),
    {ok, RiakDoc} = riakc_pb_socket:get(Conn, Bucket, Key),
    [Value|_] = riakc_obj:get_values(RiakDoc),
    {struct, Data} = mochijson2:decode(Value),
    apply(Type, new, Data).

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
    Attributes = case Record:id() of
        id ->
            Key = undefined,
            PropList = [{K, V} || {K, V} <- Record:attributes(), is_id_attr(K)];
        DefinedId when is_list(DefinedId) ->
            Key = list_to_binary(DefinedId),
            Record:attributes()
    end,
    Json = iolist_to_binary(mochijson2:encode({struct, Attributes})),
    Object = riakc_obj:new(list_to_binary(Bucket), Key, Json,
                           "application/json"),
    ok = riakc_pb_socket:put(Conn, Object),
    {ok, Record}.

is_id_attr(AttrName) ->
    lists:suffix("_id", atom_to_list(AttrName)).

list_to_proplist([], Acc) -> Acc;
list_to_proplist([K,V|T], Acc) when is_list(K) ->
    list_to_proplist(T, [{list_to_atom(K), V}|Acc]);
list_to_proplist([K,V|T], Acc) when is_atom(K) ->
    list_to_proplist(T, [{K, V}|Acc]).

infer_type_from_id(Id) when is_list(Id) ->
    [Type, BossId] = string:tokens(Id, "-"),
    {list_to_atom(Type), type_to_bucket(Type), BossId}.

% Find bucket name from Boss type
type_to_bucket(Type) ->
    list_to_atom(type_to_bucket_name(Type)).

type_to_bucket_name(Type) when is_atom(Type) ->
    type_to_bucket_name(atom_to_list(Type));
type_to_bucket_name(Type) when is_list(Type) ->
    inflector:pluralize(Type).
