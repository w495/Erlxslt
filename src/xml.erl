-module(xml).
-export([encode/1, encode_set/1, encode_data/1]).

-define(XML_TOP, "<?xml version=\"1.0\" encoding=\"UTF-8\"?>").

-define(ITEM_B, "<item>").
-define(ITEM_E, "</item>").
-define(DELIM, "\n").
-define(SPACE, " ").
-define(OPEN_B, "<").
-define(OPEN_E, "<").
-define(CLOSE_B, "</").
-define(CLOSE_E, "<").

encode_set(Proplist) ->
    string:join([?XML_TOP, encode({"set", Proplist})], ?DELIM).

encode_data(Proplist) ->
    string:join([?XML_TOP, encode({"data", Proplist})], ?DELIM).

encode({Tag, Proplist})
        when erlang:is_list(Tag) or erlang:is_atom(Tag) ->
    lists:append([
        ?OPEN_B, convert:to_list(Tag), ?OPEN_E,
            encode(Proplist),
        ?CLOSE_B, convert:to_list(Tag), ?CLOSE_E,
        ?DELIM
    ]);

encode([{Tag, Value} | Rest ])
        when erlang:is_list(Tag) or erlang:is_atom(Tag) ->
    lists:append([
        ?OPEN_B, convert:to_list(Tag), ?OPEN_E,
            encode(Value),
        ?CLOSE_B, convert:to_list(Tag), ?CLOSE_E,
        ?DELIM,
            encode(Rest),
        ?DELIM
    ]);

encode([ Item | _ ] = Proplist)
        when erlang:is_integer(Item) ->
    Proplist;

encode([ Item | Rest ])  ->
    lists:append([
        ?ITEM_B,
            encode(Item),
        ?ITEM_E,
        ?DELIM,
            encode(Rest),
        ?DELIM
    ]);

encode(Value) when is_tuple(Value) ->
    Result = string:join([[encode(Vi)] || Vi <- tuple_to_list(Value)], ?SPACE),
    convert:to_list(Result);

encode(Value) -> convert:to_list(Value).
