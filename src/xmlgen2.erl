-module(xmlgen2).
-export([encode/1, encodeData/1, encodeSet/1]).

-define(XML_TOP, "<?xml version=\"1.0\" encoding=\"UTF-8\"?>").

-define(ITEM_B, "<item>").
-define(ITEM_E, "</item>").

-define(DELIM, "\n").


encodeSet(Proplist) ->
    string:join([?XML_TOP, encode({"set", Proplist})], ?DELIM).

encodeData(Proplist) ->
    string:join([?XML_TOP, encode({"data", Proplist})], ?DELIM).

encode({Tag, Proplist})
        when erlang:is_list(Tag) or erlang:is_atom(Tag) ->
    lists:append([
        "<", convert:to_list(Tag), ">",
            encode(Proplist),
        "</", convert:to_list(Tag), ">",
        ?DELIM
    ]);

encode([{Tag, Value} | Rest ])
        when erlang:is_list(Tag) or erlang:is_atom(Tag) ->
    lists:append([
        "<", convert:to_list(Tag), ">",
            encode(Value),
        "</", convert:to_list(Tag), ">",
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
    io:format("# Value {}= ~p~n", [Value]),
    Result = string:join([[encode(Vi)] || Vi <- tuple_to_list(Value)], " "),
    convert:to_list(Result);

encode(Value) -> convert:to_list(Value).
