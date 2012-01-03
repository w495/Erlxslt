-module(xml_seq).
-export([encode_seq/1, encode_set_seq/1, encode_data_seq/1, test/0]).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("../include/common.hrl").

encode_set_seq(Proplist) ->
    string:join([?XML_TOP, encode_seq({"set", Proplist})], ?DELIM).

encode_data_seq(Proplist) ->
    string:join([?XML_TOP, encode_seq({"data", Proplist})], ?DELIM).

encode_seq({Tag, Proplist})
        when erlang:is_list(Tag) or erlang:is_atom(Tag) ->
    lists:append([
        ?OPEN_B, convert:to_list(Tag), ?OPEN_E,
            encode_seq(Proplist),
        ?CLOSE_B, convert:to_list(Tag), ?CLOSE_E,
        ?DELIM
    ]);

encode_seq([{Tag, Value} | Rest ])
        when erlang:is_list(Tag) or erlang:is_atom(Tag) ->
    lists:append([
        ?OPEN_B, convert:to_list(Tag), ?OPEN_E,
            encode_seq(Value),
        ?CLOSE_B, convert:to_list(Tag), ?CLOSE_E,
        ?DELIM,
            encode_seq(Rest),
        ?DELIM
    ]);

encode_seq([ Item | _ ] = Proplist)
        when erlang:is_integer(Item) ->
    Proplist;

encode_seq([ Item | Rest ])  ->
    lists:append([
        ?ITEM_B,
            encode_seq(Item),
        ?ITEM_E,
        ?DELIM,
            encode_seq(Rest),
        ?DELIM
    ]);

encode_seq(Value) when is_tuple(Value) ->
    Result = string:join([[encode_seq(Vi)] || Vi <- tuple_to_list(Value)], ?SPACE),
    convert:to_list(Result);

encode_seq(Value) -> convert:to_list(Value).

-include_lib("eunit/include/eunit.hrl").

test() ->
    ?assertEqual(
        "<data>"
            "some data"
        "</data>",
        encode_seq({data, "some data"})),

    ?assertEqual(
        "<data>"
            "<more>1</more>"
        "</data>",
        encode_seq({data,{more, 1}})),

    ?assertEqual(
        "<data>"
            "<item>a</item>"
            "<item>b</item>"
            "<item>c</item>"
        "</data>",
        encode_seq({data, [a, b, c]})),

    ?assertEqual(
        "<key_a>value_a</key_a>"
        "<key_b>value_b</key_b>",
        encode_seq([
            {key_a, value_a},
            {key_b, value_b}
        ])),

    ?assertEqual(
        "<key_a>value_a</key_a>"
        "<key_b>value_b</key_b>"
        "<item>c</item>",
        encode_seq([
            {key_a, value_a},
            {key_b, value_b},
            c
        ])),

    ?assertEqual(
        "<key_a>"
            "<item>a_1</item>"
            "<item>a_2</item>"
        "</key_a>"
        "<key_b>"
            "<k-b1>v-b1</k-b1>"
            "<k-b2>v-b2</k-b2>"
        "</key_b>"
        "<key_c>"
            "<key_d>"
                "<key e>"
                    "value e"
                "</key e>"
                "<key f>"
                    "value f"
                "</key f>"
            "</key_d>"
        "</key_c>",
        encode_seq([
            {key_a, [a_1, a_2]},
            {key_b, [{'k-b1', 'v-b1'}, {'k-b2', 'v-b2'}]},
            {"key_c", {"key_d", [{"key e", "value e"}, {"key f", "value f"}]}}
        ])),

    ok.
