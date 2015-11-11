-module(bel1_tests).
-include_lib("eunit/include/eunit.hrl").
-record(fork, {left, right, chars, weight}).
-record(leaf, {char, weight}).

% Creating some trees for testing
createTree0() -> #leaf{char=$d,weight=4}.
createTree1() -> #fork{left=#leaf{char=$a, weight=2},right=#leaf{char=$b, weight=3},chars=[$a,$b],weight=5}.
createTree2() -> #fork{left=#fork{left=#leaf{char=$a, weight=2},right=#leaf{char=$b, weight=3},chars=[$a,$b],weight=5},
						right=#leaf{char=$d, weight=4}, chars=[$a,$b,$d], weight=9}.

% Example Tree from the assignment description
exampleTree() -> #fork{ left = #leaf{char = 65,weight = 8}, right = #fork{left = #fork{
             left = #leaf{char = 66,weight = 3}, right = #fork{ left = #leaf{char = 67,weight = 1},
             right = #leaf{char = 68,weight = 1}, chars = "CD",weight = 2}, chars = "BCD",weight = 5},
             right = #fork{ left = #fork{ left = #leaf{char = 69,weight = 1},right = #leaf{char = 70,weight = 1},
             chars = "EF",weight = 2},right = #fork{ left = #leaf{char = 71,weight = 1}, right = #leaf{char = 72,weight = 1},
             chars = "GH",weight = 2}, chars = "EFGH",weight = 4}, chars = "BCDEFGH",weight = 9}, chars = "ABCDEFGH",weight = 17}.
exampleString()-> "AAAABCDEFGHAAAABB".
				
% tests of the basic functions						
weight_test_()->
	[	{setup, fun createTree0/0, fun(X)->?_assertEqual(4,bel1:weight(X)) end},
		{setup, fun createTree1/0, fun(X)->?_assertEqual(5,bel1:weight(X)) end},
		{setup, fun createTree2/0, fun(X)->?_assertEqual(9,bel1:weight(X)) end}].

char_test_()->
	[	{setup, fun createTree0/0, fun(X)->?_assertEqual([$d],bel1:chars(X)) end},
		{setup, fun createTree1/0, fun(X)->?_assertEqual([$a,$b],bel1:chars(X)) end},
		{setup, fun createTree2/0, fun(X)->?_assertEqual([$a,$b,$d],bel1:chars(X)) end}].

char2_test_()->
	[	{setup, fun createTree0/0, fun(X)->?_assertEqual([$d],bel1:chars(X)) end},
		{setup, fun createTree1/0, fun(X)->?_assertEqual([$a,$b],bel1:chars(X)) end},
		{setup, fun createTree2/0, fun(X)->?_assertEqual([$a,$b,$d],bel1:chars(X)) end}].

mTree_test_()->
		?_assertEqual(createTree2(),bel1:makeCodeTree(createTree1(),createTree0())).

% test for creating Huffman Trees

addLetter_test_() ->
	[	?_assertEqual([{$a,1}], bel1:addLetter([],$a)),
		?_assertEqual([{$a,1}], bel1:addLetter([],$a)),
		?_assertMatch([{_,2},{_,2},{_,2}], bel1:addLetter([{$a,2},{$b,1},{$c,2}],$b))
	].
	
createFrequency_test_()-> 
	?_assertEqual([{$a,2},{$b,3},{$c,2},{$d,1},{$q,1}], lists:sort( fun({X,_},{Y,_})->X<Y end,
								bel1:createFrequencies("abqcabcdb"))).

makeOrderedLeafList_test_()->
	?_assertEqual([#leaf{char=$d,weight=2},#leaf{char=$b,weight=5},#leaf{char=$a,weight=7},#leaf{char=$e,weight=11}],
		bel1:makeOrderedLeafList([{$b,5},{$d,2},{$e,11},{$a,7}])).

combine_test_ignore() ->
	[
		?_assertEqual([],bel1:combine([])),
		?_assertEqual([#leaf{char=$a, weight=2}],bel1:combine([#leaf{char=$a, weight=2}])),
		?_assertEqual([createTree1()],bel1:combine([#leaf{char=$a, weight=2},#leaf{char=$b, weight=3}])),
		?_assertEqual([#leaf{char = 99,weight = 7}, #leaf{char = 100,weight = 8},
						#fork{left = #leaf{char = 97,weight = 5},right = #leaf{char = 98,weight = 6},
						chars = "ab",weight = 11},#leaf{char = 101,weight = 14}],
						bel1:combine([#leaf{char=$a, weight=5},#leaf{char=$b, weight=6},#leaf{char=$c, weight=7},
									#leaf{char=$d, weight=8},#leaf{char=$e, weight=14}])),
		?_assertEqual([#fork{left = #leaf{char = 98,weight = 5},right = #leaf{char = 100,weight = 2},
					chars = "bd",weight = 7},#leaf{char=$a, weight=7},#leaf{char=$e,weight=11}],
					bel1:combine([#leaf{char=$d,weight=2},#leaf{char=$b,weight=5},
					#leaf{char=$a,weight=7},#leaf{char=$e,weight=11}]))
	].

repeatCombine_test_ignore()->
	?_assertEqual({fork, {fork, {leaf,97,7}, {fork,{leaf,98,5},{leaf,100,2},"bd",7}, "abd",14}, {leaf,101,11}, "abde",25},
	bel1:repeatCombine([#leaf{char=$d,weight=2},#leaf{char=$b,weight=5},#leaf{char=$a,weight=7},
		#leaf{char=$e,weight=11}])).

createCodeTree_test_ignore()->
	[
		?_assertEqual( {fork, {fork,{leaf,$a,7},{fork,{leaf, $b,5},{leaf,$d,2},"bd",7},"abd",14},{leaf,$e,11},"abde",25},
			bel1:createCodeTree("eeedeeedeeeeebbbbbaaaaaaa")),
		?_assertEqual(exampleTree(),
			bel1:createCodeTree(exampleString()))
	].

decode_test_()->
	?_assertEqual(
		"BAAHGA",
		bel1:decode(exampleTree(),[1,0,0,0,0,1,1,1,1,1,1,1,0,0])).

encode_decode_test_()->
		?_assertEqual("ADDABHGACDABGHAAAA", bel1:decode(exampleTree(),bel1:encode("ADDABHGACDABGHAAAA",exampleTree()))).

