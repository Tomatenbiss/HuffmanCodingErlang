-module(bel1).
-compile(export_all).
%-export([encode/2, decode/2, createCodeTree/1]).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Ein Huffman Code wird durch einen Binaermaum repraesentiert.
%
%  Jedes Blatt beinhaltet ein Zeichen, das durch den Baum kodiert wird.
%  Das Gewicht entspricht der Haeufigkeit des Vorkommens eines Zeichen innerhalb
%  eines Texts.
%
%  Die inneren Knoten repraesentieren die Kodierung. Die assoziierten Zeichen
%  weisen auf die darunter liegenden Blaetter. Das Gewicht entspricht der Summe
%  aller Zeichen, die darunter liegen.
%
%
% Definition of the Tree: two kinds of nodes:
% fork - representing the inner nodes (binary tree)
% leaf - representing the leafs of the tree
%
-type tree():: fork() | leaf().

-record(fork, {left::tree(), right::tree(), chars::list(char()),
                                            weight::non_neg_integer()}).
-type fork() :: #fork{}.
-record(leaf, {char::char(), weight::non_neg_integer()}).
-type leaf() :: #leaf{}.
-type bit() :: 0 | 1.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Basisfunktionen
-spec weight(tree()) -> non_neg_integer().
weight(#fork{weight=W}) -> W;
weight(#leaf{weight=W}) -> W.

-spec chars(tree()) -> list(char()).
chars(#fork{chars=C}) -> C;
chars(#leaf{char=C}) -> [C].

% Erzeugung eines CodeTrees aus zwei Teilbaeumen
% Aus Gruenden der Testbarkeit werden links die Teilbaeume mit dem alphabetisch
% kleinerem Wert der Zeichenketten angeordnet.
-spec makeCodeTree( T1::tree(), T2::tree()) -> tree().
makeCodeTree(T1 , T2) -> case (chars(T1) < chars(T2)) of
  true -> #fork{left=T1, right=T2, chars=chars(T1)++chars(T2),
    weight=weight(T1)+weight(T2)};
  false -> #fork{left=T2, right=T1, chars=chars(T2)++chars(T1),
    weight=weight(T1)+weight(T2)}
end.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%   Erzeugung eines Huffman Trees
%
%   Schreiben Sie eine Funktion createFrequencies, die aus einem Text die
%   Haeufigkeiten des Vorkommens eines Zeichen in der Zeichenkette berechnet.
%
%  Ergebnis der Funktion soll eine Liste von Zweiertupeln sein, die als erstes
%  Element den Character und als zweites die Haeufigkeit enthaelt.
%
%  createFrequencies("Dies ist ein Test") waere also [{$D,1}, {$i,3}, {$e,3},
%  {$s, 3}, {$ , 3}, {$t, 2}, {$n, 1}, {$T,1}]
%
%  Auf die Elemente eines Tupels kann ueber Pattern Matching zugegriffen werden:
%  z.B. {X,Y} = {$a,4}
%  Tipp: Splitten Sie die Funktion auf:
%  1. Funktion zum Eingliedern des Buchstabens in die
%     Tupelliste (z.B. addLetter(...))
%  2. Aufruf der Funktion fuer jeden Buchstaben

-spec addLetter(list({char(),non_neg_integer()}), char())-> list({char(), non_neg_integer()}).
addLetter(TupelList, Char) -> case TupelList of %3 möglich
  [] -> [{Char, 1}]++[];
  [{X,Y}|XS] when X == Char -> lists:flatten([{X, Y+1}|XS]);
  [{X,Y}|XS] when Char < X -> lists:flatten([{Char, 1}]++TupelList);
  [[]] -> lists:flatten(TupelList++[{Char, 1}]);
  [{X,Y}|XS] -> lists:flatten([[{X,Y}]++addLetter(XS, Char)])
end.


-spec createFrequencies(list(char())) -> list({char(), non_neg_integer()}).
createFrequencies(Text) -> case Text of
  [] -> [];
  [X|XS] -> addLetter(createFrequencies(XS), X)
end.


%  Erzeugung eines Blattknotens fuer jeden Buchstaben in der Liste
%  Aufsteigendes Sortieren der Blattknoten nach den Haeufigkeiten der Vorkommen der Buchstaben
%  z.B. aus makeOrderedLeafList([{$b,5},{$d,2},{$e,11},{$a,7}])
% wird [#leaf{char=$d,weight=2},#leaf{char=$b,weight=5},#leaf{char=$a,weight=7},#leaf{char=$e,weight=11}]
-spec makeOrderedLeafList(FreqList::list({char(), non_neg_integer()})) -> list(leaf()).
makeOrderedLeafList(FreqList) -> case FreqList of
  [] -> [];
  [{X1, X2}, {Y1,Y2}|YS] when X2 > Y2 ->
    [#leaf{char=Y1, weight=Y2}]++makeOrderedLeafList([{X1,X2}|YS]);
  [{X1, X2}, {Y1,Y2}|YS] ->
    [#leaf{char=X1, weight=X2}]++makeOrderedLeafList([{Y1,Y2}|YS]);
  [{X1, X2}|[]] -> [#leaf{char=X1, weight=X2}]
end.


%  Bei jedem Aufruf von combine sollen immer zwei Teilbaeume (egal ob fork oder leaf) zusammenfuegt werden.
%  Der Parameter der Funktion combine ist eine aufsteigend sortierte Liste von Knoten.
%
%  Die Funktion soll die ersten beiden Elemente der Liste nehmen, die Baeume zusammenfuegen
%  und den neuen Knoten wieder in die Liste einfuegen sowie die zusammengefuegten aus der Liste
%  loeschen. Dabei sollen der neue Knoten so eingefuegt werden, dass wieder eine sortierte Liste von
%  Knoten entsteht.
%
%  Ergebnis der Funktion soll wiederum eine sortierte Liste von Knoten sein.
%
%  Hat die Funktion weniger als zwei Elemente, so soll die Liste unveraendert bleiben.
%  Achtung: Ob die vorgefertigten Tests funktionieren, haengt davon ab, auf welcher Seite die Knoten
%  eingefuegt werden. Die Tests sind genau dann erfolgreich, wenn Sie die Baeume so kombinieren, dass
%  ein Baum entsteht, der so angeordnet ist, wie im Beispiel auf dem Aufgabenblatt. Sorgen Sie dafuer,
%  dass die Teilbaeume ebenso eingefuegt werden (erhoehter Schwierigkeitsgrad) oder schreiben Sie eigene
%  Tests.

sortedInsert([],T) -> [T];
sortedInsert([X|XS],T) -> case weight(X) < weight(T) of
  true -> [X|sortedInsert(XS,T)];
  false -> [T|[X|XS]]
  end.

-spec combine(list(tree())) -> list(tree()).
%% combine([TreeList]) -> toBeDefined.
combine([]) -> [];
combine([T1,T2|TS]) -> sortedInsert(TS,makeCodeTree(T1,T2));
combine(TS) -> TS.


%  Die Funktion repeatCombine soll die Funktion combine so lange aufrufen, bis nur noch ein Gesamtbaum uebrig ist.
-spec repeatCombine(TreeList::list(tree())) -> tree().
repeatCombine([L|[]]) ->L;
repeatCombine(TreeList)-> repeatCombine(combine(TreeList)).

%  createCodeTree fuegt die einzelnen Teilfunktionen zusammen. Soll aus einem gegebenen Text, den Gesamtbaum erzeugen.
-spec createCodeTree(Text::list(char())) -> tree().
createCodeTree(Text)-> repeatCombine(makeOrderedLeafList(createFrequencies(Text))).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Dekodieren einer Bitsequenz
%
% Die Funktion decode soll eine Liste von Bits mit einem gegebenen Huffman Code (CodeTree) dekodieren.
% Ergebnis soll die Zeichenkette im Klartext sein.

-spec decode(CodeTree::tree(), list( bit())) -> list(char()).
decode(CodeTree, []) -> [];
decode(CodeTree, BitList) -> decodeChar(CodeTree, CodeTree, BitList).

-spec decodeChar(CodeTree::tree(), CodeTree::tree(), list(bit())) -> list(char()).

decodeChar(CodeTree, #leaf{char=C},Y) ->
  [C|decodeChar(CodeTree, CodeTree, Y)];
decodeChar(CodeTree, #fork{left=Left,chars=C}, [0|YS]) ->
  decodeChar(CodeTree, Left, YS);
decodeChar(CodeTree, #fork{right=Right,chars=C}, [1|YS]) ->
  decodeChar(CodeTree, Right, YS);
decodeChar(_,_,_) -> [].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Kodieren einer Bitsequenz
%
%  Die Funktion encode soll eine Liste von Bits mit einem gegebenen Huffman Code (CodeTree) kodieren.
%  Ergebnis soll die Bitsequenz sein.
%
%  Gehen Sie dabei folgendermassen vor:
%  Schreiben Sie eine Funktion convert, die aus einem Codetree eine Tabelle generiert, die fuer jeden
%  Buchstaben die jeweilige Bitsequenz bereitstellt. Dabei soll jeder Eintrag ein Tupel sein bestehend
%  aus dem Character und der Bitsequenz.
%  Also: convert(CodeTree)->[{Char,BitSeq},...]
-spec convert(CodeTree::tree()) -> list({char(), list(bit())}).
  convert(CodeTree) -> convertTree([], CodeTree).

-spec convertTree(list({list(bit())}), tree()) -> list({char(), list(bit())}).
convertTree(BitSeq, #leaf{char=C}) -> [{C, BitSeq}];
convertTree(BitSeq, #fork{left=Left,right=Right,chars=C}) ->
  convertTree(append(BitSeq, 0), Left)++convertTree(append(BitSeq, 1), Right).

-spec append(list(bit()), bit()) -> list(bit()).
append([], Y) -> [Y];
append([X|XS], Y) -> [X|append(XS, Y)].

%  Schreiben Sie eine Funktion encode, die aus einem Text und einem CodeTree die entsprechende
%  Bitsequenz generiert.
%  Verwenden Sie dabei die erzeugte Tabelle.
-spec encode(Text::list(char()), CodeTree::tree()) -> list(bit()).
encode(Text, CodeTree) -> toBeDefined.


createTestTree() -> #fork{ left = #leaf{char = 65,weight = 8}, right = #fork{left = #fork{
             left = #leaf{char = 66,weight = 3}, right = #fork{ left = #leaf{char = 67,weight = 1},
             right = #leaf{char = 68,weight = 1}, chars = "CD",weight = 2}, chars = "BCD",weight = 5},
             right = #fork{ left = #fork{ left = #leaf{char = 69,weight = 1},right = #leaf{char = 70,weight = 1},
             chars = "EF",weight = 2},right = #fork{ left = #leaf{char = 71,weight = 1}, right = #leaf{char = 72,weight = 1},
             chars = "GH",weight = 2}, chars = "EFGH",weight = 4}, chars = "BCDEFGH",weight = 9}, chars = "ABCDEFGH",weight = 17}.
