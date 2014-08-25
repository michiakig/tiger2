structure LexerTests =
struct

open QCheck
structure T = Token
structure L = Lexer

fun ` c l = Pos.new (c, l)

fun tokenize s = Reader.consume (L.make (Pos.reader2 Reader.string)) (Pos.stream s)
fun predicate (s, ts) = ListPair.allEq (Eq.pair (T.eq, Pos.eq)) (tokenize s, ts)
val show = Show.pair (fn x => x, Show.list (Show.pair (T.show, Pos.show)))

fun test _ =
    check (List.getItem, SOME show)
          ("lexer", pred predicate)
          [
  ("if x then y else z",
   [
     (T.If,     ` 0  1)
   , (T.Id "x", ` 3  1)
   , (T.Then,   ` 5  1)
   , (T.Id "y", ` 10 1)
   , (T.Else,   ` 12 1)
   , (T.Id "z", ` 17 1)
  ])
, ("var N := 8",
   [
     (T.Var,    ` 0 1)
   , (T.Id "N", ` 4 1)
   , (T.Assign, ` 6 1)
   , (T.Int 8,  ` 9 1)
  ])
, ("type intArray = array of int",
   [
     (T.Type,          ` 0  1)
   , (T.Id "intArray", ` 5  1)
   , (T.Eq,            ` 14 1)
   , (T.Array,         ` 16 1)
   , (T.Of,            ` 22 1)
   , (T.Id "int",      ` 25 1)
  ])
          ]
end
