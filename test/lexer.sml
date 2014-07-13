structure LexerTests =
struct

open QCheck
structure T = Token
structure L = Lexer

fun tokenize s = Reader.consume (L.make (Pos.reader Reader.string)) (Pos.stream s)
fun predicate (string, tokens) = ListPair.allEq T.eq (tokenize string, tokens)

fun test _ =
    check (List.getItem, SOME (Show.pair (fn x => x, Show.list T.show)))
          ("lexer", pred predicate)
          [
  ("if x then y else z", [T.If, T.Id "x", T.Then, T.Id "y", T.Else, T.Id "z"])
, ("var N := 8", [T.Var, T.Id "N", T.Assign, T.Int 8])
, ("type intArray = array of int", [T.Type, T.Id "intArray", T.Eq, T.Array, T.Of, T.Id "int"])
          ]
end
