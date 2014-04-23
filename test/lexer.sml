structure Test =
struct

open QCheck
structure T = Token
structure L = Lexer

fun p x y = Pos.new (x, y)
val z = Pos.zero
fun tokenize s = Reader.consume (L.make (Pos.reader Reader.string)) (Pos.stream s)
fun predicate (string, tokens) = ListPair.allEq T.eq (tokenize string, tokens)

fun test _ =
    check (List.getItem, SOME (Show.pair (fn x => x, Show.list T.show)))
          ("lexer", pred predicate)
          [
  ("if x then y else z",
   [T.If z, T.Id ("x", p 3 1), T.Then (p 5 1), T.Id ("y", p 10 1), T.Else (p 12 1), T.Id ("z", p 17 1)])

, ("var N := 8",
   [T.Var z, T.Id ("N", p 4 1), T.Assign (p 6 1), T.Int (8, p 9 1)])

, ("type intArray = array of int",
   [T.Type z, T.Id ("intArray", p 5 1), T.Eq (p 14 1), T.Array (p 16 1), T.Of (p 22 1), T.Id ("int", p 25 1)])
          ]
end
