structure Test =
struct

open QCheck
structure P = Parser
structure A = Absyn

fun p x y = Pos.new (x, y)
val lexer = Lexer.make (Pos.reader Reader.string)
fun parse s = P.make lexer (Pos.stream s)
fun predicate (string, ast) = A.eq (parse string, ast)

val x = Symbol.symbol "x"
val y = Symbol.symbol "y"
val z = Symbol.symbol "z"

fun test _ =
    check (List.getItem, SOME (Show.pair (fn x => x, ShowAbsyn.show)))
          ("parser", pred predicate)
          [
  ("if x then y else z",
   A.IfExp {test = A.VarExp (A.SimpleVar (x, p 3 1)),
            then' = A.VarExp (A.SimpleVar (y, p 10 1)),
            else' = SOME (A.VarExp (A.SimpleVar (z, p 17 1))),
            pos = p 0 1})
          ]
end
