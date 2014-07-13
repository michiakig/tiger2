structure ParserTests =
struct

open QCheck
structure P = Parser
structure A = Absyn

fun p x y = Pos.new (x, y)
val lexer = Lexer.make (Pos.reader Reader.string)
fun parse s = P.make lexer (Pos.stream s)

(* compare AST nodes. ignores fields of type 'a ref *)
fun eq (A.ForExp {var, escape=_, lo, hi, body, pos},
        A.ForExp {var=v, escape=_, lo=l, hi=h, body=b, pos=p}) =
    var = v andalso lo = l andalso hi = h andalso body = b andalso pos = p
  | eq (a, b) = a = b

fun predicate (string, expected) = 
    let
       val (SOME (actual, _)) = parse string
    in
       eq (actual, expected)
    end

val x = Symbol.symbol "x"
val y = Symbol.symbol "y"
val z = Symbol.symbol "z"
val i = Symbol.symbol "i"

fun test _ =
    check (List.getItem, SOME (Show.pair (fn x => x, ShowAbsyn.show)))
          ("parser", pred predicate)
          [
  ("if x then y else z",
   A.IfExp {test = A.VarExp (A.SimpleVar (x, p 3 1)),
            then' = A.VarExp (A.SimpleVar (y, p 10 1)),
            else' = SOME (A.VarExp (A.SimpleVar (z, p 17 1))),
            pos = p 0 1})

, ("x", A.VarExp (A.SimpleVar (x, p 0 1)))

, ("nil", A.NilExp)

, ("while x do y",
   A.WhileExp {test = A.VarExp (A.SimpleVar (x, p 6 1)),
               body = A.VarExp (A.SimpleVar (y, p 11 1)),
               pos = p 0 1})

, ("for i := x to y do z",
   A.ForExp {var    = i,
             escape = ref true,
             lo     = A.VarExp (A.SimpleVar (x, p 9 1)),
             hi     = A.VarExp (A.SimpleVar (y, p 14 1)),
             body   = A.VarExp (A.SimpleVar (z, p 19 1)),
             pos    = p 0 1})
          ]
end
