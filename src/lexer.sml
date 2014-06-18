signature LEXER =
sig

   exception Error of Pos.t * string

   val make: (char, 'a * Pos.t) Reader.t -> (Token.t, 'a * Pos.t) Reader.t

end

structure Lexer =
struct

structure T = Token

exception Error of Pos.t * string

fun compilerBug msg s = raise Error (Pos.getPos s, "compiler bug! " ^ msg)

(* Pair utilities *)
fun fst (a, _) = a
fun snd (_, b) = b

(* Drop leading whitespace from a stream *)
fun skipWS rdr = Reader.dropWhile rdr Char.isSpace

fun isDigit (x, _) = Char.isDigit x
fun isPunct (x, _) = Char.isPunct x
fun isValid x = Char.isAlphaNum x orelse x = #"_"

structure Key = struct
   type ord_key = string
   val compare = String.compare
end
structure Map : ORD_MAP = BinaryMapFn(Key)

(* fold a list of keys, vals into a new map *)
fun collect l =
    List.foldl (fn ((k, v), acc) => Map.insert (acc, k, v)) Map.empty l

(* maps from raw token values to Token.t ctors *)

val symbols =
    collect
       [
         ("(",  T.LParen)
       , (")",  T.RParen)
       , ("[",  T.LBracket)
       , ("]",  T.RBracket)
       , ("{",  T.LBrace)
       , ("}",  T.RBrace)
       , (".",  T.Dot)
       , (",",  T.Comma)
       , (";",  T.Semicolon)
       , ("*",  T.Times)
       , ("/",  T.Divide)
       , ("+",  T.Plus)
       , ("-",  T.Minus)
       , ("=",  T.Eq)
       , (">",  T.Gt)
       , ("<",  T.Lt)
       , ("&",  T.And)
       , ("|",  T.Or)
       , (":",  T.Comma)
       , (":=", T.Assign)
       , ("<>", T.Neq)
       , (">=", T.Geq)
       , ("<=", T.Leq)
       , ("<",  T.Lt)
       , (">",  T.Gt)
       ]

val keywords =
    collect
       [
         ("array",    T.Array)
       , ("break",    T.Break)
       , ("do",       T.Do)
       , ("else",     T.Else)
       , ("end",      T.End)
       , ("for",      T.For)
       , ("function", T.Function)
       , ("if",       T.If)
       , ("in",       T.In)
       , ("let",      T.Let)
       , ("nil",      T.Nil)
       , ("of",       T.Of)
       , ("then",     T.Then)
       , ("to",       T.To)
       , ("type",     T.Type)
       , ("var",      T.Var)
       , ("while",    T.While)
       ]

(* extract chars from a stream that match a predicate *)
fun get rdr isValid s =
    let
       val (chars, s') = Reader.takeWhile rdr isValid s
    in
       if length chars < 1 then
          NONE
       else
          SOME (String.implode chars, s')
    end

(* extract an integer literal from a positional stream *)
fun getInt rdr s =
    case get rdr Char.isDigit s of
        NONE => NONE
      | SOME (i, s') =>
        case Int.fromString i of
            NONE => compilerBug ("could not convert string to int: " ^ i) s
          | SOME i => SOME (T.Int i, skipWS rdr s')

(* extract a keyword or identifier from a positional stream *)
fun getWord rdr s =
    let
       fun isValid x = Char.isAlphaNum x orelse x = #"_"
    in
       case get rdr isValid s of
           NONE => NONE
         | SOME (token, s') =>
           case Map.find (keywords, token) of
               NONE => SOME (T.Id token, skipWS rdr s')
             | SOME ctor => SOME (ctor, skipWS rdr s')
    end

(* extract a symbol from a positional stream *)
fun getSymbol rdr s =
    case get rdr Char.isPunct s of
        NONE => NONE
      | SOME (token, s') =>
        case Map.find (symbols, token) of
            NONE => NONE
          | SOME ctor => SOME (ctor, skipWS rdr s')

fun make rdr =
    fn s =>
       let
          val s = skipWS rdr s
       in
          case rdr s of
              NONE => NONE
            | SOME (ch, s') =>
              if Char.isPunct ch then
                 getSymbol rdr s
              else if Char.isDigit ch then
                 getInt rdr s
              else
                 getWord rdr s
       end
end

functor TestInternal () =
struct
   structure T = Token

   val rdr = Pos.reader Reader.string
   val s = Pos.stream "123 foo"

   val SOME (T.Int 123, (" foo", _)) = Lexer.getInt rdr s

   val s = Pos.stream "if x then y else z"
   val SOME (T.If, _) = Lexer.getWord rdr s

   val s = Pos.stream ":=123"
   val SOME (T.Assign, _) = Lexer.getSymbol rdr s

end

structure Lexer :> LEXER = Lexer

functor TestExternal () =
struct
   structure T = Token

   val rdr = Pos.reader Reader.string
   val s = Pos.stream "if x then y else z"
   val lex = Lexer.make rdr

   val [T.If, T.Id "x", T.Then, T.Id "y", T.Else, T.Id "z"] = Reader.consume lex s
end

functor Test () =
struct
   structure Z = TestInternal ()
   structure Z = TestExternal ()
end
