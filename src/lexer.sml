signature LEXER =
sig
   exception Error of Pos.t * string
   val make: (char * Pos.t, 'a * Pos.t) Reader.t -> (Token.t * Pos.t, 'a * Pos.t) Reader.t
end

structure Lexer =
struct

open Top
structure T = Token

exception Error of Pos.t * string
fun compilerBug msg s = raise Error (Pos.getPos s, "compiler bug! " ^ msg)

(* Predicates for stream elements *)
fun isSpace (x, _) = Char.isSpace x
fun isDigit (x, _) = Char.isDigit x
fun isPunct (x, _) = Char.isPunct x
fun isValid (x, _) = Char.isAlphaNum x orelse x = #"_"

(* Drop leading whitespace from a stream *)
fun skipWS rdr = Reader.dropWhile rdr isSpace

(* Fold a list of pairs of keys and values into a new map *)
fun collect l =
    List.foldl (fn ((k, v), acc) => StringMap.insert (acc, k, v)) StringMap.empty l

(* Map from raw token values to Token.t ctors *)
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
       , (":",  T.Colon)
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

(* Extract chars from a stream that match a predicate *)
fun get rdr p s =
    case Reader.takeWhile rdr p s of
        ([], _)                  => NONE
      | (chars as ((_, p)::_), t) => SOME ((String.implode (map fst chars), p), t)

(* Extract an integer literal from a positional stream *)
fun getInt rdr s =
    case get rdr isDigit s of
        NONE => NONE
      | SOME ((i, p), s') =>
        case Int.fromString i of
            NONE => compilerBug ("could not convert string to int: " ^ i) s
          | SOME i => SOME ((T.Int i, p), skipWS rdr s')

(* Extract a keyword or identifier from a positional stream *)
fun getWord rdr s =
    case get rdr isValid s of
        NONE => NONE
      | SOME ((tok, p), s') =>
        case StringMap.find (keywords, tok) of
            NONE      => SOME ((T.Id tok, p), skipWS rdr s')
          | SOME ctor => SOME ((ctor,     p), skipWS rdr s')

(* Extract a symbol from a positional stream *)
fun getSymbol rdr s =
    case get rdr isPunct s of
        NONE => NONE
      | SOME ((tok, p), s') =>
        case StringMap.find (symbols, tok) of
            NONE      => NONE
          | SOME ctor => SOME ((ctor, p), skipWS rdr s')

(* Given a char reader, return a token reader (lexer) *)
fun make rdr =
    fn s =>
       let
          val s = skipWS rdr s
       in
          case rdr s of
              NONE => NONE
            | SOME ((ch, _), s') =>
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

   val rdr = Pos.reader2 Reader.string
   val s = Pos.stream "123 foo"

   val SOME ((T.Int 123, _), ("foo", _)) = Lexer.getInt rdr s

   val s = Pos.stream "if x then y else z"
   val SOME ((T.If, _), _) = Lexer.getWord rdr s

   val s = Pos.stream ":=123"
   val SOME ((T.Assign, _), _) = Lexer.getSymbol rdr s
end

structure Lexer :> LEXER = Lexer

functor TestExternal () =
struct
   open Top
   structure T = Token

   val rdr = Pos.reader2 Reader.string
   val s   = Pos.stream "if x then y else z"
   val lex = Lexer.make rdr

   val [T.If, T.Id "x", T.Then, T.Id "y", T.Else, T.Id "z"] = map fst (Reader.consume lex s)
end

functor Test () =
struct
   structure Z = TestInternal ()
   structure Z = TestExternal ()
end
