signature LEXER =
sig

   exception Error of Pos.t * string

   val make: (char * Pos.t, 'a) Reader.t -> (Token.t, 'a) Reader.t

end

structure Lexer =
struct

structure T = Token

exception Error of Pos.t * string

fun compilerBug rdr msg s =
    case rdr s of
        NONE => raise Error (Pos.zero, "compiler bug! " ^ msg)
      | SOME ((_, p), _) => raise Error (p, "compiler bug! " ^ msg)

(* Pair utilities *)
fun fst (a, _) = a
fun snd (_, b) = b

(* Drop leading whitespace from a positional stream *)
fun skipWS rdr = Reader.dropWhile rdr (Char.isSpace o fst)

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

(* extract chars from a positional stream that match a predicate *)
fun get (rdr: (char * Pos.t, 'a) Reader.t) isValid s =
    let
       val (chars, s') = Reader.takeWhile rdr (isValid o fst) s
    in
       if length chars < 1 then
          NONE
       else
          SOME ((String.implode (map fst chars), snd (hd chars)), s')
    end

(* extract an integer literal from a positional stream *)
fun getInt rdr s =
    case get rdr Char.isDigit s of
        NONE => NONE
      | SOME ((i, p), s') =>
        case Int.fromString i of
            NONE => compilerBug rdr ("could not convert string to int: " ^ i) s
          | SOME i => SOME (T.Int (i, p), s')

(* extract a keyword or identifier from a positional stream *)
fun getWord rdr s =
    let
       fun isValid x = Char.isAlphaNum x orelse x = #"_"
    in
       case get rdr isValid s of
           NONE => NONE
         | SOME (x as (token, p), s') =>
           case Map.find (keywords, token) of
               NONE => SOME (T.Id x, s')
             | SOME ctor => SOME (ctor p, s')
    end

(* extract a symbol from a positional stream *)
fun getSymbol rdr s =
    case get rdr Char.isPunct s of
        NONE => NONE
      | SOME ((token, p), s') =>
        case Map.find (symbols, token) of
            NONE => NONE
          | SOME ctor => SOME (ctor p, s')

fun make (rdr : (char * Pos.t, 'a) Reader.t) : (Token.t, 'a) Reader.t =
    fn s =>
       let
          val s = skipWS rdr s
       in
          case rdr s of
              NONE => NONE
            | SOME ((ch, p), s') =>
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

   val SOME (T.Int (123, _), (" foo", _)) = Lexer.getInt rdr s

   val s = Pos.stream "if x then y else z"
   val SOME (T.If _, _) = Lexer.getWord rdr s

   val s = Pos.stream ":=123"
   val SOME (T.Assign _, _) = Lexer.getSymbol rdr s

end

structure Lexer :> LEXER = Lexer

functor TestExternal () =
struct
   structure T = Token

   val rdr = Pos.reader Reader.string
   val s = Pos.stream "if x then y else z"
   val lex = Lexer.make rdr

   val [T.If _, T.Id ("x", _), T.Then _, T.Id ("y", _), T.Else _, T.Id ("z", _)]
       = Reader.consume lex s
end

functor Test () =
struct
   structure Z = TestInternal ()
   structure Z = TestExternal ()
end

