signature PARSER =
sig

   exception Error of Pos.t * string

   (* TODO: should return Absyn.dec ? union of exp, dec fundec ? *)
   val make: (Token.t * Pos.t, 'a) Reader.t -> (Absyn.exp, 'a) Reader.t

end

structure Parser =
struct

datatype ('a, 'b) result = Success of 'a * 'b | Error of string * 'b | EOF
type ('a, 'b) t = 'b -> ('a, 'b) result

exception Error of Pos.t * string
exception NotImplemented

fun compilerBug (s, msg) = raise Error (Pos.getPos s, "compiler bug! " ^ msg)

structure T = Token
structure A = Absyn
structure S = Symbol

fun make rdr =
    fn s =>
       let
          (* local mutable pointer to current position in stream *)
          val rest = ref s

          fun getPos () = Pos.getPos (!rest)

          (* raise exceptions for syntax (i.e. user) errors *)
          fun error (t, t') = raise (Error (getPos (), "expected " ^ T.show t ^ ", but got " ^ T.show t'))
          fun eof   ()      = raise (Error (getPos (), "unexpected EOF"))

          (* get next token, if present *)
          fun peek () =
              case rdr (!rest) of
                  SOME (x, _) => SOME x
                | NONE        => NONE

          (* consume the next token. if already at EOF, do nothing *)
          fun adv () =
              case rdr (!rest) of
                  SOME (_, s') => rest := s'
                | NONE         => () (* compilerBug (!rest, "advancing past EOF") *)

          (* attempt to match the next token. consume it if successful *)
          fun match t =
              case peek () of
                  SOME t' => if t = t' then adv () else error (t, t')
                | NONE    => eof ()

          (* attempt to parse an identifier. consume and return it if successful *)
          fun parseId () =
              case peek () of
                  SOME (T.Id id) => (adv (); id)
                | SOME t         => raise (Error (getPos (), "expected Id, but got " ^ T.show t))
                | NONE           => eof ()

          (* parser entry point *)
          fun parseExp () =
              case peek () of
                  SOME (T.Id x) => A.VarExp (A.SimpleVar (S.symbol x, getPos ())) before adv ()

                | SOME T.Nil => A.NilExp before adv ()

                | SOME T.If => parseIf ()

                | SOME T.While => parseWhile ()

                | SOME T.For => parseFor ()

          and parseIf () =
              let
                 val p  = getPos ()
                 val _  = match T.If
                 val e1 = parseExp ()
                 val _  = match T.Then
                 val e2 = parseExp ()
                 val _  = match T.Else
                 val e3 = parseExp ()
              in
                 A.IfExp {test  = e1,
                          then' = e2,
                          else' = SOME e3,
                          pos   = p}
              end

          and parseWhile () =
              let
                 val p = getPos()
                 val _ = match T.While
                 val e1 = parseExp ()
                 val _ = match T.Do
                 val e2 = parseExp ()
              in
                 A.WhileExp {test = e1,
                             body = e2,
                             pos  = p}
              end

          and parseFor () =
              let
                 val p  = getPos ()
                 val _  = match T.For
                 val x  = parseId ()
                 val _  = match T.Assign
                 val e1 = parseExp ()
                 val _  = match T.To
                 val e2 = parseExp ()
                 val _  = match T.Do
                 val e3 = parseExp ()
              in
                 A.ForExp {var    = S.symbol x,
                           escape = ref true, (* ??? *)
                           lo     = e1,
                           hi     = e2,
                           body   = e3,
                           pos    = p}
              end
       in
          SOME (parseExp (), !rest)
       end

end
