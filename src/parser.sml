signature PARSER =
sig

   exception Error (* of Pos.t * string *)

   (* TODO: should return Absyn.dec ? union of exp, dec fundec ? *)
   val make: (Token.t * Pos.t, 'a) Reader.t -> (Absyn.exp, 'a) Reader.t

end

structure Parser =
struct

exception Error (* of Pos.t * string *)
exception NotImplemented

structure T = Token
structure A = Absyn
structure S = Symbol

fun parseExp rdr s =
    case rdr s of
        SOME (T.Id (x, p), s') => SOME (A.VarExp (A.SimpleVar (S.symbol x, p)), s')

fun parseIf rdr s =
    case rdr s of
        SOME (T.If pos, s1) =>
        (case parseExp rdr s1 of
             SOME (e1, s2) =>
             (case rdr s2 of
                  SOME (T.Then p1, s3) =>
                  (case parseExp rdr s3 of
                       SOME (e2, s4) =>
                       (case rdr s4 of
                            SOME (T.Else p1, s5) =>
                            (case parseExp rdr s5 of
                                 SOME (e3, s6) =>
                                 A.IfExp {test = e1, then' = e2, else' = SOME e3, pos = pos}
                               | NONE => raise Error)
                          | NONE => raise Error)
                     | NONE => raise Error)
                | NONE => raise Error)
           | NONE => raise Error)
      | NONE => raise Error

fun make rdr =
    fn s =>
       case rdr s of

           SOME (T.If p, s') => parseIf rdr s

end
