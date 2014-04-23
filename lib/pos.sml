(* Pos.sml: datatype for storing line and column position in a file.
 * 22 April 2014 v0.3 *)

signature POS =
sig
   (* Pos.t is an opaque type representing position in a stream *)
   (* ??? should include raw char count as well as line and col ??? *)
   eqtype t

   (* Pos.new constructs a new value at column and line *)
   val new: int * int -> t

   (* Pos.zero is the start of a file: line 1, column 0 *)
   val zero: t

   (* Select fields *)
   val col: t -> int
   val line: t -> int

   (* Increment fields *)
   val incrCol: t -> t
   val incrLine: t -> t

   (* Advance fields by a value *)
   val advCol: t * int -> t
   val advLine: t * int -> t

   (* Wrap a stream with the zero position *)
   val stream: 'a -> 'a * t

   (* Given a char reader, return a positional char reader *)
   val reader: (char, 'a) Reader.t -> (char * t, 'a * t) Reader.t

   val show: t -> string
end

structure Pos =
struct
   type t = {col: int, line: int}

   fun new (col, line) = {col = col, line = line}

   val zero = {col = 0, line = 1}

   fun col      {col, line} = col
   fun line     {col, line} = line

   fun incrCol  {col, line} = {col = col + 1, line = line}
   fun incrLine {col, line} = {col = 0,       line = line + 1}

   fun advCol  ({col, line}, n) = {col = col + n, line = line}
   fun advLine ({col, line}, n) = {col = 0,       line = line + n}

   fun stream s = (s, zero)

   fun reader rdr =
       fn (s, p) =>
          case rdr s of
              NONE            => NONE
            | SOME (#"\n", t) => SOME ((#"\n", p), (t, incrLine p))
            | SOME (x,     t) => SOME ((x,     p), (t, incrCol  p))

   fun show {col, line} = Int.toString line ^ ":" ^ Int.toString col
end

structure Pos :> POS = Pos

functor Test () = struct
   local
      open Pos
   in

   val 0 = col  zero
   val 1 = line zero

   val 1 = col  (incrCol  zero)
   val 2 = line (incrLine zero)

   val 0 = col  (incrLine zero)
   val 0 = col  (incrLine (incrCol zero))

   val rdr = reader Substring.getc
   val SOME ((#"f", p), (_, pp)) = rdr (stream (Substring.full "foo"))
   val 0 = col  p
   val 1 = line p
   val 1 = col  pp
   val 1 = line pp

   val SOME (_, s) = rdr (stream (Substring.full "x\ny"))
   val SOME ((#"\n", p), s as (_, pp)) = rdr s
   val 1 = col  p
   val 1 = line p

   val 2 = line pp
   val SOME ((#"y", p), s as (_, pp)) = rdr s
   val 0 = col  p
   val 2 = line p

   val 3 = line (new (4, 3))
   val 4 = col (new (4, 3))

   val 5 = col (advCol (zero, 5))
   val 4 = line (advLine (zero, 3))

   end
end
