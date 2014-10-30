structure Tiger = struct
   val fromFile : string -> TextIO.StreamIO.instream =
       TextIO.getInstream o TextIO.openIn

   val rdr   = Pos.reader Reader.streamIO
   val lexer = Lexer.make rdr
   val parse = Parser.make lexer

   fun main filename = parse (Pos.stream (fromFile filename))
end
