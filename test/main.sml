structure Main =
struct
   fun main _ = (
      LexerTests.test  ()
    ; ParserTests.test ()
    ; OS.Process.success
   )
end
