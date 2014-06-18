structure Token =
struct
   datatype t = LParen
              | RParen
              | LBracket
              | RBracket
              | LBrace
              | RBrace
              | Colon
              | Assign
              | Dot
              | Comma
              | Semicolon
              | Times
              | Divide
              | Plus
              | Minus
              | Eq
              | Neq
              | Gt
              | Lt
              | Geq
              | Leq
              | And
              | Or

              | Array
              | Break
              | Do
              | Else
              | End
              | For
              | Function
              | If
              | In
              | Let
              | Nil
              | Of
              | Then
              | To
              | Type
              | Var
              | While

              | Id of string
              | TyId of string

              | Int of int
              | String of string

   fun show LParen     = "LParen"
     | show RParen     = "RParen"
     | show LBracket   = "LBracket"
     | show RBracket   = "RBracket"
     | show LBrace     = "LBrace"
     | show RBrace     = "RBrace"
     | show Colon      = "Colon"
     | show Assign     = "Assign"
     | show Dot        = "Dot"
     | show Comma      = "Comma"
     | show Semicolon  = "Semicolon"
     | show Times      = "Times"
     | show Divide     = "Divide"
     | show Plus       = "Plus"
     | show Minus      = "Minus"
     | show Eq         = "Eq"
     | show Neq        = "Neq"
     | show Gt         = "Gt"
     | show Lt         = "Lt"
     | show Geq        = "Geq"
     | show Leq        = "Leq"
     | show And        = "And"
     | show Or         = "Or"

     | show Array      = "Array"
     | show Break      = "Break"
     | show Do         = "Do"
     | show Else       = "Else"
     | show End        = "End"
     | show For        = "For"
     | show Function   = "Function"
     | show If         = "If"
     | show In         = "In"
     | show Let        = "Let"
     | show Nil        = "Nil"
     | show Of         = "Of"
     | show Then       = "Then"
     | show To         = "To"
     | show Type       = "Type"
     | show Var        = "Var"
     | show While      = "While"

     | show (Id x)     = "Id{" ^ x ^ "}"
     | show (TyId x)   = "TyId{" ^ x ^ "}"

     | show (Int i)    = "Int {" ^ Int.toString i ^ "}"
     | show (String s) = "String {" ^ s ^ "}"

fun eq (a: t, b: t) = a = b

end
