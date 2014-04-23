structure Token =
struct
   type p = Pos.t

   datatype t = LParen of p
              | RParen of p
              | LBracket of p
              | RBracket of p
              | LBrace of p
              | RBrace of p
              | Colon of p
              | Assign of p
              | Dot of p
              | Comma of p
              | Semicolon of p
              | Times of p
              | Divide of p
              | Plus of p
              | Minus of p
              | Eq of p
              | Neq of p
              | Gt of p
              | Lt of p
              | Geq of p
              | Leq of p
              | And of p
              | Or of p

              | Array of p
              | Break of p
              | Do of p
              | Else of p
              | End of p
              | For of p
              | Function of p
              | If of p
              | In of p
              | Let of p
              | Nil of p
              | Of of p
              | Then of p
              | To of p
              | Type of p
              | Var of p
              | While of p

              | Id of string * p
              | TyId of string * p

              | Int of int * p
              | String of string * p

   fun show (LParen p)      = "LParen " ^ Pos.show p
     | show (RParen p)      = "RParen " ^ Pos.show p
     | show (LBracket p)    = "LBracket " ^ Pos.show p
     | show (RBracket p)    = "RBracket " ^ Pos.show p
     | show (LBrace p)      = "LBrace " ^ Pos.show p
     | show (RBrace p)      = "RBrace " ^ Pos.show p
     | show (Colon p)       = "Colon " ^ Pos.show p
     | show (Assign p)      = "Assign " ^ Pos.show p
     | show (Dot p)         = "Dot " ^ Pos.show p
     | show (Comma p)       = "Comma " ^ Pos.show p
     | show (Semicolon p)   = "Semicolon " ^ Pos.show p
     | show (Times p)       = "Times " ^ Pos.show p
     | show (Divide p)      = "Divide " ^ Pos.show p
     | show (Plus p)        = "Plus " ^ Pos.show p
     | show (Minus p)       = "Minus " ^ Pos.show p
     | show (Eq p)          = "Eq " ^ Pos.show p
     | show (Neq p)         = "Neq " ^ Pos.show p
     | show (Gt p)          = "Gt " ^ Pos.show p
     | show (Lt p)          = "Lt " ^ Pos.show p
     | show (Geq p)         = "Geq " ^ Pos.show p
     | show (Leq p)         = "Leq " ^ Pos.show p
     | show (And p)         = "And " ^ Pos.show p
     | show (Or p)          = "Or " ^ Pos.show p

     | show (Array p)       = "Array " ^ Pos.show p
     | show (Break p)       = "Break " ^ Pos.show p
     | show (Do p)          = "Do " ^ Pos.show p
     | show (Else p)        = "Else " ^ Pos.show p
     | show (End p)         = "End " ^ Pos.show p
     | show (For p)         = "For " ^ Pos.show p
     | show (Function p)    = "Function " ^ Pos.show p
     | show (If p)          = "If " ^ Pos.show p
     | show (In p)          = "In " ^ Pos.show p
     | show (Let p)         = "Let " ^ Pos.show p
     | show (Nil p)         = "Nil " ^ Pos.show p
     | show (Of p)          = "Of " ^ Pos.show p
     | show (Then p)        = "Then " ^ Pos.show p
     | show (To p)          = "To " ^ Pos.show p
     | show (Type p)        = "Type " ^ Pos.show p
     | show (Var p)         = "Var " ^ Pos.show p
     | show (While p)       = "While " ^ Pos.show p

     | show (Id (x, p))     = "Id (" ^ x ^ "," ^ Pos.show p ^ ")"
     | show (TyId (x, p))   = "TyId (" ^ x ^ "," ^ Pos.show p ^ ")"

     | show (Int (i, p))    = "Int (" ^ Int.toString i ^ "," ^ Pos.show p ^ ")"
     | show (String (s, p)) = "String (" ^ s ^ "," ^ Pos.show p ^ ")"

fun eq (a: t, b: t) = a = b

end
