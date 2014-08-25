structure Eq =
struct
   type 'a t = 'a * 'a -> bool
   val unit: unit t = op =
   val int: int t = op =
   val word: word t = op =
   val char: char t = op =
   val real: real t = Real.==
   val string: string t = op =
   val bool: bool t = op =
   val list: 'a t -> 'a list t = fn eq => ListPair.allEq eq
   val option: 'a t -> 'a option t =
    fn eq =>
       fn (NONE, SOME _) => false
        | (SOME _, NONE) => false
        | (SOME x, SOME y) => eq (x, y)
        | _ => true
   val pair: 'a t * 'b t -> ('a * 'b) t =
    fn (eqa,eqb) => fn ((a,b),(a',b')) => eqa (a,a') andalso eqb (b,b')
   val sq: 'a t -> ('a * 'a) t =
    fn eq => fn ((a,a'),(a'',a''')) => eq (a,a'') andalso eq (a',a''')
end
