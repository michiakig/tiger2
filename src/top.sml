structure Top =
struct
   fun fst (a, _) = a
   fun snd (_, b) = b

   structure StringMap : ORD_MAP = BinaryMapFn(struct
      type ord_key = string
      val compare = String.compare
   end)
end
