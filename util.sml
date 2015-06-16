(* ***** Basic Utility *)
fun fst (a, _) = a
fun snd (_, b) = b

fun a_ (a, _, _) = a
fun b_ (_, b, _) = b
fun c_ (_, _, c) = c

fun curry f = fn a => fn b => f(a,b)

fun each f [] = ()
  | each f (e::es) = (f e; each f es)

(* ***** String to array conversions *)
fun strToSlice str = (Word8ArraySlice.full (Word8Array.fromList (map (Word8.fromInt o Char.ord) (String.explode str))))

fun sliceToStr slice = 
    let val len = Word8ArraySlice.length slice
	fun f i = Char.chr (Word8.toInt (Word8ArraySlice.sub (slice, i)))
    in 
	String.implode (List.tabulate (len, f))
    end
