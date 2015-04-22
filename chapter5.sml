(*Jessica Tan*)


(*Problem 10*)
fun gcd(x,y) =
   if y = 0 then x
   else gcd(y, x mod y)

 
 (*Problem 11*)
fun allCaps(s) = 
   if s = "" then s
   else (Char.toString(Char.toUpper(String.sub(s,0)))) ^ allCaps(substring(s,1, (size(s)-1)))

(*Problem 12*)
fun firstCaps(l) =
	if l = [] then nil
	else (Char.toString(Char.toUpper(String.sub(List.hd(l),0)))) ^ (substring(List.hd(l), 1,(size(List.hd(l))-1))) :: firstCaps(List.tl(l))

(*Problem 13*)
fun swap ([]) = []
   | swap(h::nil) = [h]
   |  swap(h::t) = List.hd(t) :: h :: swap(List.tl(t))

(*Problem 14*)
fun rotate (n, []) = []
 	| rotate (n, h:: nil) = [h] 
    | rotate(0, l) = l
    | rotate(n,h::t) = rotate((n-1), t @ [h])

(*Problem 15*)
fun delete(n,s) = 
    let fun delh(n,[]) = nil
        | delh(0,h::t) = t
        | delh(n,h::t) = [h] @ delh(n-1, t)
    in 
        implode(delh(n,explode(s)))

    end

(*Problem 16*)
fun intpow(x,0) = 1
    | intpow(x,1) = x
    | intpow(x,n) = 
          if (n mod 2 = 0 ) then intpow(x*x, n div 2) 
          else x * intpow(x*x, (n-1) div 2 )

(*Problem 17*)

fun rotate2 (iter, L) =
    let fun rhelp(i, L) =
        if iter = length(L) - i then L
        else let
            val last = List.last(L)
            val rest = rev(tl(rev(L)))
        in
            rhelp(i + 1,  last :: rest)
        end
    in
        rhelp(0, L)
    end


(*Problem 18*)
fun rotate3(n,[]) = nil
    | rotate3(n, h ::nil) =[h]
    | rotate3(0,l) = l
    | rotate3(n,h::t)= if (n > length(h::t)) then rotate3((n-length(h::t)) -1, t @[h] ) else rotate3((n-1), t @ [h])

(*Problem 19*)
(*ask about this one*)

fun delete2 0 = (fn s => substring(s,1,size(s)-1))
    | delete2 i = (fn s => let fun delh(i,[]) = nil
        | delh(0,h::t) = t
        | delh(i,h::t) = [h] @ delh(i-1, t)
    in 
        implode(delh(i,explode(s)))

    end)


(*Problem 20*)
fun delete5(S) =
    let fun delh(n, "") = ""
        | delh (0, s) = String.substring(s, 1, size(s) - 1)
        | delh (x, s) = Char.toString(String.sub(s, 0)) ^ delh(x-1, String.substring(s, 1, size(s) - 1))
    in
        delh(5, S)
    end   

(*Problem 21*)

fun evens(L)= List.filter(fn x=>((x mod 2) = 0)) L 

(*Problem 22*)

fun lowerFirsts(L) = List.filter(fn x =>Char.isLower(hd((explode(x))))) L

(*Problem 23*)

fun allCaps2(s) = implode( List.map (fn x=> Char.toUpper(x)) (explode(s)))


(*Problem 24*)

fun find(s, file) =
  let
    val ins = TextIO.openIn file;
    fun helper(ins)=
        let val t = TextIO.inputLine(ins) 
            in 
                case t of
                    NONE=> ""
                    | SOME(u)=> if String.isSubstring s u 
                        then u ^ helper(ins)
                        else helper(ins)
            end
    in 
        TextIO.output(TextIO.stdOut, helper(ins))

    end


(*Problem 25*)

fun transform f L= 
    let 
        fun change([])= []
            | change(h::t) =  (f h::change(t)) handle _ => (h::change t) 
        
    in 
        change L
    end 
    
(*Problem 26*)
datatype Natural = O
| succ of Natural
 
(*Problem 27*)
fun convert(x) =
    let 
        fun helper(O,t) = t
        | helper(succ(x), t) = helper(x, t + 1)
    in 
        helper(x,0)

    end 
(*Problem 28*)
fun add(x,y) = 
    let
        fun helper(x,O) = x
            | helper(x, succ(y)) = helper(succ(x),y)
    in 
        helper(x,y)
    end


(*Problem 29*)
fun mul(x,y) = 
    let fun helper (O, _) = O
        |helper(succ(x),y) = add(succ(x), helper(x,y))
    in
        helper(x,y)
    end 



(*Problem 30*)
fun hadd([]) = O
    | hadd(L) = foldr add O L
