(*val stack = ref [(NULO2, [NULO])]*)

fun push(x : (Expr * (Tree list))) = 
    let 
      
    in
       (* stack := x :: (!stack);*)
        ()
    end

fun pop(s : (Expr * (Tree list)) list) =
    [] => nil
    | x::xs => (s := xs; x)

(*
fun find (e, [], vars) = raise OperationNotSupported
    | find (e, (a, b) :: xs, vars) = 
        let
            val evaluedExpr = eval(e, vars)
            val evaluedTarget = eval(a, vars)
        in
            if (a = nil) then 
                nil
            else 
		if (evaluedExpr = evaluedTarget) then
		    b
		else 
		    find (e, xs, vars) 
        end  
*)

