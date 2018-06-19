
structure ParseTree = struct

exception OperationNotSupported

open Grammar
open Helper

datatype UnOp = Mean | StdDev | Median | SumL | ProdL | ToString | ToInt | ToFloat | Variance 
datatype BinOp = Add | Sub | Div | Mul | Not | And | Or | Pow | RT | Cov | Corr | Concat | LinReg | GetFloat | GetInt | GetString
datatype TerOp = SubString
datatype OpRel = GTR | LTR | EQR | NEQR | GEQR | LEQR

datatype Expr = Const of tipo
              | FuncOne of UnOp * Expr
              | FuncTwo of BinOp * Expr * Expr
              | FuncThree of TerOp * Expr * Expr * Expr
              | Rel of OpRel * Expr * Expr
              | Var of string


datatype Tree = Assign of string * Expr
              | Print of Expr
              | If of Expr * (Tree list) * (Tree list)
              | While of Expr * (Tree list)
              | Null

type RoseTree = Tree list

fun getTertiaryFun("substring", e1, e2, e3) = FuncThree(SubString, e1, e2, e3)
  | getTertiaryFun(_,_,_,_) = raise OperationNotSupported

fun getBinaryFun("+", e1, e2) = FuncTwo(Add, e1, e2)
  | getBinaryFun("-", e1, e2) = FuncTwo(Sub, e1, e2)
  | getBinaryFun("/", e1, e2) = FuncTwo(Div, e1, e2)
  | getBinaryFun("*", e1, e2) = FuncTwo(Mul, e1, e2)
  | getBinaryFun("!", e1, e2) = FuncTwo(Not, e1, e2)
  | getBinaryFun("&&", e1, e2) = FuncTwo(And, e1, e2)
  | getBinaryFun("||", e1, e2) = FuncTwo(Or, e1, e2)
  | getBinaryFun("pow", e1, e2) = FuncTwo(Pow, e1, e2)
  | getBinaryFun("rt", e1, e2) = FuncTwo(RT, e1, e2)
  | getBinaryFun("covariance", e1, e2) = FuncTwo(Cov, e1, e2)
  | getBinaryFun("correlation", e1, e2) = FuncTwo(Corr, e1, e2)
  | getBinaryFun("++", e1, e2) = FuncTwo(Concat, e1, e2)
  | getBinaryFun("linearRegression", e1, e2) = FuncTwo(LinReg, e1, e2)
  | getBinaryFun("getFloat", e1, e2) = FuncTwo(GetFloat, e1, e2)
  | getBinaryFun("getString", e1, e2) = FuncTwo(GetString, e1, e2)
  | getBinaryFun(_,_,_) = raise OperationNotSupported
  handle e => (print "Exception: ";exnName e; e)

fun floatListToSampleExpr(fl) = Const(Sample (List.map (fn(x) => Primitivo(Float_ x)) fl))

fun intListToSampleExpr(il) = Const(Sample (List.map (fn(x) => Primitivo(Int_ x)) il))

fun stringListToSampleExpr(il) = Const(Sample (List.map (fn(x) => Primitivo(String_ x)) il))

fun boolListToSampleExpr(il) = Const(Sample (List.map (fn(x) => Primitivo(Boolean_ x)) il))



fun showBinOp(Add) = "+"
  | showBinOp(Sub) = "-"
  | showBinOp(Div) = "/"
  | showBinOp(Mul) = "*"
  | showBinOp(Not) = "!"
  | showBinOp(And) = "&&"
  | showBinOp(Or) = "||"
  | showBinOp(Pow) = "pow"
  | showBinOp(RT) = "rt"
  | showBinOp(Cov) = "covariance"
  | showBinOp(Corr) = "correlation"
  | showBinOp(Concat) = "++"
  | showBinOp(LinReg) = "linearRegression"
  | showBinOp(GetFloat) = "getFloat"
  | showBinOp(GetInt) = "getInt"
  | showBinOp(GetString) = "getString"


fun getFunctionOne("mean", e1) = FuncOne(Mean,e1)
  | getFunctionOne("stdDeviation", e1) = FuncOne(StdDev,e1)
  | getFunctionOne("variance", e1) = FuncOne(Variance,e1)
  | getFunctionOne("median", e1) = FuncOne(Median,e1)
  | getFunctionOne("sum", e1) = FuncOne(SumL,e1)
  | getFunctionOne("prod", e1) = FuncOne(ProdL,e1)
  | getFunctionOne("toString", e1) = FuncOne(ToString,e1)
  | getFunctionOne("toInt", e1) = FuncOne(ToInt,e1)
  | getFunctionOne("toFloat", e1) = FuncOne(ToFloat,e1)
  | getFunctionOne(_, _) = raise OperationNotSupported
  handle e => (print "Exception: ";exnName e; e)

fun getExprBoolTree("==",e1,e2) = Rel(EQR,e1,e2)
    | getExprBoolTree("!=",e1,e2) = Rel(NEQR,e1,e2)
    | getExprBoolTree("<=",e1,e2) = Rel(LEQR,e1,e2)
    | getExprBoolTree(">=",e1,e2) = Rel(GEQR,e1,e2)
    | getExprBoolTree("<",e1,e2) = Rel(LTR,e1,e2)
    | getExprBoolTree(">",e1,e2) = Rel(GTR,e1,e2)
    | getExprBoolTree(_,_,_) = raise OperationNotSupported

fun insere(hm,n,"int") = AtomMap.insert(hm, n, Grammar.Primitivo(Grammar.Int_ 0))
    | insere(hm,n,"string") = AtomMap.insert(hm, n, Grammar.Primitivo(Grammar.String_ ""))
    | insere(hm,n,"float") = AtomMap.insert(hm, n, Grammar.Primitivo(Grammar.Float_ 0.0))
    | insere(hm,n,"boolean") = AtomMap.insert(hm, n, Grammar.Primitivo(Grammar.Boolean_ false))
    | insere(hm,n,_) = AtomMap.insert(hm, n, Grammar.Void)

fun showOpRel(NEQR) = "!="
  | showOpRel(EQR) = "="
  | showOpRel(GEQR) = ">="
  | showOpRel(LEQR) = "<="
  | showOpRel(GTR) = ">"
  | showOpRel(LTR) = "<"

fun showFunctionOne(Mean) = "mean"
  | showFunctionOne(StdDev) = "stdDeviation"
  | showFunctionOne(Median) = "median"
  | showFunctionOne(SumL) = "sum"
  | showFunctionOne(ProdL) = "prod"
  | showFunctionOne(ToInt) = "toInt"
  | showFunctionOne(ToString) = "toString"
  | showFunctionOne(ToFloat) = "toFloat"
  | showFunctionOne(Variance) = "variance"

fun eval(Const t,vars) = t
  | eval(Var s,vars) =
    let
        val v = valOf(AtomMap.find(vars,Atom.atom s))
    in
        v
    end
  | eval(Rel(oprel,e1,e2),vars) =
        let
            val ee1 = eval(e1,vars)
            val ee2 = eval(e2,vars)
        in
            case (TypeChecker.typeof ee1,TypeChecker.typeof ee2) of
                  ("int","int") => TypeChecker.oper(showOpRel oprel,ee1,ee2)
                | ("float","float") => TypeChecker.oper(showOpRel oprel,ee1,ee2)
                | (_,_) => raise TypeChecker.TypeMismatch
        end
  | eval(FuncOne(ToString,e),vars) = Grammar.Primitivo(Grammar.String_(Grammar.show(eval(e,vars))))
  | eval(FuncOne(func, e1), vars) =
        let
            val ee1 = eval(e1, vars)
        in
            case (TypeChecker.typeof ee1) of
                ("sample of float") => TypeChecker.functionOne(showFunctionOne func, ee1)
              | ("float") => TypeChecker.functionOne(showFunctionOne func, ee1)
              | ("int") => TypeChecker.functionOne(showFunctionOne func, ee1)
              | ("string") => TypeChecker.functionOne(showFunctionOne func, ee1)
              | (_) => raise TypeChecker.TypeMismatch
        end
  | eval(FuncTwo(binop, e1, e2), vars) =
        let
            val ee1 = eval(e1, vars)
            val ee2 = eval(e2, vars)
        in
            case (TypeChecker.typeof ee1, TypeChecker.typeof ee2) of
                ("sample of float", "sample of float") => TypeChecker.statistics(showBinOp binop, ee1, ee2)
              | ("sample of float","int") => TypeChecker.functionTwo(showBinOp binop,ee1,ee2)
              | ("sample of int","int") => TypeChecker.functionTwo(showBinOp binop,ee1,ee2)
              | ("sample of string","int") => TypeChecker.functionTwo(showBinOp binop,ee1,ee2)
              | ("float", "float") => TypeChecker.oper(showBinOp binop, ee1, ee2)
              | ("int", "int") => TypeChecker.oper(showBinOp binop, ee1, ee2)
              | ("string", "string") => TypeChecker.oper(showBinOp binop, ee1, ee2)
              | (_) => raise TypeChecker.TypeMismatch
        end
  | eval(FuncThree(binop, e1, e2, e3), vars) =
        let
            val ee1 = eval(e1, vars)
            val ee2 = eval(e2, vars)
            val ee3 = eval(e3, vars)
        in
            case (TypeChecker.typeof ee1, TypeChecker.typeof ee2, TypeChecker.typeof ee3) of
                ("string", "int", "int") => TypeChecker.functionThree("substring", ee1, ee2, ee3)
              | _ => raise TypeChecker.TypeMismatch
        end


fun interpret((Print expr)::cs,vars,tps) =
        let
            val evaluedExpr = eval(expr,vars)
        in
            print(Grammar.show evaluedExpr ^ "\n");
            interpret(cs,vars,tps)
        end
  | interpret(Assign(var,e)::cs,vars,tps) =
        let
            val evaluedExpr = eval(e,vars)
            val varExists = AtomMap.inDomain(vars,Atom.atom var)
            val t = valOf(AtomMap.find (tps,Atom.atom var))
            val sameType = (TypeChecker.isType evaluedExpr t)
        in
            if (varExists andalso sameType) then
                interpret(cs,Grammar.updateHt(vars,Atom.atom var,evaluedExpr),tps)
            else
                raise TypeChecker.TypeError
        end
  | interpret(If(e,c1,c2)::cs,vars,tps) =
        let
            val evaluedExpr = TypeChecker.extractBool(eval(e,vars))
        in
            if evaluedExpr then
                interpret(c1,vars,tps)
            else
                interpret(c2,vars,tps);
            interpret(cs,vars,tps)
        end
  | interpret(While(e,c1)::cs,vars,tps) =
        let
            val evaluedExpr = TypeChecker.extractBool(eval(e,vars))
            fun innerLoop evExpr exp =
                if evExpr then
                    let 
                        val newExpr = TypeChecker.extractBool(eval(exp,vars))
                    in
                        interpret(c1,vars,tps);
                        innerLoop newExpr exp
                    end
                else
                    ()
        in       
            innerLoop evaluedExpr e;
            interpret(cs,vars,tps)
        end
  | interpret(Null::cs,vars,tps) = print "Null"
  | interpret(nil,vars,tps) = ()
  handle e => (print ("Exception: " ^ exnName e); ())

end
