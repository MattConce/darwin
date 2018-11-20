structure DarwinTokens =
  struct
    datatype token
      = KW_let
      | KW_in
      | KW_title
      | SSTRING of (string list)
      | ID of string
      | NUM of Int.int
      | REAL of Real.real
      | SINT of (Int.int list)
      | PLUS
      | EEQ
      | SFLOAT of (Real.real list)
      | TIMES
      | DIV
      | MINUS
      | COMMA
      | SBOOL of (Bool.bool list)
      | LP
      | RP
      | BOOL of bool
      | AND
      | OR
      | SPACE
      | GT
      | LT
      | LEQ
      | GEQ
      | NEQ
      | KW_variables
      | SEMI
      | TIPO of string
      | DOTDOT
      | COLON
      | KW_commands
      | STR of string
      | KW_Print
      | KW_endvars
      | KW_terminate
      | KW_SUM
      | KW_PROD
      | EMPTY
      | KW_GETS
      | KW_IF
      | KW_THEN
      | KW_ELSE
      | KW_WHILE
      | KW_CASE
      | OF
      | KW_DO
      | KW_END
      | KW_TOSTRING
      | KW_MEAN
      | KW_CORR
      | KW_MEDIAN
      | KW_STDEV
      | KW_VAR
      | KW_RT
      | KW_POW
      | KW_GETF
      | KW_COV
      | KW_LINREG
      | VOID
      | TUPLE of string
      | STUPLE of string
      | KW_GETI
      | KW_TOFLOAT
      | KW_TOINT
      | CONCAT
      | EOF
    val allToks = [
            KW_let, KW_in, KW_title, PLUS, EEQ, TIMES, DIV, MINUS, COMMA, LP, RP, AND, OR, SPACE, GT, LT, LEQ, GEQ, NEQ, KW_variables, SEMI, DOTDOT, COLON, KW_commands, KW_Print, KW_endvars, KW_terminate, KW_SUM, KW_PROD, EMPTY, KW_GETS, KW_IF, KW_THEN, KW_ELSE, KW_WHILE, KW_CASE, OF, KW_DO, KW_END, KW_TOSTRING, KW_MEAN, KW_CORR, KW_MEDIAN, KW_STDEV, KW_VAR, KW_RT, KW_POW, KW_GETF, KW_COV, KW_LINREG, VOID, KW_GETI, KW_TOFLOAT, KW_TOINT, CONCAT, EOF
           ]
    fun toString tok =
(case (tok)
 of (KW_let) => "let"
  | (KW_in) => "in"
  | (KW_title) => "title"
  | (SSTRING(_)) => "SSTRING"
  | (ID(_)) => "ID"
  | (NUM(_)) => "NUM"
  | (REAL(_)) => "REAL"
  | (SINT(_)) => "SINT"
  | (PLUS) => "+"
  | (EEQ) => "=="
  | (SFLOAT(_)) => "SFLOAT"
  | (TIMES) => "*"
  | (DIV) => "/"
  | (MINUS) => "-"
  | (COMMA) => ","
  | (SBOOL(_)) => "SBOOL"
  | (LP) => "("
  | (RP) => ")"
  | (BOOL(_)) => "BOOL"
  | (AND) => "&&"
  | (OR) => "||"
  | (SPACE) => " "
  | (GT) => ">"
  | (LT) => "<"
  | (LEQ) => "<="
  | (GEQ) => ">="
  | (NEQ) => "!="
  | (KW_variables) => "variables"
  | (SEMI) => "SEMI"
  | (TIPO(_)) => "TIPO"
  | (DOTDOT) => ":="
  | (COLON) => ":"
  | (KW_commands) => "commands"
  | (STR(_)) => "STR"
  | (KW_Print) => "print"
  | (KW_endvars) => "end variables"
  | (KW_terminate) => "terminate"
  | (KW_SUM) => "sum"
  | (KW_PROD) => "prod"
  | (EMPTY) => "{}"
  | (KW_GETS) => "getString"
  | (KW_IF) => "if"
  | (KW_THEN) => "then"
  | (KW_ELSE) => "else"
  | (KW_WHILE) => "while"
  | (KW_CASE) => "case"
  | (OF) => "of"
  | (KW_DO) => "do"
  | (KW_END) => "end"
  | (KW_TOSTRING) => "toString"
  | (KW_MEAN) => "mean"
  | (KW_CORR) => "correlation"
  | (KW_MEDIAN) => "median"
  | (KW_STDEV) => "stdDeviation"
  | (KW_VAR) => "variance"
  | (KW_RT) => "rt"
  | (KW_POW) => "pow"
  | (KW_GETF) => "getFloat"
  | (KW_COV) => "covariance"
  | (KW_LINREG) => "linearRegression"
  | (VOID) => "void"
  | (TUPLE(_)) => "TUPLE"
  | (STUPLE(_)) => "STUPLE"
  | (KW_GETI) => "getInt"
  | (KW_TOFLOAT) => "toFloat"
  | (KW_TOINT) => "toInt"
  | (CONCAT) => "++"
  | (EOF) => "EOF"
(* end case *))
    fun isKW tok =
(case (tok)
 of (KW_let) => false
  | (KW_in) => false
  | (KW_title) => true
  | (SSTRING(_)) => false
  | (ID(_)) => false
  | (NUM(_)) => false
  | (REAL(_)) => false
  | (SINT(_)) => false
  | (PLUS) => false
  | (EEQ) => false
  | (SFLOAT(_)) => false
  | (TIMES) => false
  | (DIV) => false
  | (MINUS) => false
  | (COMMA) => false
  | (SBOOL(_)) => false
  | (LP) => false
  | (RP) => false
  | (BOOL(_)) => false
  | (AND) => false
  | (OR) => false
  | (SPACE) => false
  | (GT) => false
  | (LT) => false
  | (LEQ) => false
  | (GEQ) => false
  | (NEQ) => false
  | (KW_variables) => true
  | (SEMI) => false
  | (TIPO(_)) => false
  | (DOTDOT) => false
  | (COLON) => false
  | (KW_commands) => true
  | (STR(_)) => false
  | (KW_Print) => false
  | (KW_endvars) => true
  | (KW_terminate) => true
  | (KW_SUM) => true
  | (KW_PROD) => true
  | (EMPTY) => false
  | (KW_GETS) => true
  | (KW_IF) => true
  | (KW_THEN) => true
  | (KW_ELSE) => true
  | (KW_WHILE) => true
  | (KW_CASE) => true
  | (OF) => false
  | (KW_DO) => true
  | (KW_END) => true
  | (KW_TOSTRING) => true
  | (KW_MEAN) => true
  | (KW_CORR) => true
  | (KW_MEDIAN) => true
  | (KW_STDEV) => true
  | (KW_VAR) => true
  | (KW_RT) => true
  | (KW_POW) => true
  | (KW_GETF) => true
  | (KW_COV) => true
  | (KW_LINREG) => true
  | (VOID) => false
  | (TUPLE(_)) => false
  | (STUPLE(_)) => false
  | (KW_GETI) => true
  | (KW_TOFLOAT) => true
  | (KW_TOINT) => true
  | (CONCAT) => false
  | (EOF) => false
(* end case *))
    fun isEOF EOF = true
      | isEOF _ = false
  end (* DarwinTokens *)

functor DarwinParseFn (Lex : ANTLR_LEXER) = struct

  local
    structure Tok =
DarwinTokens
    structure UserCode =
      struct

    open ParseTree
    fun getTupla x = Helper.toTupla(Helper.getTupleFrom x)
    fun getSampleTuple x = List.map getTupla (String.tokens Helper.isSemi (Helper.removeBrackets(String.implode(Helper.toSemi(String.explode x)))))
    fun getInt x = (TypeChecker.extractInt x)
    fun getFloat x = (TypeChecker.extractFloat x)
    fun getString x = (TypeChecker.extractString x)
    fun getBool x = (TypeChecker.extractBool x)
    fun getList x = (TypeChecker.extractList x)

    fun getVar v = AtomMap.appi (fn (k,w) => print (
        let val _ = print(Atom.toString k)
            val _ = print (TypeChecker.show w)
        in
            ""
        end)) v

fun program_PROD_1_ACT (SR, STR, SEMI, KW_title, KW_variables, KW_commands, variables, SR_SPAN : (Lex.pos * Lex.pos), STR_SPAN : (Lex.pos * Lex.pos), SEMI_SPAN : (Lex.pos * Lex.pos), KW_title_SPAN : (Lex.pos * Lex.pos), KW_variables_SPAN : (Lex.pos * Lex.pos), KW_commands_SPAN : (Lex.pos * Lex.pos), variables_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  (SR)
fun commands_PROD_1_ACT (SEMI, prints, SEMI_SPAN : (Lex.pos * Lex.pos), prints_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  (prints)
fun commands_PROD_2_ACT (SEMI, assign, SEMI_SPAN : (Lex.pos * Lex.pos), assign_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  (assign)
fun commands_PROD_3_ACT (conditional, conditional_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  (conditional)
fun commands_PROD_4_ACT (case_st, case_st_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  (case_st)
fun commands_PROD_5_ACT (loop, loop_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  (loop)
fun assign_PROD_1_ACT (ID, expr, DOTDOT, ID_SPAN : (Lex.pos * Lex.pos), expr_SPAN : (Lex.pos * Lex.pos), DOTDOT_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  (tree:=(ParseTree.Assign(ID,expr))::(!tree); (ParseTree.Assign(ID,expr)))
fun expr_PROD_1_ACT (exp_string, exp_string_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  (exp_string)
fun expr_PROD_2_ACT (exp_bool, exp_bool_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  (exp_bool)
fun expr_PROD_3_ACT (exp_arit, exp_arit_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  (exp_arit)
fun expr_PROD_4_ACT (exp_tupla, exp_tupla_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  (exp_tupla)
fun expr_PROD_5_ACT (funcs_float, funcs_float_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  (funcs_float)
fun expr_PROD_6_ACT (funcs_int, funcs_int_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  (funcs_int)
fun expr_PROD_7_ACT (funcs_string, funcs_string_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  (funcs_string)
fun expr_PROD_8_ACT (val_list, val_list_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  (val_list)
fun val_list_PROD_1_ACT (SINT, SINT_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  (ParseTree.intListToSampleExpr(SINT))
fun val_list_PROD_2_ACT (SFLOAT, SFLOAT_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  (ParseTree.floatListToSampleExpr(SFLOAT))
fun val_list_PROD_3_ACT (SBOOL, SBOOL_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  (ParseTree.boolListToSampleExpr(SBOOL))
fun val_list_PROD_4_ACT (SSTRING, SSTRING_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  (ParseTree.stringListToSampleExpr(SSTRING))
fun val_list_PROD_5_ACT (STUPLE, STUPLE_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  (ParseTree.Const (ParseTree.Sample (getSampleTuple STUPLE)))
fun val_list_PROD_6_ACT (ID, ID_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  (ParseTree.Var ID)
fun prints_PROD_1_ACT (LP, RP, KW_Print, exp_string, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), KW_Print_SPAN : (Lex.pos * Lex.pos), exp_string_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  (
        let
            val k = (ParseTree.Print exp_string)::(!tree)
        in
            tree := k;
            (ParseTree.Print exp_string)
        end
    )
fun funcs_string_PROD_1_ACT (LP, RP, expr, KW_TOSTRING, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), expr_SPAN : (Lex.pos * Lex.pos), KW_TOSTRING_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  (ParseTree.FuncOne(ParseTree.ToString,expr))
fun funcs_string_PROD_2_ACT (LP, RP, exp_arit, KW_GETS, COMMA, string_list, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), exp_arit_SPAN : (Lex.pos * Lex.pos), KW_GETS_SPAN : (Lex.pos * Lex.pos), COMMA_SPAN : (Lex.pos * Lex.pos), string_list_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  (ParseTree.getBinaryFun("getString", string_list, exp_arit))
fun funcs_string_PROD_3_ACT (LP, RP, COMMA, string_list1, string_list2, CONCAT, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), COMMA_SPAN : (Lex.pos * Lex.pos), string_list1_SPAN : (Lex.pos * Lex.pos), string_list2_SPAN : (Lex.pos * Lex.pos), CONCAT_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  (ParseTree.getBinaryFun("concat", string_list1, string_list2))
fun funcs_string_PROD_4_ACT (LP, RP, KW_LINREG, numbers_list1, numbers_list2, COMMA, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), KW_LINREG_SPAN : (Lex.pos * Lex.pos), numbers_list1_SPAN : (Lex.pos * Lex.pos), numbers_list2_SPAN : (Lex.pos * Lex.pos), COMMA_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  (ParseTree.getBinaryFun("linearRegression", numbers_list1, numbers_list2))
fun funcs_int_PROD_1_ACT (LP, RP, exp_arit, KW_GETI, COMMA, int_list, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), exp_arit_SPAN : (Lex.pos * Lex.pos), KW_GETI_SPAN : (Lex.pos * Lex.pos), COMMA_SPAN : (Lex.pos * Lex.pos), int_list_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  (ParseTree.getBinaryFun("getInt", int_list, exp_arit))
fun funcs_int_PROD_2_ACT (LP, RP, KW_TOINT, expr, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), KW_TOINT_SPAN : (Lex.pos * Lex.pos), expr_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  (ParseTree.getFunctionOne("toInt", expr))
fun funcs_float_PROD_1_ACT (LP, RP, EMPTY, KW_SUM, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), EMPTY_SPAN : (Lex.pos * Lex.pos), KW_SUM_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  (ParseTree.getFunctionOne("sum", ParseTree.Const(Grammar.Sample nil)))
fun funcs_float_PROD_2_ACT (LP, RP, float_list, KW_SUM, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), float_list_SPAN : (Lex.pos * Lex.pos), KW_SUM_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  (ParseTree.getFunctionOne("sum", float_list))
fun funcs_float_PROD_3_ACT (LP, RP, KW_PROD, EMPTY, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), KW_PROD_SPAN : (Lex.pos * Lex.pos), EMPTY_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  (ParseTree.getFunctionOne("prod", ParseTree.Const(Grammar.Sample nil)))
fun funcs_float_PROD_4_ACT (LP, RP, KW_PROD, float_list, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), KW_PROD_SPAN : (Lex.pos * Lex.pos), float_list_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  (ParseTree.getFunctionOne("prod", float_list))
fun funcs_float_PROD_5_ACT (LP, RP, KW_MEAN, numbers_list, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), KW_MEAN_SPAN : (Lex.pos * Lex.pos), numbers_list_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  (ParseTree.getFunctionOne("mean", numbers_list))
fun funcs_float_PROD_6_ACT (LP, RP, KW_CORR, numbers_list1, numbers_list2, COMMA, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), KW_CORR_SPAN : (Lex.pos * Lex.pos), numbers_list1_SPAN : (Lex.pos * Lex.pos), numbers_list2_SPAN : (Lex.pos * Lex.pos), COMMA_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  (ParseTree.getBinaryFun("correlation", numbers_list1, numbers_list2))
fun funcs_float_PROD_7_ACT (LP, RP, KW_STDEV, numbers_list, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), KW_STDEV_SPAN : (Lex.pos * Lex.pos), numbers_list_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  (ParseTree.getFunctionOne("stdDeviation", numbers_list))
fun funcs_float_PROD_8_ACT (LP, RP, KW_MEDIAN, numbers_list, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), KW_MEDIAN_SPAN : (Lex.pos * Lex.pos), numbers_list_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  (ParseTree.getFunctionOne("median", numbers_list))
fun funcs_float_PROD_9_ACT (LP, RP, numbers_list, KW_VAR, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), numbers_list_SPAN : (Lex.pos * Lex.pos), KW_VAR_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  (ParseTree.getFunctionOne("variance", numbers_list))
fun funcs_float_PROD_10_ACT (LP, RP, numbers_list1, numbers_list2, COMMA, KW_COV, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), numbers_list1_SPAN : (Lex.pos * Lex.pos), numbers_list2_SPAN : (Lex.pos * Lex.pos), COMMA_SPAN : (Lex.pos * Lex.pos), KW_COV_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  (ParseTree.getBinaryFun("covariance", numbers_list1, numbers_list2))
fun funcs_float_PROD_11_ACT (LP, RP, exp_arit, KW_GETF, float_list, COMMA, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), exp_arit_SPAN : (Lex.pos * Lex.pos), KW_GETF_SPAN : (Lex.pos * Lex.pos), float_list_SPAN : (Lex.pos * Lex.pos), COMMA_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  (ParseTree.getBinaryFun("getFloat", float_list, exp_arit))
fun funcs_float_PROD_12_ACT (LP, RP, expr, KW_TOFLOAT, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), expr_SPAN : (Lex.pos * Lex.pos), KW_TOFLOAT_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  (ParseTree.getFunctionOne("toFloat", expr))
fun float_list_PROD_1_ACT (ID, ID_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  (ParseTree.Var ID)
fun float_list_PROD_2_ACT (SFLOAT, SFLOAT_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  (ParseTree.floatListToSampleExpr(SFLOAT))
fun string_list_PROD_1_ACT (ID, ID_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  (ParseTree.Var ID)
fun string_list_PROD_2_ACT (SSTRING, SSTRING_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  (ParseTree.stringListToSampleExpr(SSTRING))
fun int_list_PROD_1_ACT (ID, ID_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  (ParseTree.Var ID)
fun int_list_PROD_2_ACT (SINT, SINT_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  (ParseTree.intListToSampleExpr(SINT))
fun numbers_list_PROD_1_ACT (float_list, float_list_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  (float_list)
fun numbers_list_PROD_2_ACT (int_list, int_list_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  (int_list)
fun exp_bool_PROD_1_ACT (rel_op, addExp1, addExp2, rel_op_SPAN : (Lex.pos * Lex.pos), addExp1_SPAN : (Lex.pos * Lex.pos), addExp2_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  (ParseTree.getExprBoolTree(rel_op,addExp1,addExp2))
fun exp_bool_PROD_2_ACT (op_bool, op_bool_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  (op_bool)
fun exp_bool_PROD_3_ACT (atom_bool, atom_bool_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  (atom_bool)
fun exp_tupla_PROD_1_ACT (ID, ID_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  (ParseTree.Var ID)
fun exp_tupla_PROD_2_ACT (TUPLE, TUPLE_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  (ParseTree.Const (Helper.toTupla(Helper.getTupleFrom (TUPLE))))
fun op_bool_PROD_1_ACT (AND, atom_bool1, atom_bool2, AND_SPAN : (Lex.pos * Lex.pos), atom_bool1_SPAN : (Lex.pos * Lex.pos), atom_bool2_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  (ParseTree.FuncTwo(ParseTree.And,atom_bool1,atom_bool2))
fun op_bool_PROD_2_ACT (OR, atom_bool1, atom_bool2, OR_SPAN : (Lex.pos * Lex.pos), atom_bool1_SPAN : (Lex.pos * Lex.pos), atom_bool2_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  (ParseTree.FuncTwo(ParseTree.Or,atom_bool1,atom_bool2))
fun exp_string_PROD_1_ACT (op_str, op_str_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  (op_str)
fun exp_string_PROD_2_ACT (atom_string, atom_string_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  (atom_string)
fun op_str_PROD_1_ACT (atom_string1, atom_string2, CONCAT, atom_string1_SPAN : (Lex.pos * Lex.pos), atom_string2_SPAN : (Lex.pos * Lex.pos), CONCAT_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  (ParseTree.FuncTwo(ParseTree.Concat,atom_string1,atom_string2))
fun atom_string_PROD_1_ACT (ID, ID_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  (ParseTree.Var ID)
fun atom_string_PROD_2_ACT (STR, STR_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  (ParseTree.Const (Grammar.Primitivo (Grammar.String_ STR)))
fun rel_op_PROD_1_ACT (EEQ, EEQ_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  ("==")
fun rel_op_PROD_2_ACT (NEQ, NEQ_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  ("!=")
fun rel_op_PROD_3_ACT (GEQ, GEQ_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  (">=")
fun rel_op_PROD_4_ACT (LEQ, LEQ_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  ("<=")
fun rel_op_PROD_5_ACT (LT, LT_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  ("<")
fun rel_op_PROD_6_ACT (GT, GT_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  (">")
fun atom_bool_PROD_1_ACT (ID, ID_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  (ParseTree.Var ID)
fun atom_bool_PROD_2_ACT (BOOL, BOOL_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  ( (ParseTree.Const (Grammar.Primitivo (Grammar.Boolean_ BOOL))))
fun loop_PROD_1_ACT (SR, KW_WHILE, exp_bool, KW_DO, KW_END, SR_SPAN : (Lex.pos * Lex.pos), KW_WHILE_SPAN : (Lex.pos * Lex.pos), exp_bool_SPAN : (Lex.pos * Lex.pos), KW_DO_SPAN : (Lex.pos * Lex.pos), KW_END_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  (
            let
                val w = (ParseTree.While(exp_bool,SR))
            in
                tree := (w :: (!tree));
                w
            end
        )
fun conditional_PROD_1_ACT (SR1, SR2, exp_bool, KW_ELSE, KW_THEN, KW_IF, KW_END, SR1_SPAN : (Lex.pos * Lex.pos), SR2_SPAN : (Lex.pos * Lex.pos), exp_bool_SPAN : (Lex.pos * Lex.pos), KW_ELSE_SPAN : (Lex.pos * Lex.pos), KW_THEN_SPAN : (Lex.pos * Lex.pos), KW_IF_SPAN : (Lex.pos * Lex.pos), KW_END_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  (
            let
                val ifi = (ParseTree.If(exp_bool,SR1,SR2))
            in
                tree := (ifi :: (!tree));
                ifi
            end
        )
fun case_label_PROD_1_ACT (exp_arit, exp_arit_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  (exp_arit)
fun case_label_PROD_2_ACT (exp_string, exp_string_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  (exp_string)
fun case_expr_PROD_1_ACT (exp_string, exp_string_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  (exp_string)
fun case_expr_PROD_2_ACT (exp_arit, exp_arit_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  (exp_arit)
fun case_expr_PROD_3_ACT (funcs_float, funcs_float_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  (funcs_float)
fun case_expr_PROD_4_ACT (funcs_int, funcs_int_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  (funcs_int)
fun case_expr_PROD_5_ACT (funcs_string, funcs_string_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  (funcs_string)
fun case_element_PROD_1_ACT (SR, COLON, case_label, SR_SPAN : (Lex.pos * Lex.pos), COLON_SPAN : (Lex.pos * Lex.pos), case_label_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  (case_label, SR)
fun case_st_PROD_1_ACT (OF, SR, KW_CASE, case_expr, KW_END, OF_SPAN : (Lex.pos * Lex.pos), SR_SPAN : (Lex.pos * Lex.pos), KW_CASE_SPAN : (Lex.pos * Lex.pos), case_expr_SPAN : (Lex.pos * Lex.pos), KW_END_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  (
            let
                val c = (ParseTree.Case(case_expr, SR))
            in
                tree := (c :: (!tree));
                c
            end
        )
fun exp_arit_PROD_1_ACT (addExp, addExp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  (addExp)
fun exp_arit_PROD_2_ACT (atomicExp, atomicExp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  (atomicExp)
fun addExp_PROD_1_ACT (PLUS, multExp1, multExp2, PLUS_SPAN : (Lex.pos * Lex.pos), multExp1_SPAN : (Lex.pos * Lex.pos), multExp2_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  (ParseTree.FuncTwo(ParseTree.Add,multExp1,multExp2))
fun addExp_PROD_2_ACT (multExp1, multExp2, MINUS, multExp1_SPAN : (Lex.pos * Lex.pos), multExp2_SPAN : (Lex.pos * Lex.pos), MINUS_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  (ParseTree.FuncTwo(ParseTree.Sub,multExp1,multExp2))
fun addExp_PROD_3_ACT (multExp, multExp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  (multExp)
fun multExp_PROD_1_ACT (DIV, rootExp1, rootExp2, DIV_SPAN : (Lex.pos * Lex.pos), rootExp1_SPAN : (Lex.pos * Lex.pos), rootExp2_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  (ParseTree.FuncTwo(ParseTree.Div,rootExp1,rootExp2))
fun multExp_PROD_2_ACT (TIMES, rootExp1, rootExp2, TIMES_SPAN : (Lex.pos * Lex.pos), rootExp1_SPAN : (Lex.pos * Lex.pos), rootExp2_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  (ParseTree.FuncTwo(ParseTree.Mul,rootExp1,rootExp2))
fun multExp_PROD_3_ACT (rootExp, rootExp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  (rootExp)
fun rootExp_PROD_1_ACT (LP, RP, COMMA, KW_RT, prefixExp1, prefixExp2, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), COMMA_SPAN : (Lex.pos * Lex.pos), KW_RT_SPAN : (Lex.pos * Lex.pos), prefixExp1_SPAN : (Lex.pos * Lex.pos), prefixExp2_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  (ParseTree.FuncTwo(ParseTree.RT,prefixExp1,prefixExp2))
fun rootExp_PROD_2_ACT (LP, RP, COMMA, prefixExp1, prefixExp2, KW_POW, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), COMMA_SPAN : (Lex.pos * Lex.pos), prefixExp1_SPAN : (Lex.pos * Lex.pos), prefixExp2_SPAN : (Lex.pos * Lex.pos), KW_POW_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  (ParseTree.FuncTwo(ParseTree.Pow,prefixExp1,prefixExp2))
fun rootExp_PROD_3_ACT (prefixExp, prefixExp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  (prefixExp)
fun prefixExp_PROD_1_ACT (atomicExp, atomicExp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  (atomicExp)
fun prefixExp_PROD_2_ACT (MINUS, prefixExp, MINUS_SPAN : (Lex.pos * Lex.pos), prefixExp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  (ParseTree.FuncTwo(ParseTree.Sub,ParseTree.Const(Grammar.Primitivo (Grammar.Int_ 0)),prefixExp))
fun atomicExp_PROD_1_ACT (ID, ID_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  (ParseTree.Var ID)
fun atomicExp_PROD_2_ACT (NUM, NUM_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  (ParseTree.Const (Grammar.Primitivo (Grammar.Int_ NUM)))
fun atomicExp_PROD_3_ACT (REAL, REAL_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  (ParseTree.Const (Grammar.Primitivo (Grammar.Float_ REAL)))
fun atomicExp_PROD_5_ACT (funcs_float, funcs_float_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  (funcs_float)
fun atomicExp_PROD_6_ACT (funcs_int, funcs_int_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  (funcs_int)
fun variables_PROD_1_ACT (SR, KW_endvars, SR_SPAN : (Lex.pos * Lex.pos), KW_endvars_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  ((fn(_) => ()) SR)
fun declaration_PROD_1_ACT (ID, SEMI, TIPO, ID_SPAN : (Lex.pos * Lex.pos), SEMI_SPAN : (Lex.pos * Lex.pos), TIPO_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  (vars:=ParseTree.insere(!vars,Atom.atom ID,Atom.toString(Atom.atom TIPO));
                      ts:=AtomMap.insert(!ts,Atom.atom ID,TIPO))
fun mkts_REFC() : (string AtomMap.map) ref = ref (AtomMap.empty)
fun mktree_REFC() : (ParseTree.RoseTree) ref = ref (nil)
fun mkvars_REFC() : (tipo AtomMap.map) ref = ref (AtomMap.empty)
      end (* UserCode *)

    structure Err = AntlrErrHandler(
      structure Tok = Tok
      structure Lex = Lex)

(* replace functor with inline structure for better optimization
    structure EBNF = AntlrEBNF(
      struct
	type strm = Err.wstream
	val getSpan = Err.getSpan
      end)
*)
    structure EBNF =
      struct
	fun optional (pred, parse, strm) =
	      if pred strm
		then let
		  val (y, span, strm') = parse strm
		  in
		    (SOME y, span, strm')
		  end
		else (NONE, Err.getSpan strm, strm)

	fun closure (pred, parse, strm) = let
	      fun iter (strm, (left, right), ys) =
		    if pred strm
		      then let
			val (y, (_, right'), strm') = parse strm
			in iter (strm', (left, right'), y::ys)
			end
		      else (List.rev ys, (left, right), strm)
	      in
		iter (strm, Err.getSpan strm, [])
	      end

	fun posclos (pred, parse, strm) = let
	      val (y, (left, _), strm') = parse strm
	      val (ys, (_, right), strm'') = closure (pred, parse, strm')
	      in
		(y::ys, (left, right), strm'')
	      end
      end

    fun mk lexFn = let
val ts_REFC = UserCode.mkts_REFC()
val tree_REFC = UserCode.mktree_REFC()
val vars_REFC = UserCode.mkvars_REFC()
fun getS() = {ts = !ts_REFC, tree = !tree_REFC, vars = !vars_REFC}
fun putS{ts, tree, vars} = (ts_REFC := ts; tree_REFC := tree; vars_REFC := vars)
fun unwrap (ret, strm, repairs) = (ret, strm, repairs, getS())
        val (eh, lex) = Err.mkErrHandler {get = getS, put = putS}
	fun fail() = Err.failure eh
	fun tryProds (strm, prods) = let
	  fun try [] = fail()
	    | try (prod :: prods) =
	        (Err.whileDisabled eh (fn() => prod strm))
		handle Err.ParseError => try (prods)
          in try prods end
fun matchKW_let strm = (case (lex(strm))
 of (Tok.KW_let, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_in strm = (case (lex(strm))
 of (Tok.KW_in, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_title strm = (case (lex(strm))
 of (Tok.KW_title, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchSSTRING strm = (case (lex(strm))
 of (Tok.SSTRING(x), span, strm') => (x, span, strm')
  | _ => fail()
(* end case *))
fun matchID strm = (case (lex(strm))
 of (Tok.ID(x), span, strm') => (x, span, strm')
  | _ => fail()
(* end case *))
fun matchNUM strm = (case (lex(strm))
 of (Tok.NUM(x), span, strm') => (x, span, strm')
  | _ => fail()
(* end case *))
fun matchREAL strm = (case (lex(strm))
 of (Tok.REAL(x), span, strm') => (x, span, strm')
  | _ => fail()
(* end case *))
fun matchSINT strm = (case (lex(strm))
 of (Tok.SINT(x), span, strm') => (x, span, strm')
  | _ => fail()
(* end case *))
fun matchPLUS strm = (case (lex(strm))
 of (Tok.PLUS, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchEEQ strm = (case (lex(strm))
 of (Tok.EEQ, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchSFLOAT strm = (case (lex(strm))
 of (Tok.SFLOAT(x), span, strm') => (x, span, strm')
  | _ => fail()
(* end case *))
fun matchTIMES strm = (case (lex(strm))
 of (Tok.TIMES, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchDIV strm = (case (lex(strm))
 of (Tok.DIV, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchMINUS strm = (case (lex(strm))
 of (Tok.MINUS, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchCOMMA strm = (case (lex(strm))
 of (Tok.COMMA, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchSBOOL strm = (case (lex(strm))
 of (Tok.SBOOL(x), span, strm') => (x, span, strm')
  | _ => fail()
(* end case *))
fun matchLP strm = (case (lex(strm))
 of (Tok.LP, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchRP strm = (case (lex(strm))
 of (Tok.RP, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchBOOL strm = (case (lex(strm))
 of (Tok.BOOL(x), span, strm') => (x, span, strm')
  | _ => fail()
(* end case *))
fun matchAND strm = (case (lex(strm))
 of (Tok.AND, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchOR strm = (case (lex(strm))
 of (Tok.OR, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchSPACE strm = (case (lex(strm))
 of (Tok.SPACE, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchGT strm = (case (lex(strm))
 of (Tok.GT, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchLT strm = (case (lex(strm))
 of (Tok.LT, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchLEQ strm = (case (lex(strm))
 of (Tok.LEQ, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchGEQ strm = (case (lex(strm))
 of (Tok.GEQ, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchNEQ strm = (case (lex(strm))
 of (Tok.NEQ, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_variables strm = (case (lex(strm))
 of (Tok.KW_variables, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchSEMI strm = (case (lex(strm))
 of (Tok.SEMI, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchTIPO strm = (case (lex(strm))
 of (Tok.TIPO(x), span, strm') => (x, span, strm')
  | _ => fail()
(* end case *))
fun matchDOTDOT strm = (case (lex(strm))
 of (Tok.DOTDOT, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchCOLON strm = (case (lex(strm))
 of (Tok.COLON, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_commands strm = (case (lex(strm))
 of (Tok.KW_commands, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchSTR strm = (case (lex(strm))
 of (Tok.STR(x), span, strm') => (x, span, strm')
  | _ => fail()
(* end case *))
fun matchKW_Print strm = (case (lex(strm))
 of (Tok.KW_Print, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_endvars strm = (case (lex(strm))
 of (Tok.KW_endvars, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_terminate strm = (case (lex(strm))
 of (Tok.KW_terminate, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_SUM strm = (case (lex(strm))
 of (Tok.KW_SUM, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_PROD strm = (case (lex(strm))
 of (Tok.KW_PROD, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchEMPTY strm = (case (lex(strm))
 of (Tok.EMPTY, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_GETS strm = (case (lex(strm))
 of (Tok.KW_GETS, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_IF strm = (case (lex(strm))
 of (Tok.KW_IF, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_THEN strm = (case (lex(strm))
 of (Tok.KW_THEN, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_ELSE strm = (case (lex(strm))
 of (Tok.KW_ELSE, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_WHILE strm = (case (lex(strm))
 of (Tok.KW_WHILE, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_CASE strm = (case (lex(strm))
 of (Tok.KW_CASE, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchOF strm = (case (lex(strm))
 of (Tok.OF, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_DO strm = (case (lex(strm))
 of (Tok.KW_DO, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_END strm = (case (lex(strm))
 of (Tok.KW_END, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_TOSTRING strm = (case (lex(strm))
 of (Tok.KW_TOSTRING, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_MEAN strm = (case (lex(strm))
 of (Tok.KW_MEAN, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_CORR strm = (case (lex(strm))
 of (Tok.KW_CORR, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_MEDIAN strm = (case (lex(strm))
 of (Tok.KW_MEDIAN, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_STDEV strm = (case (lex(strm))
 of (Tok.KW_STDEV, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_VAR strm = (case (lex(strm))
 of (Tok.KW_VAR, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_RT strm = (case (lex(strm))
 of (Tok.KW_RT, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_POW strm = (case (lex(strm))
 of (Tok.KW_POW, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_GETF strm = (case (lex(strm))
 of (Tok.KW_GETF, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_COV strm = (case (lex(strm))
 of (Tok.KW_COV, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_LINREG strm = (case (lex(strm))
 of (Tok.KW_LINREG, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchVOID strm = (case (lex(strm))
 of (Tok.VOID, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchTUPLE strm = (case (lex(strm))
 of (Tok.TUPLE(x), span, strm') => (x, span, strm')
  | _ => fail()
(* end case *))
fun matchSTUPLE strm = (case (lex(strm))
 of (Tok.STUPLE(x), span, strm') => (x, span, strm')
  | _ => fail()
(* end case *))
fun matchKW_GETI strm = (case (lex(strm))
 of (Tok.KW_GETI, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_TOFLOAT strm = (case (lex(strm))
 of (Tok.KW_TOFLOAT, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_TOINT strm = (case (lex(strm))
 of (Tok.KW_TOINT, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchCONCAT strm = (case (lex(strm))
 of (Tok.CONCAT, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchEOF strm = (case (lex(strm))
 of (Tok.EOF, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))

val (program_NT) = 
let
fun val_list_NT (strm) = let
      fun val_list_PROD_1 (strm) = let
            val (SINT_RES, SINT_SPAN, strm') = matchSINT(strm)
            val FULL_SPAN = (#1(SINT_SPAN), #2(SINT_SPAN))
            in
              (UserCode.val_list_PROD_1_ACT (SINT_RES, SINT_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts_REFC, tree_REFC, vars_REFC),
                FULL_SPAN, strm')
            end
      fun val_list_PROD_2 (strm) = let
            val (SFLOAT_RES, SFLOAT_SPAN, strm') = matchSFLOAT(strm)
            val FULL_SPAN = (#1(SFLOAT_SPAN), #2(SFLOAT_SPAN))
            in
              (UserCode.val_list_PROD_2_ACT (SFLOAT_RES, SFLOAT_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts_REFC, tree_REFC, vars_REFC),
                FULL_SPAN, strm')
            end
      fun val_list_PROD_3 (strm) = let
            val (SBOOL_RES, SBOOL_SPAN, strm') = matchSBOOL(strm)
            val FULL_SPAN = (#1(SBOOL_SPAN), #2(SBOOL_SPAN))
            in
              (UserCode.val_list_PROD_3_ACT (SBOOL_RES, SBOOL_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts_REFC, tree_REFC, vars_REFC),
                FULL_SPAN, strm')
            end
      fun val_list_PROD_4 (strm) = let
            val (SSTRING_RES, SSTRING_SPAN, strm') = matchSSTRING(strm)
            val FULL_SPAN = (#1(SSTRING_SPAN), #2(SSTRING_SPAN))
            in
              (UserCode.val_list_PROD_4_ACT (SSTRING_RES, SSTRING_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts_REFC, tree_REFC, vars_REFC),
                FULL_SPAN, strm')
            end
      fun val_list_PROD_5 (strm) = let
            val (STUPLE_RES, STUPLE_SPAN, strm') = matchSTUPLE(strm)
            val FULL_SPAN = (#1(STUPLE_SPAN), #2(STUPLE_SPAN))
            in
              (UserCode.val_list_PROD_5_ACT (STUPLE_RES, STUPLE_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts_REFC, tree_REFC, vars_REFC),
                FULL_SPAN, strm')
            end
      fun val_list_PROD_6 (strm) = let
            val (ID_RES, ID_SPAN, strm') = matchID(strm)
            val FULL_SPAN = (#1(ID_SPAN), #2(ID_SPAN))
            in
              (UserCode.val_list_PROD_6_ACT (ID_RES, ID_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts_REFC, tree_REFC, vars_REFC),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.ID(_), _, strm') => val_list_PROD_6(strm)
          | (Tok.SSTRING(_), _, strm') => val_list_PROD_4(strm)
          | (Tok.SFLOAT(_), _, strm') => val_list_PROD_2(strm)
          | (Tok.SINT(_), _, strm') => val_list_PROD_1(strm)
          | (Tok.SBOOL(_), _, strm') => val_list_PROD_3(strm)
          | (Tok.STUPLE(_), _, strm') => val_list_PROD_5(strm)
          | _ => fail()
        (* end case *))
      end
fun int_list_NT (strm) = let
      fun int_list_PROD_1 (strm) = let
            val (ID_RES, ID_SPAN, strm') = matchID(strm)
            val FULL_SPAN = (#1(ID_SPAN), #2(ID_SPAN))
            in
              (UserCode.int_list_PROD_1_ACT (ID_RES, ID_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts_REFC, tree_REFC, vars_REFC),
                FULL_SPAN, strm')
            end
      fun int_list_PROD_2 (strm) = let
            val (SINT_RES, SINT_SPAN, strm') = matchSINT(strm)
            val FULL_SPAN = (#1(SINT_SPAN), #2(SINT_SPAN))
            in
              (UserCode.int_list_PROD_2_ACT (SINT_RES, SINT_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts_REFC, tree_REFC, vars_REFC),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.SINT(_), _, strm') => int_list_PROD_2(strm)
          | (Tok.ID(_), _, strm') => int_list_PROD_1(strm)
          | _ => fail()
        (* end case *))
      end
fun float_list_NT (strm) = let
      fun float_list_PROD_1 (strm) = let
            val (ID_RES, ID_SPAN, strm') = matchID(strm)
            val FULL_SPAN = (#1(ID_SPAN), #2(ID_SPAN))
            in
              (UserCode.float_list_PROD_1_ACT (ID_RES, ID_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts_REFC, tree_REFC, vars_REFC),
                FULL_SPAN, strm')
            end
      fun float_list_PROD_2 (strm) = let
            val (SFLOAT_RES, SFLOAT_SPAN, strm') = matchSFLOAT(strm)
            val FULL_SPAN = (#1(SFLOAT_SPAN), #2(SFLOAT_SPAN))
            in
              (UserCode.float_list_PROD_2_ACT (SFLOAT_RES, SFLOAT_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts_REFC, tree_REFC, vars_REFC),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.SFLOAT(_), _, strm') => float_list_PROD_2(strm)
          | (Tok.ID(_), _, strm') => float_list_PROD_1(strm)
          | _ => fail()
        (* end case *))
      end
fun numbers_list_NT (strm) = let
      fun numbers_list_PROD_1 (strm) = let
            val (float_list_RES, float_list_SPAN, strm') = float_list_NT(strm)
            val FULL_SPAN = (#1(float_list_SPAN), #2(float_list_SPAN))
            in
              (UserCode.numbers_list_PROD_1_ACT (float_list_RES, float_list_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts_REFC, tree_REFC, vars_REFC),
                FULL_SPAN, strm')
            end
      fun numbers_list_PROD_2 (strm) = let
            val (int_list_RES, int_list_SPAN, strm') = int_list_NT(strm)
            val FULL_SPAN = (#1(int_list_SPAN), #2(int_list_SPAN))
            in
              (UserCode.numbers_list_PROD_2_ACT (int_list_RES, int_list_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts_REFC, tree_REFC, vars_REFC),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.SINT(_), _, strm') => numbers_list_PROD_2(strm)
          | (Tok.ID(_), _, strm') =>
              tryProds(strm, [numbers_list_PROD_1, numbers_list_PROD_2])
          | (Tok.SFLOAT(_), _, strm') => numbers_list_PROD_1(strm)
          | _ => fail()
        (* end case *))
      end
fun string_list_NT (strm) = let
      fun string_list_PROD_1 (strm) = let
            val (ID_RES, ID_SPAN, strm') = matchID(strm)
            val FULL_SPAN = (#1(ID_SPAN), #2(ID_SPAN))
            in
              (UserCode.string_list_PROD_1_ACT (ID_RES, ID_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts_REFC, tree_REFC, vars_REFC),
                FULL_SPAN, strm')
            end
      fun string_list_PROD_2 (strm) = let
            val (SSTRING_RES, SSTRING_SPAN, strm') = matchSSTRING(strm)
            val FULL_SPAN = (#1(SSTRING_SPAN), #2(SSTRING_SPAN))
            in
              (UserCode.string_list_PROD_2_ACT (SSTRING_RES, SSTRING_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts_REFC, tree_REFC, vars_REFC),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.SSTRING(_), _, strm') => string_list_PROD_2(strm)
          | (Tok.ID(_), _, strm') => string_list_PROD_1(strm)
          | _ => fail()
        (* end case *))
      end
fun exp_tupla_NT (strm) = let
      fun exp_tupla_PROD_1 (strm) = let
            val (ID_RES, ID_SPAN, strm') = matchID(strm)
            val FULL_SPAN = (#1(ID_SPAN), #2(ID_SPAN))
            in
              (UserCode.exp_tupla_PROD_1_ACT (ID_RES, ID_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts_REFC, tree_REFC, vars_REFC),
                FULL_SPAN, strm')
            end
      fun exp_tupla_PROD_2 (strm) = let
            val (TUPLE_RES, TUPLE_SPAN, strm') = matchTUPLE(strm)
            val FULL_SPAN = (#1(TUPLE_SPAN), #2(TUPLE_SPAN))
            in
              (UserCode.exp_tupla_PROD_2_ACT (TUPLE_RES, TUPLE_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts_REFC, tree_REFC, vars_REFC),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.TUPLE(_), _, strm') => exp_tupla_PROD_2(strm)
          | (Tok.ID(_), _, strm') => exp_tupla_PROD_1(strm)
          | _ => fail()
        (* end case *))
      end
fun rel_op_NT (strm) = let
      fun rel_op_PROD_1 (strm) = let
            val (EEQ_RES, EEQ_SPAN, strm') = matchEEQ(strm)
            val FULL_SPAN = (#1(EEQ_SPAN), #2(EEQ_SPAN))
            in
              (UserCode.rel_op_PROD_1_ACT (EEQ_RES, EEQ_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts_REFC, tree_REFC, vars_REFC),
                FULL_SPAN, strm')
            end
      fun rel_op_PROD_2 (strm) = let
            val (NEQ_RES, NEQ_SPAN, strm') = matchNEQ(strm)
            val FULL_SPAN = (#1(NEQ_SPAN), #2(NEQ_SPAN))
            in
              (UserCode.rel_op_PROD_2_ACT (NEQ_RES, NEQ_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts_REFC, tree_REFC, vars_REFC),
                FULL_SPAN, strm')
            end
      fun rel_op_PROD_3 (strm) = let
            val (GEQ_RES, GEQ_SPAN, strm') = matchGEQ(strm)
            val FULL_SPAN = (#1(GEQ_SPAN), #2(GEQ_SPAN))
            in
              (UserCode.rel_op_PROD_3_ACT (GEQ_RES, GEQ_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts_REFC, tree_REFC, vars_REFC),
                FULL_SPAN, strm')
            end
      fun rel_op_PROD_4 (strm) = let
            val (LEQ_RES, LEQ_SPAN, strm') = matchLEQ(strm)
            val FULL_SPAN = (#1(LEQ_SPAN), #2(LEQ_SPAN))
            in
              (UserCode.rel_op_PROD_4_ACT (LEQ_RES, LEQ_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts_REFC, tree_REFC, vars_REFC),
                FULL_SPAN, strm')
            end
      fun rel_op_PROD_5 (strm) = let
            val (LT_RES, LT_SPAN, strm') = matchLT(strm)
            val FULL_SPAN = (#1(LT_SPAN), #2(LT_SPAN))
            in
              (UserCode.rel_op_PROD_5_ACT (LT_RES, LT_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts_REFC, tree_REFC, vars_REFC),
                FULL_SPAN, strm')
            end
      fun rel_op_PROD_6 (strm) = let
            val (GT_RES, GT_SPAN, strm') = matchGT(strm)
            val FULL_SPAN = (#1(GT_SPAN), #2(GT_SPAN))
            in
              (UserCode.rel_op_PROD_6_ACT (GT_RES, GT_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts_REFC, tree_REFC, vars_REFC),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.GT, _, strm') => rel_op_PROD_6(strm)
          | (Tok.LEQ, _, strm') => rel_op_PROD_4(strm)
          | (Tok.NEQ, _, strm') => rel_op_PROD_2(strm)
          | (Tok.EEQ, _, strm') => rel_op_PROD_1(strm)
          | (Tok.GEQ, _, strm') => rel_op_PROD_3(strm)
          | (Tok.LT, _, strm') => rel_op_PROD_5(strm)
          | _ => fail()
        (* end case *))
      end
fun exp_bool_NT (strm) = let
      fun exp_bool_PROD_1 (strm) = let
            val (addExp1_RES, addExp1_SPAN, strm') = addExp_NT(strm)
            val (rel_op_RES, rel_op_SPAN, strm') = rel_op_NT(strm')
            val (addExp2_RES, addExp2_SPAN, strm') = addExp_NT(strm')
            val FULL_SPAN = (#1(addExp1_SPAN), #2(addExp2_SPAN))
            in
              (UserCode.exp_bool_PROD_1_ACT (rel_op_RES, addExp1_RES, addExp2_RES, rel_op_SPAN : (Lex.pos * Lex.pos), addExp1_SPAN : (Lex.pos * Lex.pos), addExp2_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts_REFC, tree_REFC, vars_REFC),
                FULL_SPAN, strm')
            end
      fun exp_bool_PROD_2 (strm) = let
            val (op_bool_RES, op_bool_SPAN, strm') = op_bool_NT(strm)
            val FULL_SPAN = (#1(op_bool_SPAN), #2(op_bool_SPAN))
            in
              (UserCode.exp_bool_PROD_2_ACT (op_bool_RES, op_bool_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts_REFC, tree_REFC, vars_REFC),
                FULL_SPAN, strm')
            end
      fun exp_bool_PROD_3 (strm) = let
            val (atom_bool_RES, atom_bool_SPAN, strm') = atom_bool_NT(strm)
            val FULL_SPAN = (#1(atom_bool_SPAN), #2(atom_bool_SPAN))
            in
              (UserCode.exp_bool_PROD_3_ACT (atom_bool_RES, atom_bool_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts_REFC, tree_REFC, vars_REFC),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.NUM(_), _, strm') => exp_bool_PROD_1(strm)
          | (Tok.REAL(_), _, strm') => exp_bool_PROD_1(strm)
          | (Tok.MINUS, _, strm') => exp_bool_PROD_1(strm)
          | (Tok.KW_SUM, _, strm') => exp_bool_PROD_1(strm)
          | (Tok.KW_PROD, _, strm') => exp_bool_PROD_1(strm)
          | (Tok.KW_MEAN, _, strm') => exp_bool_PROD_1(strm)
          | (Tok.KW_CORR, _, strm') => exp_bool_PROD_1(strm)
          | (Tok.KW_MEDIAN, _, strm') => exp_bool_PROD_1(strm)
          | (Tok.KW_STDEV, _, strm') => exp_bool_PROD_1(strm)
          | (Tok.KW_VAR, _, strm') => exp_bool_PROD_1(strm)
          | (Tok.KW_RT, _, strm') => exp_bool_PROD_1(strm)
          | (Tok.KW_POW, _, strm') => exp_bool_PROD_1(strm)
          | (Tok.KW_GETF, _, strm') => exp_bool_PROD_1(strm)
          | (Tok.KW_COV, _, strm') => exp_bool_PROD_1(strm)
          | (Tok.KW_GETI, _, strm') => exp_bool_PROD_1(strm)
          | (Tok.KW_TOFLOAT, _, strm') => exp_bool_PROD_1(strm)
          | (Tok.KW_TOINT, _, strm') => exp_bool_PROD_1(strm)
          | (Tok.ID(_), _, strm') =>
              tryProds(strm, [exp_bool_PROD_1, exp_bool_PROD_2,
                exp_bool_PROD_3])
          | (Tok.LP, _, strm') =>
              tryProds(strm, [exp_bool_PROD_1, exp_bool_PROD_2,
                exp_bool_PROD_3])
          | (Tok.BOOL(_), _, strm') =>
              tryProds(strm, [exp_bool_PROD_2, exp_bool_PROD_3])
          | _ => fail()
        (* end case *))
      end
and atom_bool_NT (strm) = let
      fun atom_bool_PROD_1 (strm) = let
            val (ID_RES, ID_SPAN, strm') = matchID(strm)
            val FULL_SPAN = (#1(ID_SPAN), #2(ID_SPAN))
            in
              (UserCode.atom_bool_PROD_1_ACT (ID_RES, ID_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts_REFC, tree_REFC, vars_REFC),
                FULL_SPAN, strm')
            end
      fun atom_bool_PROD_2 (strm) = let
            val (BOOL_RES, BOOL_SPAN, strm') = matchBOOL(strm)
            val FULL_SPAN = (#1(BOOL_SPAN), #2(BOOL_SPAN))
            in
              (UserCode.atom_bool_PROD_2_ACT (BOOL_RES, BOOL_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts_REFC, tree_REFC, vars_REFC),
                FULL_SPAN, strm')
            end
      fun atom_bool_PROD_3 (strm) = let
            val (LP_RES, LP_SPAN, strm') = matchLP(strm)
            val (exp_bool_RES, exp_bool_SPAN, strm') = exp_bool_NT(strm')
            val (RP_RES, RP_SPAN, strm') = matchRP(strm')
            val FULL_SPAN = (#1(LP_SPAN), #2(RP_SPAN))
            in
              ((exp_bool_RES), FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.LP, _, strm') => atom_bool_PROD_3(strm)
          | (Tok.ID(_), _, strm') => atom_bool_PROD_1(strm)
          | (Tok.BOOL(_), _, strm') => atom_bool_PROD_2(strm)
          | _ => fail()
        (* end case *))
      end
and op_bool_NT (strm) = let
      fun op_bool_PROD_1 (strm) = let
            val (atom_bool1_RES, atom_bool1_SPAN, strm') = atom_bool_NT(strm)
            val (AND_RES, AND_SPAN, strm') = matchAND(strm')
            val (atom_bool2_RES, atom_bool2_SPAN, strm') = atom_bool_NT(strm')
            val FULL_SPAN = (#1(atom_bool1_SPAN), #2(atom_bool2_SPAN))
            in
              (UserCode.op_bool_PROD_1_ACT (AND_RES, atom_bool1_RES, atom_bool2_RES, AND_SPAN : (Lex.pos * Lex.pos), atom_bool1_SPAN : (Lex.pos * Lex.pos), atom_bool2_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts_REFC, tree_REFC, vars_REFC),
                FULL_SPAN, strm')
            end
      fun op_bool_PROD_2 (strm) = let
            val (atom_bool1_RES, atom_bool1_SPAN, strm') = atom_bool_NT(strm)
            val (OR_RES, OR_SPAN, strm') = matchOR(strm')
            val (atom_bool2_RES, atom_bool2_SPAN, strm') = atom_bool_NT(strm')
            val FULL_SPAN = (#1(atom_bool1_SPAN), #2(atom_bool2_SPAN))
            in
              (UserCode.op_bool_PROD_2_ACT (OR_RES, atom_bool1_RES, atom_bool2_RES, OR_SPAN : (Lex.pos * Lex.pos), atom_bool1_SPAN : (Lex.pos * Lex.pos), atom_bool2_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts_REFC, tree_REFC, vars_REFC),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.ID(_), _, strm') =>
              tryProds(strm, [op_bool_PROD_1, op_bool_PROD_2])
          | (Tok.LP, _, strm') =>
              tryProds(strm, [op_bool_PROD_1, op_bool_PROD_2])
          | (Tok.BOOL(_), _, strm') =>
              tryProds(strm, [op_bool_PROD_1, op_bool_PROD_2])
          | _ => fail()
        (* end case *))
      end
and addExp_NT (strm) = let
      fun addExp_PROD_1 (strm) = let
            val (multExp1_RES, multExp1_SPAN, strm') = multExp_NT(strm)
            val (PLUS_RES, PLUS_SPAN, strm') = matchPLUS(strm')
            val (multExp2_RES, multExp2_SPAN, strm') = multExp_NT(strm')
            val FULL_SPAN = (#1(multExp1_SPAN), #2(multExp2_SPAN))
            in
              (UserCode.addExp_PROD_1_ACT (PLUS_RES, multExp1_RES, multExp2_RES, PLUS_SPAN : (Lex.pos * Lex.pos), multExp1_SPAN : (Lex.pos * Lex.pos), multExp2_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts_REFC, tree_REFC, vars_REFC),
                FULL_SPAN, strm')
            end
      fun addExp_PROD_2 (strm) = let
            val (multExp1_RES, multExp1_SPAN, strm') = multExp_NT(strm)
            val (MINUS_RES, MINUS_SPAN, strm') = matchMINUS(strm')
            val (multExp2_RES, multExp2_SPAN, strm') = multExp_NT(strm')
            val FULL_SPAN = (#1(multExp1_SPAN), #2(multExp2_SPAN))
            in
              (UserCode.addExp_PROD_2_ACT (multExp1_RES, multExp2_RES, MINUS_RES, multExp1_SPAN : (Lex.pos * Lex.pos), multExp2_SPAN : (Lex.pos * Lex.pos), MINUS_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts_REFC, tree_REFC, vars_REFC),
                FULL_SPAN, strm')
            end
      fun addExp_PROD_3 (strm) = let
            val (multExp_RES, multExp_SPAN, strm') = multExp_NT(strm)
            val FULL_SPAN = (#1(multExp_SPAN), #2(multExp_SPAN))
            in
              (UserCode.addExp_PROD_3_ACT (multExp_RES, multExp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts_REFC, tree_REFC, vars_REFC),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.ID(_), _, strm') =>
              tryProds(strm, [addExp_PROD_1, addExp_PROD_2, addExp_PROD_3])
          | (Tok.NUM(_), _, strm') =>
              tryProds(strm, [addExp_PROD_1, addExp_PROD_2, addExp_PROD_3])
          | (Tok.REAL(_), _, strm') =>
              tryProds(strm, [addExp_PROD_1, addExp_PROD_2, addExp_PROD_3])
          | (Tok.MINUS, _, strm') =>
              tryProds(strm, [addExp_PROD_1, addExp_PROD_2, addExp_PROD_3])
          | (Tok.LP, _, strm') =>
              tryProds(strm, [addExp_PROD_1, addExp_PROD_2, addExp_PROD_3])
          | (Tok.KW_SUM, _, strm') =>
              tryProds(strm, [addExp_PROD_1, addExp_PROD_2, addExp_PROD_3])
          | (Tok.KW_PROD, _, strm') =>
              tryProds(strm, [addExp_PROD_1, addExp_PROD_2, addExp_PROD_3])
          | (Tok.KW_MEAN, _, strm') =>
              tryProds(strm, [addExp_PROD_1, addExp_PROD_2, addExp_PROD_3])
          | (Tok.KW_CORR, _, strm') =>
              tryProds(strm, [addExp_PROD_1, addExp_PROD_2, addExp_PROD_3])
          | (Tok.KW_MEDIAN, _, strm') =>
              tryProds(strm, [addExp_PROD_1, addExp_PROD_2, addExp_PROD_3])
          | (Tok.KW_STDEV, _, strm') =>
              tryProds(strm, [addExp_PROD_1, addExp_PROD_2, addExp_PROD_3])
          | (Tok.KW_VAR, _, strm') =>
              tryProds(strm, [addExp_PROD_1, addExp_PROD_2, addExp_PROD_3])
          | (Tok.KW_RT, _, strm') =>
              tryProds(strm, [addExp_PROD_1, addExp_PROD_2, addExp_PROD_3])
          | (Tok.KW_POW, _, strm') =>
              tryProds(strm, [addExp_PROD_1, addExp_PROD_2, addExp_PROD_3])
          | (Tok.KW_GETF, _, strm') =>
              tryProds(strm, [addExp_PROD_1, addExp_PROD_2, addExp_PROD_3])
          | (Tok.KW_COV, _, strm') =>
              tryProds(strm, [addExp_PROD_1, addExp_PROD_2, addExp_PROD_3])
          | (Tok.KW_GETI, _, strm') =>
              tryProds(strm, [addExp_PROD_1, addExp_PROD_2, addExp_PROD_3])
          | (Tok.KW_TOFLOAT, _, strm') =>
              tryProds(strm, [addExp_PROD_1, addExp_PROD_2, addExp_PROD_3])
          | (Tok.KW_TOINT, _, strm') =>
              tryProds(strm, [addExp_PROD_1, addExp_PROD_2, addExp_PROD_3])
          | _ => fail()
        (* end case *))
      end
and multExp_NT (strm) = let
      fun multExp_PROD_1 (strm) = let
            val (rootExp1_RES, rootExp1_SPAN, strm') = rootExp_NT(strm)
            val (DIV_RES, DIV_SPAN, strm') = matchDIV(strm')
            val (rootExp2_RES, rootExp2_SPAN, strm') = rootExp_NT(strm')
            val FULL_SPAN = (#1(rootExp1_SPAN), #2(rootExp2_SPAN))
            in
              (UserCode.multExp_PROD_1_ACT (DIV_RES, rootExp1_RES, rootExp2_RES, DIV_SPAN : (Lex.pos * Lex.pos), rootExp1_SPAN : (Lex.pos * Lex.pos), rootExp2_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts_REFC, tree_REFC, vars_REFC),
                FULL_SPAN, strm')
            end
      fun multExp_PROD_2 (strm) = let
            val (rootExp1_RES, rootExp1_SPAN, strm') = rootExp_NT(strm)
            val (TIMES_RES, TIMES_SPAN, strm') = matchTIMES(strm')
            val (rootExp2_RES, rootExp2_SPAN, strm') = rootExp_NT(strm')
            val FULL_SPAN = (#1(rootExp1_SPAN), #2(rootExp2_SPAN))
            in
              (UserCode.multExp_PROD_2_ACT (TIMES_RES, rootExp1_RES, rootExp2_RES, TIMES_SPAN : (Lex.pos * Lex.pos), rootExp1_SPAN : (Lex.pos * Lex.pos), rootExp2_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts_REFC, tree_REFC, vars_REFC),
                FULL_SPAN, strm')
            end
      fun multExp_PROD_3 (strm) = let
            val (rootExp_RES, rootExp_SPAN, strm') = rootExp_NT(strm)
            val FULL_SPAN = (#1(rootExp_SPAN), #2(rootExp_SPAN))
            in
              (UserCode.multExp_PROD_3_ACT (rootExp_RES, rootExp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts_REFC, tree_REFC, vars_REFC),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.ID(_), _, strm') =>
              tryProds(strm, [multExp_PROD_1, multExp_PROD_2, multExp_PROD_3])
          | (Tok.NUM(_), _, strm') =>
              tryProds(strm, [multExp_PROD_1, multExp_PROD_2, multExp_PROD_3])
          | (Tok.REAL(_), _, strm') =>
              tryProds(strm, [multExp_PROD_1, multExp_PROD_2, multExp_PROD_3])
          | (Tok.MINUS, _, strm') =>
              tryProds(strm, [multExp_PROD_1, multExp_PROD_2, multExp_PROD_3])
          | (Tok.LP, _, strm') =>
              tryProds(strm, [multExp_PROD_1, multExp_PROD_2, multExp_PROD_3])
          | (Tok.KW_SUM, _, strm') =>
              tryProds(strm, [multExp_PROD_1, multExp_PROD_2, multExp_PROD_3])
          | (Tok.KW_PROD, _, strm') =>
              tryProds(strm, [multExp_PROD_1, multExp_PROD_2, multExp_PROD_3])
          | (Tok.KW_MEAN, _, strm') =>
              tryProds(strm, [multExp_PROD_1, multExp_PROD_2, multExp_PROD_3])
          | (Tok.KW_CORR, _, strm') =>
              tryProds(strm, [multExp_PROD_1, multExp_PROD_2, multExp_PROD_3])
          | (Tok.KW_MEDIAN, _, strm') =>
              tryProds(strm, [multExp_PROD_1, multExp_PROD_2, multExp_PROD_3])
          | (Tok.KW_STDEV, _, strm') =>
              tryProds(strm, [multExp_PROD_1, multExp_PROD_2, multExp_PROD_3])
          | (Tok.KW_VAR, _, strm') =>
              tryProds(strm, [multExp_PROD_1, multExp_PROD_2, multExp_PROD_3])
          | (Tok.KW_RT, _, strm') =>
              tryProds(strm, [multExp_PROD_1, multExp_PROD_2, multExp_PROD_3])
          | (Tok.KW_POW, _, strm') =>
              tryProds(strm, [multExp_PROD_1, multExp_PROD_2, multExp_PROD_3])
          | (Tok.KW_GETF, _, strm') =>
              tryProds(strm, [multExp_PROD_1, multExp_PROD_2, multExp_PROD_3])
          | (Tok.KW_COV, _, strm') =>
              tryProds(strm, [multExp_PROD_1, multExp_PROD_2, multExp_PROD_3])
          | (Tok.KW_GETI, _, strm') =>
              tryProds(strm, [multExp_PROD_1, multExp_PROD_2, multExp_PROD_3])
          | (Tok.KW_TOFLOAT, _, strm') =>
              tryProds(strm, [multExp_PROD_1, multExp_PROD_2, multExp_PROD_3])
          | (Tok.KW_TOINT, _, strm') =>
              tryProds(strm, [multExp_PROD_1, multExp_PROD_2, multExp_PROD_3])
          | _ => fail()
        (* end case *))
      end
and rootExp_NT (strm) = let
      fun rootExp_PROD_1 (strm) = let
            val (KW_RT_RES, KW_RT_SPAN, strm') = matchKW_RT(strm)
            val (LP_RES, LP_SPAN, strm') = matchLP(strm')
            val (prefixExp1_RES, prefixExp1_SPAN, strm') = prefixExp_NT(strm')
            val (COMMA_RES, COMMA_SPAN, strm') = matchCOMMA(strm')
            val (prefixExp2_RES, prefixExp2_SPAN, strm') = prefixExp_NT(strm')
            val (RP_RES, RP_SPAN, strm') = matchRP(strm')
            val FULL_SPAN = (#1(KW_RT_SPAN), #2(RP_SPAN))
            in
              (UserCode.rootExp_PROD_1_ACT (LP_RES, RP_RES, COMMA_RES, KW_RT_RES, prefixExp1_RES, prefixExp2_RES, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), COMMA_SPAN : (Lex.pos * Lex.pos), KW_RT_SPAN : (Lex.pos * Lex.pos), prefixExp1_SPAN : (Lex.pos * Lex.pos), prefixExp2_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts_REFC, tree_REFC, vars_REFC),
                FULL_SPAN, strm')
            end
      fun rootExp_PROD_2 (strm) = let
            val (KW_POW_RES, KW_POW_SPAN, strm') = matchKW_POW(strm)
            val (LP_RES, LP_SPAN, strm') = matchLP(strm')
            val (prefixExp1_RES, prefixExp1_SPAN, strm') = prefixExp_NT(strm')
            val (COMMA_RES, COMMA_SPAN, strm') = matchCOMMA(strm')
            val (prefixExp2_RES, prefixExp2_SPAN, strm') = prefixExp_NT(strm')
            val (RP_RES, RP_SPAN, strm') = matchRP(strm')
            val FULL_SPAN = (#1(KW_POW_SPAN), #2(RP_SPAN))
            in
              (UserCode.rootExp_PROD_2_ACT (LP_RES, RP_RES, COMMA_RES, prefixExp1_RES, prefixExp2_RES, KW_POW_RES, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), COMMA_SPAN : (Lex.pos * Lex.pos), prefixExp1_SPAN : (Lex.pos * Lex.pos), prefixExp2_SPAN : (Lex.pos * Lex.pos), KW_POW_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts_REFC, tree_REFC, vars_REFC),
                FULL_SPAN, strm')
            end
      fun rootExp_PROD_3 (strm) = let
            val (prefixExp_RES, prefixExp_SPAN, strm') = prefixExp_NT(strm)
            val FULL_SPAN = (#1(prefixExp_SPAN), #2(prefixExp_SPAN))
            in
              (UserCode.rootExp_PROD_3_ACT (prefixExp_RES, prefixExp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts_REFC, tree_REFC, vars_REFC),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.ID(_), _, strm') => rootExp_PROD_3(strm)
          | (Tok.NUM(_), _, strm') => rootExp_PROD_3(strm)
          | (Tok.REAL(_), _, strm') => rootExp_PROD_3(strm)
          | (Tok.MINUS, _, strm') => rootExp_PROD_3(strm)
          | (Tok.LP, _, strm') => rootExp_PROD_3(strm)
          | (Tok.KW_SUM, _, strm') => rootExp_PROD_3(strm)
          | (Tok.KW_PROD, _, strm') => rootExp_PROD_3(strm)
          | (Tok.KW_MEAN, _, strm') => rootExp_PROD_3(strm)
          | (Tok.KW_CORR, _, strm') => rootExp_PROD_3(strm)
          | (Tok.KW_MEDIAN, _, strm') => rootExp_PROD_3(strm)
          | (Tok.KW_STDEV, _, strm') => rootExp_PROD_3(strm)
          | (Tok.KW_VAR, _, strm') => rootExp_PROD_3(strm)
          | (Tok.KW_GETF, _, strm') => rootExp_PROD_3(strm)
          | (Tok.KW_COV, _, strm') => rootExp_PROD_3(strm)
          | (Tok.KW_GETI, _, strm') => rootExp_PROD_3(strm)
          | (Tok.KW_TOFLOAT, _, strm') => rootExp_PROD_3(strm)
          | (Tok.KW_TOINT, _, strm') => rootExp_PROD_3(strm)
          | (Tok.KW_RT, _, strm') => rootExp_PROD_1(strm)
          | (Tok.KW_POW, _, strm') => rootExp_PROD_2(strm)
          | _ => fail()
        (* end case *))
      end
and prefixExp_NT (strm) = let
      fun prefixExp_PROD_1 (strm) = let
            val (atomicExp_RES, atomicExp_SPAN, strm') = atomicExp_NT(strm)
            val FULL_SPAN = (#1(atomicExp_SPAN), #2(atomicExp_SPAN))
            in
              (UserCode.prefixExp_PROD_1_ACT (atomicExp_RES, atomicExp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts_REFC, tree_REFC, vars_REFC),
                FULL_SPAN, strm')
            end
      fun prefixExp_PROD_2 (strm) = let
            val (MINUS_RES, MINUS_SPAN, strm') = matchMINUS(strm)
            val (prefixExp_RES, prefixExp_SPAN, strm') = prefixExp_NT(strm')
            val FULL_SPAN = (#1(MINUS_SPAN), #2(prefixExp_SPAN))
            in
              (UserCode.prefixExp_PROD_2_ACT (MINUS_RES, prefixExp_RES, MINUS_SPAN : (Lex.pos * Lex.pos), prefixExp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts_REFC, tree_REFC, vars_REFC),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.MINUS, _, strm') => prefixExp_PROD_2(strm)
          | (Tok.ID(_), _, strm') => prefixExp_PROD_1(strm)
          | (Tok.NUM(_), _, strm') => prefixExp_PROD_1(strm)
          | (Tok.REAL(_), _, strm') => prefixExp_PROD_1(strm)
          | (Tok.LP, _, strm') => prefixExp_PROD_1(strm)
          | (Tok.KW_SUM, _, strm') => prefixExp_PROD_1(strm)
          | (Tok.KW_PROD, _, strm') => prefixExp_PROD_1(strm)
          | (Tok.KW_MEAN, _, strm') => prefixExp_PROD_1(strm)
          | (Tok.KW_CORR, _, strm') => prefixExp_PROD_1(strm)
          | (Tok.KW_MEDIAN, _, strm') => prefixExp_PROD_1(strm)
          | (Tok.KW_STDEV, _, strm') => prefixExp_PROD_1(strm)
          | (Tok.KW_VAR, _, strm') => prefixExp_PROD_1(strm)
          | (Tok.KW_GETF, _, strm') => prefixExp_PROD_1(strm)
          | (Tok.KW_COV, _, strm') => prefixExp_PROD_1(strm)
          | (Tok.KW_GETI, _, strm') => prefixExp_PROD_1(strm)
          | (Tok.KW_TOFLOAT, _, strm') => prefixExp_PROD_1(strm)
          | (Tok.KW_TOINT, _, strm') => prefixExp_PROD_1(strm)
          | _ => fail()
        (* end case *))
      end
and atomicExp_NT (strm) = let
      fun atomicExp_PROD_1 (strm) = let
            val (ID_RES, ID_SPAN, strm') = matchID(strm)
            val FULL_SPAN = (#1(ID_SPAN), #2(ID_SPAN))
            in
              (UserCode.atomicExp_PROD_1_ACT (ID_RES, ID_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts_REFC, tree_REFC, vars_REFC),
                FULL_SPAN, strm')
            end
      fun atomicExp_PROD_2 (strm) = let
            val (NUM_RES, NUM_SPAN, strm') = matchNUM(strm)
            val FULL_SPAN = (#1(NUM_SPAN), #2(NUM_SPAN))
            in
              (UserCode.atomicExp_PROD_2_ACT (NUM_RES, NUM_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts_REFC, tree_REFC, vars_REFC),
                FULL_SPAN, strm')
            end
      fun atomicExp_PROD_3 (strm) = let
            val (REAL_RES, REAL_SPAN, strm') = matchREAL(strm)
            val FULL_SPAN = (#1(REAL_SPAN), #2(REAL_SPAN))
            in
              (UserCode.atomicExp_PROD_3_ACT (REAL_RES, REAL_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts_REFC, tree_REFC, vars_REFC),
                FULL_SPAN, strm')
            end
      fun atomicExp_PROD_4 (strm) = let
            val (LP_RES, LP_SPAN, strm') = matchLP(strm)
            val (exp_arit_RES, exp_arit_SPAN, strm') = exp_arit_NT(strm')
            val (RP_RES, RP_SPAN, strm') = matchRP(strm')
            val FULL_SPAN = (#1(LP_SPAN), #2(RP_SPAN))
            in
              ((exp_arit_RES), FULL_SPAN, strm')
            end
      fun atomicExp_PROD_5 (strm) = let
            val (funcs_float_RES, funcs_float_SPAN, strm') = funcs_float_NT(strm)
            val FULL_SPAN = (#1(funcs_float_SPAN), #2(funcs_float_SPAN))
            in
              (UserCode.atomicExp_PROD_5_ACT (funcs_float_RES, funcs_float_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts_REFC, tree_REFC, vars_REFC),
                FULL_SPAN, strm')
            end
      fun atomicExp_PROD_6 (strm) = let
            val (funcs_int_RES, funcs_int_SPAN, strm') = funcs_int_NT(strm)
            val FULL_SPAN = (#1(funcs_int_SPAN), #2(funcs_int_SPAN))
            in
              (UserCode.atomicExp_PROD_6_ACT (funcs_int_RES, funcs_int_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts_REFC, tree_REFC, vars_REFC),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.KW_GETI, _, strm') => atomicExp_PROD_6(strm)
          | (Tok.KW_TOINT, _, strm') => atomicExp_PROD_6(strm)
          | (Tok.LP, _, strm') => atomicExp_PROD_4(strm)
          | (Tok.NUM(_), _, strm') => atomicExp_PROD_2(strm)
          | (Tok.ID(_), _, strm') => atomicExp_PROD_1(strm)
          | (Tok.REAL(_), _, strm') => atomicExp_PROD_3(strm)
          | (Tok.KW_SUM, _, strm') => atomicExp_PROD_5(strm)
          | (Tok.KW_PROD, _, strm') => atomicExp_PROD_5(strm)
          | (Tok.KW_MEAN, _, strm') => atomicExp_PROD_5(strm)
          | (Tok.KW_CORR, _, strm') => atomicExp_PROD_5(strm)
          | (Tok.KW_MEDIAN, _, strm') => atomicExp_PROD_5(strm)
          | (Tok.KW_STDEV, _, strm') => atomicExp_PROD_5(strm)
          | (Tok.KW_VAR, _, strm') => atomicExp_PROD_5(strm)
          | (Tok.KW_GETF, _, strm') => atomicExp_PROD_5(strm)
          | (Tok.KW_COV, _, strm') => atomicExp_PROD_5(strm)
          | (Tok.KW_TOFLOAT, _, strm') => atomicExp_PROD_5(strm)
          | _ => fail()
        (* end case *))
      end
and funcs_int_NT (strm) = let
      fun funcs_int_PROD_1 (strm) = let
            val (KW_GETI_RES, KW_GETI_SPAN, strm') = matchKW_GETI(strm)
            val (LP_RES, LP_SPAN, strm') = matchLP(strm')
            val (int_list_RES, int_list_SPAN, strm') = int_list_NT(strm')
            val (COMMA_RES, COMMA_SPAN, strm') = matchCOMMA(strm')
            val (exp_arit_RES, exp_arit_SPAN, strm') = exp_arit_NT(strm')
            val (RP_RES, RP_SPAN, strm') = matchRP(strm')
            val FULL_SPAN = (#1(KW_GETI_SPAN), #2(RP_SPAN))
            in
              (UserCode.funcs_int_PROD_1_ACT (LP_RES, RP_RES, exp_arit_RES, KW_GETI_RES, COMMA_RES, int_list_RES, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), exp_arit_SPAN : (Lex.pos * Lex.pos), KW_GETI_SPAN : (Lex.pos * Lex.pos), COMMA_SPAN : (Lex.pos * Lex.pos), int_list_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts_REFC, tree_REFC, vars_REFC),
                FULL_SPAN, strm')
            end
      fun funcs_int_PROD_2 (strm) = let
            val (KW_TOINT_RES, KW_TOINT_SPAN, strm') = matchKW_TOINT(strm)
            val (LP_RES, LP_SPAN, strm') = matchLP(strm')
            val (expr_RES, expr_SPAN, strm') = expr_NT(strm')
            val (RP_RES, RP_SPAN, strm') = matchRP(strm')
            val FULL_SPAN = (#1(KW_TOINT_SPAN), #2(RP_SPAN))
            in
              (UserCode.funcs_int_PROD_2_ACT (LP_RES, RP_RES, KW_TOINT_RES, expr_RES, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), KW_TOINT_SPAN : (Lex.pos * Lex.pos), expr_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts_REFC, tree_REFC, vars_REFC),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.KW_TOINT, _, strm') => funcs_int_PROD_2(strm)
          | (Tok.KW_GETI, _, strm') => funcs_int_PROD_1(strm)
          | _ => fail()
        (* end case *))
      end
and expr_NT (strm) = let
      fun expr_PROD_1 (strm) = let
            val (exp_string_RES, exp_string_SPAN, strm') = exp_string_NT(strm)
            val FULL_SPAN = (#1(exp_string_SPAN), #2(exp_string_SPAN))
            in
              (UserCode.expr_PROD_1_ACT (exp_string_RES, exp_string_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts_REFC, tree_REFC, vars_REFC),
                FULL_SPAN, strm')
            end
      fun expr_PROD_2 (strm) = let
            val (exp_bool_RES, exp_bool_SPAN, strm') = exp_bool_NT(strm)
            val FULL_SPAN = (#1(exp_bool_SPAN), #2(exp_bool_SPAN))
            in
              (UserCode.expr_PROD_2_ACT (exp_bool_RES, exp_bool_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts_REFC, tree_REFC, vars_REFC),
                FULL_SPAN, strm')
            end
      fun expr_PROD_3 (strm) = let
            val (exp_arit_RES, exp_arit_SPAN, strm') = exp_arit_NT(strm)
            val FULL_SPAN = (#1(exp_arit_SPAN), #2(exp_arit_SPAN))
            in
              (UserCode.expr_PROD_3_ACT (exp_arit_RES, exp_arit_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts_REFC, tree_REFC, vars_REFC),
                FULL_SPAN, strm')
            end
      fun expr_PROD_4 (strm) = let
            val (exp_tupla_RES, exp_tupla_SPAN, strm') = exp_tupla_NT(strm)
            val FULL_SPAN = (#1(exp_tupla_SPAN), #2(exp_tupla_SPAN))
            in
              (UserCode.expr_PROD_4_ACT (exp_tupla_RES, exp_tupla_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts_REFC, tree_REFC, vars_REFC),
                FULL_SPAN, strm')
            end
      fun expr_PROD_5 (strm) = let
            val (funcs_float_RES, funcs_float_SPAN, strm') = funcs_float_NT(strm)
            val FULL_SPAN = (#1(funcs_float_SPAN), #2(funcs_float_SPAN))
            in
              (UserCode.expr_PROD_5_ACT (funcs_float_RES, funcs_float_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts_REFC, tree_REFC, vars_REFC),
                FULL_SPAN, strm')
            end
      fun expr_PROD_6 (strm) = let
            val (funcs_int_RES, funcs_int_SPAN, strm') = funcs_int_NT(strm)
            val FULL_SPAN = (#1(funcs_int_SPAN), #2(funcs_int_SPAN))
            in
              (UserCode.expr_PROD_6_ACT (funcs_int_RES, funcs_int_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts_REFC, tree_REFC, vars_REFC),
                FULL_SPAN, strm')
            end
      fun expr_PROD_7 (strm) = let
            val (funcs_string_RES, funcs_string_SPAN, strm') = funcs_string_NT(strm)
            val FULL_SPAN = (#1(funcs_string_SPAN), #2(funcs_string_SPAN))
            in
              (UserCode.expr_PROD_7_ACT (funcs_string_RES, funcs_string_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts_REFC, tree_REFC, vars_REFC),
                FULL_SPAN, strm')
            end
      fun expr_PROD_8 (strm) = let
            val (val_list_RES, val_list_SPAN, strm') = val_list_NT(strm)
            val FULL_SPAN = (#1(val_list_SPAN), #2(val_list_SPAN))
            in
              (UserCode.expr_PROD_8_ACT (val_list_RES, val_list_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts_REFC, tree_REFC, vars_REFC),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.SSTRING(_), _, strm') => expr_PROD_8(strm)
          | (Tok.SINT(_), _, strm') => expr_PROD_8(strm)
          | (Tok.SFLOAT(_), _, strm') => expr_PROD_8(strm)
          | (Tok.SBOOL(_), _, strm') => expr_PROD_8(strm)
          | (Tok.STUPLE(_), _, strm') => expr_PROD_8(strm)
          | (Tok.TUPLE(_), _, strm') => expr_PROD_4(strm)
          | (Tok.BOOL(_), _, strm') => expr_PROD_2(strm)
          | (Tok.KW_GETI, _, strm') =>
              tryProds(strm, [expr_PROD_2, expr_PROD_3, expr_PROD_6])
          | (Tok.KW_TOINT, _, strm') =>
              tryProds(strm, [expr_PROD_2, expr_PROD_3, expr_PROD_6])
          | (Tok.NUM(_), _, strm') =>
              tryProds(strm, [expr_PROD_2, expr_PROD_3])
          | (Tok.REAL(_), _, strm') =>
              tryProds(strm, [expr_PROD_2, expr_PROD_3])
          | (Tok.MINUS, _, strm') => tryProds(strm, [expr_PROD_2, expr_PROD_3])
          | (Tok.KW_RT, _, strm') => tryProds(strm, [expr_PROD_2, expr_PROD_3])
          | (Tok.KW_POW, _, strm') =>
              tryProds(strm, [expr_PROD_2, expr_PROD_3])
          | (Tok.KW_SUM, _, strm') =>
              tryProds(strm, [expr_PROD_2, expr_PROD_3, expr_PROD_5])
          | (Tok.KW_PROD, _, strm') =>
              tryProds(strm, [expr_PROD_2, expr_PROD_3, expr_PROD_5])
          | (Tok.KW_MEAN, _, strm') =>
              tryProds(strm, [expr_PROD_2, expr_PROD_3, expr_PROD_5])
          | (Tok.KW_CORR, _, strm') =>
              tryProds(strm, [expr_PROD_2, expr_PROD_3, expr_PROD_5])
          | (Tok.KW_MEDIAN, _, strm') =>
              tryProds(strm, [expr_PROD_2, expr_PROD_3, expr_PROD_5])
          | (Tok.KW_STDEV, _, strm') =>
              tryProds(strm, [expr_PROD_2, expr_PROD_3, expr_PROD_5])
          | (Tok.KW_VAR, _, strm') =>
              tryProds(strm, [expr_PROD_2, expr_PROD_3, expr_PROD_5])
          | (Tok.KW_GETF, _, strm') =>
              tryProds(strm, [expr_PROD_2, expr_PROD_3, expr_PROD_5])
          | (Tok.KW_COV, _, strm') =>
              tryProds(strm, [expr_PROD_2, expr_PROD_3, expr_PROD_5])
          | (Tok.KW_TOFLOAT, _, strm') =>
              tryProds(strm, [expr_PROD_2, expr_PROD_3, expr_PROD_5])
          | (Tok.ID(_), _, strm') =>
              tryProds(strm, [expr_PROD_1, expr_PROD_2, expr_PROD_3,
                expr_PROD_4, expr_PROD_8])
          | (Tok.LP, _, strm') =>
              tryProds(strm, [expr_PROD_1, expr_PROD_2, expr_PROD_3])
          | (Tok.STR(_), _, strm') => expr_PROD_1(strm)
          | (Tok.KW_GETS, _, strm') =>
              tryProds(strm, [expr_PROD_1, expr_PROD_7])
          | (Tok.KW_TOSTRING, _, strm') =>
              tryProds(strm, [expr_PROD_1, expr_PROD_7])
          | (Tok.KW_LINREG, _, strm') =>
              tryProds(strm, [expr_PROD_1, expr_PROD_7])
          | (Tok.CONCAT, _, strm') =>
              tryProds(strm, [expr_PROD_1, expr_PROD_7])
          | _ => fail()
        (* end case *))
      end
and funcs_string_NT (strm) = let
      fun funcs_string_PROD_1 (strm) = let
            val (KW_TOSTRING_RES, KW_TOSTRING_SPAN, strm') = matchKW_TOSTRING(strm)
            val (LP_RES, LP_SPAN, strm') = matchLP(strm')
            val (expr_RES, expr_SPAN, strm') = expr_NT(strm')
            val (RP_RES, RP_SPAN, strm') = matchRP(strm')
            val FULL_SPAN = (#1(KW_TOSTRING_SPAN), #2(RP_SPAN))
            in
              (UserCode.funcs_string_PROD_1_ACT (LP_RES, RP_RES, expr_RES, KW_TOSTRING_RES, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), expr_SPAN : (Lex.pos * Lex.pos), KW_TOSTRING_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts_REFC, tree_REFC, vars_REFC),
                FULL_SPAN, strm')
            end
      fun funcs_string_PROD_2 (strm) = let
            val (KW_GETS_RES, KW_GETS_SPAN, strm') = matchKW_GETS(strm)
            val (LP_RES, LP_SPAN, strm') = matchLP(strm')
            val (string_list_RES, string_list_SPAN, strm') = string_list_NT(strm')
            val (COMMA_RES, COMMA_SPAN, strm') = matchCOMMA(strm')
            val (exp_arit_RES, exp_arit_SPAN, strm') = exp_arit_NT(strm')
            val (RP_RES, RP_SPAN, strm') = matchRP(strm')
            val FULL_SPAN = (#1(KW_GETS_SPAN), #2(RP_SPAN))
            in
              (UserCode.funcs_string_PROD_2_ACT (LP_RES, RP_RES, exp_arit_RES, KW_GETS_RES, COMMA_RES, string_list_RES, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), exp_arit_SPAN : (Lex.pos * Lex.pos), KW_GETS_SPAN : (Lex.pos * Lex.pos), COMMA_SPAN : (Lex.pos * Lex.pos), string_list_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts_REFC, tree_REFC, vars_REFC),
                FULL_SPAN, strm')
            end
      fun funcs_string_PROD_3 (strm) = let
            val (CONCAT_RES, CONCAT_SPAN, strm') = matchCONCAT(strm)
            val (LP_RES, LP_SPAN, strm') = matchLP(strm')
            val (string_list1_RES, string_list1_SPAN, strm') = string_list_NT(strm')
            val (COMMA_RES, COMMA_SPAN, strm') = matchCOMMA(strm')
            val (string_list2_RES, string_list2_SPAN, strm') = string_list_NT(strm')
            val (RP_RES, RP_SPAN, strm') = matchRP(strm')
            val FULL_SPAN = (#1(CONCAT_SPAN), #2(RP_SPAN))
            in
              (UserCode.funcs_string_PROD_3_ACT (LP_RES, RP_RES, COMMA_RES, string_list1_RES, string_list2_RES, CONCAT_RES, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), COMMA_SPAN : (Lex.pos * Lex.pos), string_list1_SPAN : (Lex.pos * Lex.pos), string_list2_SPAN : (Lex.pos * Lex.pos), CONCAT_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts_REFC, tree_REFC, vars_REFC),
                FULL_SPAN, strm')
            end
      fun funcs_string_PROD_4 (strm) = let
            val (KW_LINREG_RES, KW_LINREG_SPAN, strm') = matchKW_LINREG(strm)
            val (LP_RES, LP_SPAN, strm') = matchLP(strm')
            val (numbers_list1_RES, numbers_list1_SPAN, strm') = numbers_list_NT(strm')
            val (COMMA_RES, COMMA_SPAN, strm') = matchCOMMA(strm')
            val (numbers_list2_RES, numbers_list2_SPAN, strm') = numbers_list_NT(strm')
            val (RP_RES, RP_SPAN, strm') = matchRP(strm')
            val FULL_SPAN = (#1(KW_LINREG_SPAN), #2(RP_SPAN))
            in
              (UserCode.funcs_string_PROD_4_ACT (LP_RES, RP_RES, KW_LINREG_RES, numbers_list1_RES, numbers_list2_RES, COMMA_RES, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), KW_LINREG_SPAN : (Lex.pos * Lex.pos), numbers_list1_SPAN : (Lex.pos * Lex.pos), numbers_list2_SPAN : (Lex.pos * Lex.pos), COMMA_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts_REFC, tree_REFC, vars_REFC),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.KW_LINREG, _, strm') => funcs_string_PROD_4(strm)
          | (Tok.KW_GETS, _, strm') => funcs_string_PROD_2(strm)
          | (Tok.KW_TOSTRING, _, strm') => funcs_string_PROD_1(strm)
          | (Tok.CONCAT, _, strm') => funcs_string_PROD_3(strm)
          | _ => fail()
        (* end case *))
      end
and exp_arit_NT (strm) = let
      fun exp_arit_PROD_1 (strm) = let
            val (addExp_RES, addExp_SPAN, strm') = addExp_NT(strm)
            val FULL_SPAN = (#1(addExp_SPAN), #2(addExp_SPAN))
            in
              (UserCode.exp_arit_PROD_1_ACT (addExp_RES, addExp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts_REFC, tree_REFC, vars_REFC),
                FULL_SPAN, strm')
            end
      fun exp_arit_PROD_2 (strm) = let
            val (atomicExp_RES, atomicExp_SPAN, strm') = atomicExp_NT(strm)
            val FULL_SPAN = (#1(atomicExp_SPAN), #2(atomicExp_SPAN))
            in
              (UserCode.exp_arit_PROD_2_ACT (atomicExp_RES, atomicExp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts_REFC, tree_REFC, vars_REFC),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.ID(_), _, strm') =>
              tryProds(strm, [exp_arit_PROD_1, exp_arit_PROD_2])
          | (Tok.NUM(_), _, strm') =>
              tryProds(strm, [exp_arit_PROD_1, exp_arit_PROD_2])
          | (Tok.REAL(_), _, strm') =>
              tryProds(strm, [exp_arit_PROD_1, exp_arit_PROD_2])
          | (Tok.LP, _, strm') =>
              tryProds(strm, [exp_arit_PROD_1, exp_arit_PROD_2])
          | (Tok.KW_SUM, _, strm') =>
              tryProds(strm, [exp_arit_PROD_1, exp_arit_PROD_2])
          | (Tok.KW_PROD, _, strm') =>
              tryProds(strm, [exp_arit_PROD_1, exp_arit_PROD_2])
          | (Tok.KW_MEAN, _, strm') =>
              tryProds(strm, [exp_arit_PROD_1, exp_arit_PROD_2])
          | (Tok.KW_CORR, _, strm') =>
              tryProds(strm, [exp_arit_PROD_1, exp_arit_PROD_2])
          | (Tok.KW_MEDIAN, _, strm') =>
              tryProds(strm, [exp_arit_PROD_1, exp_arit_PROD_2])
          | (Tok.KW_STDEV, _, strm') =>
              tryProds(strm, [exp_arit_PROD_1, exp_arit_PROD_2])
          | (Tok.KW_VAR, _, strm') =>
              tryProds(strm, [exp_arit_PROD_1, exp_arit_PROD_2])
          | (Tok.KW_GETF, _, strm') =>
              tryProds(strm, [exp_arit_PROD_1, exp_arit_PROD_2])
          | (Tok.KW_COV, _, strm') =>
              tryProds(strm, [exp_arit_PROD_1, exp_arit_PROD_2])
          | (Tok.KW_GETI, _, strm') =>
              tryProds(strm, [exp_arit_PROD_1, exp_arit_PROD_2])
          | (Tok.KW_TOFLOAT, _, strm') =>
              tryProds(strm, [exp_arit_PROD_1, exp_arit_PROD_2])
          | (Tok.KW_TOINT, _, strm') =>
              tryProds(strm, [exp_arit_PROD_1, exp_arit_PROD_2])
          | (Tok.MINUS, _, strm') => exp_arit_PROD_1(strm)
          | (Tok.KW_RT, _, strm') => exp_arit_PROD_1(strm)
          | (Tok.KW_POW, _, strm') => exp_arit_PROD_1(strm)
          | _ => fail()
        (* end case *))
      end
and funcs_float_NT (strm) = let
      fun funcs_float_PROD_1 (strm) = let
            val (KW_SUM_RES, KW_SUM_SPAN, strm') = matchKW_SUM(strm)
            val (LP_RES, LP_SPAN, strm') = matchLP(strm')
            val (EMPTY_RES, EMPTY_SPAN, strm') = matchEMPTY(strm')
            val (RP_RES, RP_SPAN, strm') = matchRP(strm')
            val FULL_SPAN = (#1(KW_SUM_SPAN), #2(RP_SPAN))
            in
              (UserCode.funcs_float_PROD_1_ACT (LP_RES, RP_RES, EMPTY_RES, KW_SUM_RES, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), EMPTY_SPAN : (Lex.pos * Lex.pos), KW_SUM_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts_REFC, tree_REFC, vars_REFC),
                FULL_SPAN, strm')
            end
      fun funcs_float_PROD_2 (strm) = let
            val (KW_SUM_RES, KW_SUM_SPAN, strm') = matchKW_SUM(strm)
            val (LP_RES, LP_SPAN, strm') = matchLP(strm')
            val (float_list_RES, float_list_SPAN, strm') = float_list_NT(strm')
            val (RP_RES, RP_SPAN, strm') = matchRP(strm')
            val FULL_SPAN = (#1(KW_SUM_SPAN), #2(RP_SPAN))
            in
              (UserCode.funcs_float_PROD_2_ACT (LP_RES, RP_RES, float_list_RES, KW_SUM_RES, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), float_list_SPAN : (Lex.pos * Lex.pos), KW_SUM_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts_REFC, tree_REFC, vars_REFC),
                FULL_SPAN, strm')
            end
      fun funcs_float_PROD_3 (strm) = let
            val (KW_PROD_RES, KW_PROD_SPAN, strm') = matchKW_PROD(strm)
            val (LP_RES, LP_SPAN, strm') = matchLP(strm')
            val (EMPTY_RES, EMPTY_SPAN, strm') = matchEMPTY(strm')
            val (RP_RES, RP_SPAN, strm') = matchRP(strm')
            val FULL_SPAN = (#1(KW_PROD_SPAN), #2(RP_SPAN))
            in
              (UserCode.funcs_float_PROD_3_ACT (LP_RES, RP_RES, KW_PROD_RES, EMPTY_RES, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), KW_PROD_SPAN : (Lex.pos * Lex.pos), EMPTY_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts_REFC, tree_REFC, vars_REFC),
                FULL_SPAN, strm')
            end
      fun funcs_float_PROD_4 (strm) = let
            val (KW_PROD_RES, KW_PROD_SPAN, strm') = matchKW_PROD(strm)
            val (LP_RES, LP_SPAN, strm') = matchLP(strm')
            val (float_list_RES, float_list_SPAN, strm') = float_list_NT(strm')
            val (RP_RES, RP_SPAN, strm') = matchRP(strm')
            val FULL_SPAN = (#1(KW_PROD_SPAN), #2(RP_SPAN))
            in
              (UserCode.funcs_float_PROD_4_ACT (LP_RES, RP_RES, KW_PROD_RES, float_list_RES, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), KW_PROD_SPAN : (Lex.pos * Lex.pos), float_list_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts_REFC, tree_REFC, vars_REFC),
                FULL_SPAN, strm')
            end
      fun funcs_float_PROD_5 (strm) = let
            val (KW_MEAN_RES, KW_MEAN_SPAN, strm') = matchKW_MEAN(strm)
            val (LP_RES, LP_SPAN, strm') = matchLP(strm')
            val (numbers_list_RES, numbers_list_SPAN, strm') = numbers_list_NT(strm')
            val (RP_RES, RP_SPAN, strm') = matchRP(strm')
            val FULL_SPAN = (#1(KW_MEAN_SPAN), #2(RP_SPAN))
            in
              (UserCode.funcs_float_PROD_5_ACT (LP_RES, RP_RES, KW_MEAN_RES, numbers_list_RES, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), KW_MEAN_SPAN : (Lex.pos * Lex.pos), numbers_list_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts_REFC, tree_REFC, vars_REFC),
                FULL_SPAN, strm')
            end
      fun funcs_float_PROD_6 (strm) = let
            val (KW_CORR_RES, KW_CORR_SPAN, strm') = matchKW_CORR(strm)
            val (LP_RES, LP_SPAN, strm') = matchLP(strm')
            val (numbers_list1_RES, numbers_list1_SPAN, strm') = numbers_list_NT(strm')
            val (COMMA_RES, COMMA_SPAN, strm') = matchCOMMA(strm')
            val (numbers_list2_RES, numbers_list2_SPAN, strm') = numbers_list_NT(strm')
            val (RP_RES, RP_SPAN, strm') = matchRP(strm')
            val FULL_SPAN = (#1(KW_CORR_SPAN), #2(RP_SPAN))
            in
              (UserCode.funcs_float_PROD_6_ACT (LP_RES, RP_RES, KW_CORR_RES, numbers_list1_RES, numbers_list2_RES, COMMA_RES, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), KW_CORR_SPAN : (Lex.pos * Lex.pos), numbers_list1_SPAN : (Lex.pos * Lex.pos), numbers_list2_SPAN : (Lex.pos * Lex.pos), COMMA_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts_REFC, tree_REFC, vars_REFC),
                FULL_SPAN, strm')
            end
      fun funcs_float_PROD_7 (strm) = let
            val (KW_STDEV_RES, KW_STDEV_SPAN, strm') = matchKW_STDEV(strm)
            val (LP_RES, LP_SPAN, strm') = matchLP(strm')
            val (numbers_list_RES, numbers_list_SPAN, strm') = numbers_list_NT(strm')
            val (RP_RES, RP_SPAN, strm') = matchRP(strm')
            val FULL_SPAN = (#1(KW_STDEV_SPAN), #2(RP_SPAN))
            in
              (UserCode.funcs_float_PROD_7_ACT (LP_RES, RP_RES, KW_STDEV_RES, numbers_list_RES, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), KW_STDEV_SPAN : (Lex.pos * Lex.pos), numbers_list_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts_REFC, tree_REFC, vars_REFC),
                FULL_SPAN, strm')
            end
      fun funcs_float_PROD_8 (strm) = let
            val (KW_MEDIAN_RES, KW_MEDIAN_SPAN, strm') = matchKW_MEDIAN(strm)
            val (LP_RES, LP_SPAN, strm') = matchLP(strm')
            val (numbers_list_RES, numbers_list_SPAN, strm') = numbers_list_NT(strm')
            val (RP_RES, RP_SPAN, strm') = matchRP(strm')
            val FULL_SPAN = (#1(KW_MEDIAN_SPAN), #2(RP_SPAN))
            in
              (UserCode.funcs_float_PROD_8_ACT (LP_RES, RP_RES, KW_MEDIAN_RES, numbers_list_RES, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), KW_MEDIAN_SPAN : (Lex.pos * Lex.pos), numbers_list_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts_REFC, tree_REFC, vars_REFC),
                FULL_SPAN, strm')
            end
      fun funcs_float_PROD_9 (strm) = let
            val (KW_VAR_RES, KW_VAR_SPAN, strm') = matchKW_VAR(strm)
            val (LP_RES, LP_SPAN, strm') = matchLP(strm')
            val (numbers_list_RES, numbers_list_SPAN, strm') = numbers_list_NT(strm')
            val (RP_RES, RP_SPAN, strm') = matchRP(strm')
            val FULL_SPAN = (#1(KW_VAR_SPAN), #2(RP_SPAN))
            in
              (UserCode.funcs_float_PROD_9_ACT (LP_RES, RP_RES, numbers_list_RES, KW_VAR_RES, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), numbers_list_SPAN : (Lex.pos * Lex.pos), KW_VAR_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts_REFC, tree_REFC, vars_REFC),
                FULL_SPAN, strm')
            end
      fun funcs_float_PROD_10 (strm) = let
            val (KW_COV_RES, KW_COV_SPAN, strm') = matchKW_COV(strm)
            val (LP_RES, LP_SPAN, strm') = matchLP(strm')
            val (numbers_list1_RES, numbers_list1_SPAN, strm') = numbers_list_NT(strm')
            val (COMMA_RES, COMMA_SPAN, strm') = matchCOMMA(strm')
            val (numbers_list2_RES, numbers_list2_SPAN, strm') = numbers_list_NT(strm')
            val (RP_RES, RP_SPAN, strm') = matchRP(strm')
            val FULL_SPAN = (#1(KW_COV_SPAN), #2(RP_SPAN))
            in
              (UserCode.funcs_float_PROD_10_ACT (LP_RES, RP_RES, numbers_list1_RES, numbers_list2_RES, COMMA_RES, KW_COV_RES, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), numbers_list1_SPAN : (Lex.pos * Lex.pos), numbers_list2_SPAN : (Lex.pos * Lex.pos), COMMA_SPAN : (Lex.pos * Lex.pos), KW_COV_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts_REFC, tree_REFC, vars_REFC),
                FULL_SPAN, strm')
            end
      fun funcs_float_PROD_11 (strm) = let
            val (KW_GETF_RES, KW_GETF_SPAN, strm') = matchKW_GETF(strm)
            val (LP_RES, LP_SPAN, strm') = matchLP(strm')
            val (float_list_RES, float_list_SPAN, strm') = float_list_NT(strm')
            val (COMMA_RES, COMMA_SPAN, strm') = matchCOMMA(strm')
            val (exp_arit_RES, exp_arit_SPAN, strm') = exp_arit_NT(strm')
            val (RP_RES, RP_SPAN, strm') = matchRP(strm')
            val FULL_SPAN = (#1(KW_GETF_SPAN), #2(RP_SPAN))
            in
              (UserCode.funcs_float_PROD_11_ACT (LP_RES, RP_RES, exp_arit_RES, KW_GETF_RES, float_list_RES, COMMA_RES, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), exp_arit_SPAN : (Lex.pos * Lex.pos), KW_GETF_SPAN : (Lex.pos * Lex.pos), float_list_SPAN : (Lex.pos * Lex.pos), COMMA_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts_REFC, tree_REFC, vars_REFC),
                FULL_SPAN, strm')
            end
      fun funcs_float_PROD_12 (strm) = let
            val (KW_TOFLOAT_RES, KW_TOFLOAT_SPAN, strm') = matchKW_TOFLOAT(strm)
            val (LP_RES, LP_SPAN, strm') = matchLP(strm')
            val (expr_RES, expr_SPAN, strm') = expr_NT(strm')
            val (RP_RES, RP_SPAN, strm') = matchRP(strm')
            val FULL_SPAN = (#1(KW_TOFLOAT_SPAN), #2(RP_SPAN))
            in
              (UserCode.funcs_float_PROD_12_ACT (LP_RES, RP_RES, expr_RES, KW_TOFLOAT_RES, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), expr_SPAN : (Lex.pos * Lex.pos), KW_TOFLOAT_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts_REFC, tree_REFC, vars_REFC),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.KW_TOFLOAT, _, strm') => funcs_float_PROD_12(strm)
          | (Tok.KW_COV, _, strm') => funcs_float_PROD_10(strm)
          | (Tok.KW_MEDIAN, _, strm') => funcs_float_PROD_8(strm)
          | (Tok.KW_CORR, _, strm') => funcs_float_PROD_6(strm)
          | (Tok.KW_SUM, _, strm') =>
              (case (lex(strm'))
               of (Tok.LP, _, strm') =>
                    (case (lex(strm'))
                     of (Tok.ID(_), _, strm') => funcs_float_PROD_2(strm)
                      | (Tok.SFLOAT(_), _, strm') => funcs_float_PROD_2(strm)
                      | (Tok.EMPTY, _, strm') => funcs_float_PROD_1(strm)
                      | _ => fail()
                    (* end case *))
                | _ => fail()
              (* end case *))
          | (Tok.KW_PROD, _, strm') =>
              (case (lex(strm'))
               of (Tok.LP, _, strm') =>
                    (case (lex(strm'))
                     of (Tok.ID(_), _, strm') => funcs_float_PROD_4(strm)
                      | (Tok.SFLOAT(_), _, strm') => funcs_float_PROD_4(strm)
                      | (Tok.EMPTY, _, strm') => funcs_float_PROD_3(strm)
                      | _ => fail()
                    (* end case *))
                | _ => fail()
              (* end case *))
          | (Tok.KW_MEAN, _, strm') => funcs_float_PROD_5(strm)
          | (Tok.KW_STDEV, _, strm') => funcs_float_PROD_7(strm)
          | (Tok.KW_VAR, _, strm') => funcs_float_PROD_9(strm)
          | (Tok.KW_GETF, _, strm') => funcs_float_PROD_11(strm)
          | _ => fail()
        (* end case *))
      end
and exp_string_NT (strm) = let
      fun exp_string_PROD_1 (strm) = let
            val (op_str_RES, op_str_SPAN, strm') = op_str_NT(strm)
            val FULL_SPAN = (#1(op_str_SPAN), #2(op_str_SPAN))
            in
              (UserCode.exp_string_PROD_1_ACT (op_str_RES, op_str_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts_REFC, tree_REFC, vars_REFC),
                FULL_SPAN, strm')
            end
      fun exp_string_PROD_2 (strm) = let
            val (atom_string_RES, atom_string_SPAN, strm') = atom_string_NT(strm)
            val FULL_SPAN = (#1(atom_string_SPAN), #2(atom_string_SPAN))
            in
              (UserCode.exp_string_PROD_2_ACT (atom_string_RES, atom_string_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts_REFC, tree_REFC, vars_REFC),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.ID(_), _, strm') =>
              tryProds(strm, [exp_string_PROD_1, exp_string_PROD_2])
          | (Tok.LP, _, strm') =>
              tryProds(strm, [exp_string_PROD_1, exp_string_PROD_2])
          | (Tok.STR(_), _, strm') =>
              tryProds(strm, [exp_string_PROD_1, exp_string_PROD_2])
          | (Tok.KW_GETS, _, strm') =>
              tryProds(strm, [exp_string_PROD_1, exp_string_PROD_2])
          | (Tok.KW_TOSTRING, _, strm') =>
              tryProds(strm, [exp_string_PROD_1, exp_string_PROD_2])
          | (Tok.KW_LINREG, _, strm') =>
              tryProds(strm, [exp_string_PROD_1, exp_string_PROD_2])
          | (Tok.CONCAT, _, strm') =>
              tryProds(strm, [exp_string_PROD_1, exp_string_PROD_2])
          | _ => fail()
        (* end case *))
      end
and atom_string_NT (strm) = let
      fun atom_string_PROD_1 (strm) = let
            val (ID_RES, ID_SPAN, strm') = matchID(strm)
            val FULL_SPAN = (#1(ID_SPAN), #2(ID_SPAN))
            in
              (UserCode.atom_string_PROD_1_ACT (ID_RES, ID_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts_REFC, tree_REFC, vars_REFC),
                FULL_SPAN, strm')
            end
      fun atom_string_PROD_2 (strm) = let
            val (STR_RES, STR_SPAN, strm') = matchSTR(strm)
            val FULL_SPAN = (#1(STR_SPAN), #2(STR_SPAN))
            in
              (UserCode.atom_string_PROD_2_ACT (STR_RES, STR_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts_REFC, tree_REFC, vars_REFC),
                FULL_SPAN, strm')
            end
      fun atom_string_PROD_3 (strm) = let
            val (funcs_string_RES, funcs_string_SPAN, strm') = funcs_string_NT(strm)
            val FULL_SPAN = (#1(funcs_string_SPAN), #2(funcs_string_SPAN))
            in
              ((funcs_string_RES), FULL_SPAN, strm')
            end
      fun atom_string_PROD_4 (strm) = let
            val (LP_RES, LP_SPAN, strm') = matchLP(strm)
            val (exp_string_RES, exp_string_SPAN, strm') = exp_string_NT(strm')
            val (RP_RES, RP_SPAN, strm') = matchRP(strm')
            val FULL_SPAN = (#1(LP_SPAN), #2(RP_SPAN))
            in
              ((exp_string_RES), FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.LP, _, strm') => atom_string_PROD_4(strm)
          | (Tok.STR(_), _, strm') => atom_string_PROD_2(strm)
          | (Tok.ID(_), _, strm') => atom_string_PROD_1(strm)
          | (Tok.KW_GETS, _, strm') => atom_string_PROD_3(strm)
          | (Tok.KW_TOSTRING, _, strm') => atom_string_PROD_3(strm)
          | (Tok.KW_LINREG, _, strm') => atom_string_PROD_3(strm)
          | (Tok.CONCAT, _, strm') => atom_string_PROD_3(strm)
          | _ => fail()
        (* end case *))
      end
and op_str_NT (strm) = let
      val (atom_string1_RES, atom_string1_SPAN, strm') = atom_string_NT(strm)
      val (CONCAT_RES, CONCAT_SPAN, strm') = matchCONCAT(strm')
      val (atom_string2_RES, atom_string2_SPAN, strm') = atom_string_NT(strm')
      val FULL_SPAN = (#1(atom_string1_SPAN), #2(atom_string2_SPAN))
      in
        (UserCode.op_str_PROD_1_ACT (atom_string1_RES, atom_string2_RES, CONCAT_RES, atom_string1_SPAN : (Lex.pos * Lex.pos), atom_string2_SPAN : (Lex.pos * Lex.pos), CONCAT_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts_REFC, tree_REFC, vars_REFC),
          FULL_SPAN, strm')
      end
fun case_label_NT (strm) = let
      fun case_label_PROD_1 (strm) = let
            val (exp_arit_RES, exp_arit_SPAN, strm') = exp_arit_NT(strm)
            val FULL_SPAN = (#1(exp_arit_SPAN), #2(exp_arit_SPAN))
            in
              (UserCode.case_label_PROD_1_ACT (exp_arit_RES, exp_arit_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts_REFC, tree_REFC, vars_REFC),
                FULL_SPAN, strm')
            end
      fun case_label_PROD_2 (strm) = let
            val (exp_string_RES, exp_string_SPAN, strm') = exp_string_NT(strm)
            val FULL_SPAN = (#1(exp_string_SPAN), #2(exp_string_SPAN))
            in
              (UserCode.case_label_PROD_2_ACT (exp_string_RES, exp_string_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts_REFC, tree_REFC, vars_REFC),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.STR(_), _, strm') => case_label_PROD_2(strm)
          | (Tok.KW_GETS, _, strm') => case_label_PROD_2(strm)
          | (Tok.KW_TOSTRING, _, strm') => case_label_PROD_2(strm)
          | (Tok.KW_LINREG, _, strm') => case_label_PROD_2(strm)
          | (Tok.CONCAT, _, strm') => case_label_PROD_2(strm)
          | (Tok.ID(_), _, strm') =>
              tryProds(strm, [case_label_PROD_1, case_label_PROD_2])
          | (Tok.LP, _, strm') =>
              tryProds(strm, [case_label_PROD_1, case_label_PROD_2])
          | (Tok.NUM(_), _, strm') => case_label_PROD_1(strm)
          | (Tok.REAL(_), _, strm') => case_label_PROD_1(strm)
          | (Tok.MINUS, _, strm') => case_label_PROD_1(strm)
          | (Tok.KW_SUM, _, strm') => case_label_PROD_1(strm)
          | (Tok.KW_PROD, _, strm') => case_label_PROD_1(strm)
          | (Tok.KW_MEAN, _, strm') => case_label_PROD_1(strm)
          | (Tok.KW_CORR, _, strm') => case_label_PROD_1(strm)
          | (Tok.KW_MEDIAN, _, strm') => case_label_PROD_1(strm)
          | (Tok.KW_STDEV, _, strm') => case_label_PROD_1(strm)
          | (Tok.KW_VAR, _, strm') => case_label_PROD_1(strm)
          | (Tok.KW_RT, _, strm') => case_label_PROD_1(strm)
          | (Tok.KW_POW, _, strm') => case_label_PROD_1(strm)
          | (Tok.KW_GETF, _, strm') => case_label_PROD_1(strm)
          | (Tok.KW_COV, _, strm') => case_label_PROD_1(strm)
          | (Tok.KW_GETI, _, strm') => case_label_PROD_1(strm)
          | (Tok.KW_TOFLOAT, _, strm') => case_label_PROD_1(strm)
          | (Tok.KW_TOINT, _, strm') => case_label_PROD_1(strm)
          | _ => fail()
        (* end case *))
      end
fun case_expr_NT (strm) = let
      fun case_expr_PROD_1 (strm) = let
            val (exp_string_RES, exp_string_SPAN, strm') = exp_string_NT(strm)
            val FULL_SPAN = (#1(exp_string_SPAN), #2(exp_string_SPAN))
            in
              (UserCode.case_expr_PROD_1_ACT (exp_string_RES, exp_string_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts_REFC, tree_REFC, vars_REFC),
                FULL_SPAN, strm')
            end
      fun case_expr_PROD_2 (strm) = let
            val (exp_arit_RES, exp_arit_SPAN, strm') = exp_arit_NT(strm)
            val FULL_SPAN = (#1(exp_arit_SPAN), #2(exp_arit_SPAN))
            in
              (UserCode.case_expr_PROD_2_ACT (exp_arit_RES, exp_arit_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts_REFC, tree_REFC, vars_REFC),
                FULL_SPAN, strm')
            end
      fun case_expr_PROD_3 (strm) = let
            val (funcs_float_RES, funcs_float_SPAN, strm') = funcs_float_NT(strm)
            val FULL_SPAN = (#1(funcs_float_SPAN), #2(funcs_float_SPAN))
            in
              (UserCode.case_expr_PROD_3_ACT (funcs_float_RES, funcs_float_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts_REFC, tree_REFC, vars_REFC),
                FULL_SPAN, strm')
            end
      fun case_expr_PROD_4 (strm) = let
            val (funcs_int_RES, funcs_int_SPAN, strm') = funcs_int_NT(strm)
            val FULL_SPAN = (#1(funcs_int_SPAN), #2(funcs_int_SPAN))
            in
              (UserCode.case_expr_PROD_4_ACT (funcs_int_RES, funcs_int_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts_REFC, tree_REFC, vars_REFC),
                FULL_SPAN, strm')
            end
      fun case_expr_PROD_5 (strm) = let
            val (funcs_string_RES, funcs_string_SPAN, strm') = funcs_string_NT(strm)
            val FULL_SPAN = (#1(funcs_string_SPAN), #2(funcs_string_SPAN))
            in
              (UserCode.case_expr_PROD_5_ACT (funcs_string_RES, funcs_string_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts_REFC, tree_REFC, vars_REFC),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.KW_GETS, _, strm') =>
              tryProds(strm, [case_expr_PROD_1, case_expr_PROD_5])
          | (Tok.KW_TOSTRING, _, strm') =>
              tryProds(strm, [case_expr_PROD_1, case_expr_PROD_5])
          | (Tok.KW_LINREG, _, strm') =>
              tryProds(strm, [case_expr_PROD_1, case_expr_PROD_5])
          | (Tok.CONCAT, _, strm') =>
              tryProds(strm, [case_expr_PROD_1, case_expr_PROD_5])
          | (Tok.STR(_), _, strm') => case_expr_PROD_1(strm)
          | (Tok.ID(_), _, strm') =>
              tryProds(strm, [case_expr_PROD_1, case_expr_PROD_2])
          | (Tok.LP, _, strm') =>
              tryProds(strm, [case_expr_PROD_1, case_expr_PROD_2])
          | (Tok.KW_SUM, _, strm') =>
              tryProds(strm, [case_expr_PROD_2, case_expr_PROD_3])
          | (Tok.KW_PROD, _, strm') =>
              tryProds(strm, [case_expr_PROD_2, case_expr_PROD_3])
          | (Tok.KW_MEAN, _, strm') =>
              tryProds(strm, [case_expr_PROD_2, case_expr_PROD_3])
          | (Tok.KW_CORR, _, strm') =>
              tryProds(strm, [case_expr_PROD_2, case_expr_PROD_3])
          | (Tok.KW_MEDIAN, _, strm') =>
              tryProds(strm, [case_expr_PROD_2, case_expr_PROD_3])
          | (Tok.KW_STDEV, _, strm') =>
              tryProds(strm, [case_expr_PROD_2, case_expr_PROD_3])
          | (Tok.KW_VAR, _, strm') =>
              tryProds(strm, [case_expr_PROD_2, case_expr_PROD_3])
          | (Tok.KW_GETF, _, strm') =>
              tryProds(strm, [case_expr_PROD_2, case_expr_PROD_3])
          | (Tok.KW_COV, _, strm') =>
              tryProds(strm, [case_expr_PROD_2, case_expr_PROD_3])
          | (Tok.KW_TOFLOAT, _, strm') =>
              tryProds(strm, [case_expr_PROD_2, case_expr_PROD_3])
          | (Tok.NUM(_), _, strm') => case_expr_PROD_2(strm)
          | (Tok.REAL(_), _, strm') => case_expr_PROD_2(strm)
          | (Tok.MINUS, _, strm') => case_expr_PROD_2(strm)
          | (Tok.KW_RT, _, strm') => case_expr_PROD_2(strm)
          | (Tok.KW_POW, _, strm') => case_expr_PROD_2(strm)
          | (Tok.KW_GETI, _, strm') =>
              tryProds(strm, [case_expr_PROD_2, case_expr_PROD_4])
          | (Tok.KW_TOINT, _, strm') =>
              tryProds(strm, [case_expr_PROD_2, case_expr_PROD_4])
          | _ => fail()
        (* end case *))
      end
fun assign_NT (strm) = let
      val (ID_RES, ID_SPAN, strm') = matchID(strm)
      val (DOTDOT_RES, DOTDOT_SPAN, strm') = matchDOTDOT(strm')
      val (expr_RES, expr_SPAN, strm') = expr_NT(strm')
      val FULL_SPAN = (#1(ID_SPAN), #2(expr_SPAN))
      in
        (UserCode.assign_PROD_1_ACT (ID_RES, expr_RES, DOTDOT_RES, ID_SPAN : (Lex.pos * Lex.pos), expr_SPAN : (Lex.pos * Lex.pos), DOTDOT_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts_REFC, tree_REFC, vars_REFC),
          FULL_SPAN, strm')
      end
fun prints_NT (strm) = let
      val (KW_Print_RES, KW_Print_SPAN, strm') = matchKW_Print(strm)
      val (LP_RES, LP_SPAN, strm') = matchLP(strm')
      val (exp_string_RES, exp_string_SPAN, strm') = exp_string_NT(strm')
      val (RP_RES, RP_SPAN, strm') = matchRP(strm')
      val FULL_SPAN = (#1(KW_Print_SPAN), #2(RP_SPAN))
      in
        (UserCode.prints_PROD_1_ACT (LP_RES, RP_RES, KW_Print_RES, exp_string_RES, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), KW_Print_SPAN : (Lex.pos * Lex.pos), exp_string_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts_REFC, tree_REFC, vars_REFC),
          FULL_SPAN, strm')
      end
fun commands_NT (strm) = let
      fun commands_PROD_1 (strm) = let
            val (prints_RES, prints_SPAN, strm') = prints_NT(strm)
            val (SEMI_RES, SEMI_SPAN, strm') = matchSEMI(strm')
            val FULL_SPAN = (#1(prints_SPAN), #2(SEMI_SPAN))
            in
              (UserCode.commands_PROD_1_ACT (SEMI_RES, prints_RES, SEMI_SPAN : (Lex.pos * Lex.pos), prints_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts_REFC, tree_REFC, vars_REFC),
                FULL_SPAN, strm')
            end
      fun commands_PROD_2 (strm) = let
            val (assign_RES, assign_SPAN, strm') = assign_NT(strm)
            val (SEMI_RES, SEMI_SPAN, strm') = matchSEMI(strm')
            val FULL_SPAN = (#1(assign_SPAN), #2(SEMI_SPAN))
            in
              (UserCode.commands_PROD_2_ACT (SEMI_RES, assign_RES, SEMI_SPAN : (Lex.pos * Lex.pos), assign_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts_REFC, tree_REFC, vars_REFC),
                FULL_SPAN, strm')
            end
      fun commands_PROD_3 (strm) = let
            val (conditional_RES, conditional_SPAN, strm') = conditional_NT(strm)
            val FULL_SPAN = (#1(conditional_SPAN), #2(conditional_SPAN))
            in
              (UserCode.commands_PROD_3_ACT (conditional_RES, conditional_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts_REFC, tree_REFC, vars_REFC),
                FULL_SPAN, strm')
            end
      fun commands_PROD_4 (strm) = let
            val (case_st_RES, case_st_SPAN, strm') = case_st_NT(strm)
            val FULL_SPAN = (#1(case_st_SPAN), #2(case_st_SPAN))
            in
              (UserCode.commands_PROD_4_ACT (case_st_RES, case_st_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts_REFC, tree_REFC, vars_REFC),
                FULL_SPAN, strm')
            end
      fun commands_PROD_5 (strm) = let
            val (loop_RES, loop_SPAN, strm') = loop_NT(strm)
            val FULL_SPAN = (#1(loop_SPAN), #2(loop_SPAN))
            in
              (UserCode.commands_PROD_5_ACT (loop_RES, loop_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts_REFC, tree_REFC, vars_REFC),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.KW_WHILE, _, strm') => commands_PROD_5(strm)
          | (Tok.KW_IF, _, strm') => commands_PROD_3(strm)
          | (Tok.KW_Print, _, strm') => commands_PROD_1(strm)
          | (Tok.ID(_), _, strm') => commands_PROD_2(strm)
          | (Tok.KW_CASE, _, strm') => commands_PROD_4(strm)
          | _ => fail()
        (* end case *))
      end
and loop_NT (strm) = let
      val (KW_WHILE_RES, KW_WHILE_SPAN, strm') = matchKW_WHILE(strm)
      val (exp_bool_RES, exp_bool_SPAN, strm') = exp_bool_NT(strm')
      val (KW_DO_RES, KW_DO_SPAN, strm') = matchKW_DO(strm')
      fun loop_PROD_1_SUBRULE_1_NT (strm) = let
            val (commands_RES, commands_SPAN, strm') = commands_NT(strm)
            val FULL_SPAN = (#1(commands_SPAN), #2(commands_SPAN))
            in
              ((commands_RES), FULL_SPAN, strm')
            end
      fun loop_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
             of (Tok.ID(_), _, strm') => true
              | (Tok.KW_Print, _, strm') => true
              | (Tok.KW_IF, _, strm') => true
              | (Tok.KW_WHILE, _, strm') => true
              | (Tok.KW_CASE, _, strm') => true
              | _ => false
            (* end case *))
      val (SR_RES, SR_SPAN, strm') = EBNF.closure(loop_PROD_1_SUBRULE_1_PRED, loop_PROD_1_SUBRULE_1_NT, strm')
      val (KW_END_RES, KW_END_SPAN, strm') = matchKW_END(strm')
      val FULL_SPAN = (#1(KW_WHILE_SPAN), #2(KW_END_SPAN))
      in
        (UserCode.loop_PROD_1_ACT (SR_RES, KW_WHILE_RES, exp_bool_RES, KW_DO_RES, KW_END_RES, SR_SPAN : (Lex.pos * Lex.pos), KW_WHILE_SPAN : (Lex.pos * Lex.pos), exp_bool_SPAN : (Lex.pos * Lex.pos), KW_DO_SPAN : (Lex.pos * Lex.pos), KW_END_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts_REFC, tree_REFC, vars_REFC),
          FULL_SPAN, strm')
      end
and case_st_NT (strm) = let
      val (KW_CASE_RES, KW_CASE_SPAN, strm') = matchKW_CASE(strm)
      val (case_expr_RES, case_expr_SPAN, strm') = case_expr_NT(strm')
      val (OF_RES, OF_SPAN, strm') = matchOF(strm')
      fun case_st_PROD_1_SUBRULE_1_NT (strm) = let
            val (case_element_RES, case_element_SPAN, strm') = case_element_NT(strm)
            val FULL_SPAN = (#1(case_element_SPAN), #2(case_element_SPAN))
            in
              ((case_element_RES), FULL_SPAN, strm')
            end
      fun case_st_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
             of (Tok.ID(_), _, strm') => true
              | (Tok.NUM(_), _, strm') => true
              | (Tok.REAL(_), _, strm') => true
              | (Tok.MINUS, _, strm') => true
              | (Tok.LP, _, strm') => true
              | (Tok.STR(_), _, strm') => true
              | (Tok.KW_SUM, _, strm') => true
              | (Tok.KW_PROD, _, strm') => true
              | (Tok.KW_GETS, _, strm') => true
              | (Tok.KW_TOSTRING, _, strm') => true
              | (Tok.KW_MEAN, _, strm') => true
              | (Tok.KW_CORR, _, strm') => true
              | (Tok.KW_MEDIAN, _, strm') => true
              | (Tok.KW_STDEV, _, strm') => true
              | (Tok.KW_VAR, _, strm') => true
              | (Tok.KW_RT, _, strm') => true
              | (Tok.KW_POW, _, strm') => true
              | (Tok.KW_GETF, _, strm') => true
              | (Tok.KW_COV, _, strm') => true
              | (Tok.KW_LINREG, _, strm') => true
              | (Tok.KW_GETI, _, strm') => true
              | (Tok.KW_TOFLOAT, _, strm') => true
              | (Tok.KW_TOINT, _, strm') => true
              | (Tok.CONCAT, _, strm') => true
              | _ => false
            (* end case *))
      val (SR_RES, SR_SPAN, strm') = EBNF.closure(case_st_PROD_1_SUBRULE_1_PRED, case_st_PROD_1_SUBRULE_1_NT, strm')
      val (KW_END_RES, KW_END_SPAN, strm') = matchKW_END(strm')
      val FULL_SPAN = (#1(KW_CASE_SPAN), #2(KW_END_SPAN))
      in
        (UserCode.case_st_PROD_1_ACT (OF_RES, SR_RES, KW_CASE_RES, case_expr_RES, KW_END_RES, OF_SPAN : (Lex.pos * Lex.pos), SR_SPAN : (Lex.pos * Lex.pos), KW_CASE_SPAN : (Lex.pos * Lex.pos), case_expr_SPAN : (Lex.pos * Lex.pos), KW_END_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts_REFC, tree_REFC, vars_REFC),
          FULL_SPAN, strm')
      end
and case_element_NT (strm) = let
      val (case_label_RES, case_label_SPAN, strm') = case_label_NT(strm)
      val (COLON_RES, COLON_SPAN, strm') = matchCOLON(strm')
      fun case_element_PROD_1_SUBRULE_1_NT (strm) = let
            val (commands_RES, commands_SPAN, strm') = commands_NT(strm)
            val FULL_SPAN = (#1(commands_SPAN), #2(commands_SPAN))
            in
              ((commands_RES), FULL_SPAN, strm')
            end
      fun case_element_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
             of (Tok.KW_Print, _, strm') => true
              | (Tok.KW_IF, _, strm') => true
              | (Tok.KW_WHILE, _, strm') => true
              | (Tok.KW_CASE, _, strm') => true
              | (Tok.ID(_), _, strm') =>
                  (case (lex(strm'))
                   of (Tok.DOTDOT, _, strm') => true
                    | _ => false
                  (* end case *))
              | _ => false
            (* end case *))
      val (SR_RES, SR_SPAN, strm') = EBNF.closure(case_element_PROD_1_SUBRULE_1_PRED, case_element_PROD_1_SUBRULE_1_NT, strm')
      val FULL_SPAN = (#1(case_label_SPAN), #2(SR_SPAN))
      in
        (UserCode.case_element_PROD_1_ACT (SR_RES, COLON_RES, case_label_RES, SR_SPAN : (Lex.pos * Lex.pos), COLON_SPAN : (Lex.pos * Lex.pos), case_label_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts_REFC, tree_REFC, vars_REFC),
          FULL_SPAN, strm')
      end
and conditional_NT (strm) = let
      val (KW_IF_RES, KW_IF_SPAN, strm') = matchKW_IF(strm)
      val (exp_bool_RES, exp_bool_SPAN, strm') = exp_bool_NT(strm')
      val (KW_THEN_RES, KW_THEN_SPAN, strm') = matchKW_THEN(strm')
      fun conditional_PROD_1_SUBRULE_1_NT (strm) = let
            val (commands_RES, commands_SPAN, strm') = commands_NT(strm)
            val FULL_SPAN = (#1(commands_SPAN), #2(commands_SPAN))
            in
              ((commands_RES), FULL_SPAN, strm')
            end
      fun conditional_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
             of (Tok.ID(_), _, strm') => true
              | (Tok.KW_Print, _, strm') => true
              | (Tok.KW_IF, _, strm') => true
              | (Tok.KW_WHILE, _, strm') => true
              | (Tok.KW_CASE, _, strm') => true
              | _ => false
            (* end case *))
      val (SR1_RES, SR1_SPAN, strm') = EBNF.closure(conditional_PROD_1_SUBRULE_1_PRED, conditional_PROD_1_SUBRULE_1_NT, strm')
      val (KW_ELSE_RES, KW_ELSE_SPAN, strm') = matchKW_ELSE(strm')
      fun conditional_PROD_1_SUBRULE_2_NT (strm) = let
            val (commands_RES, commands_SPAN, strm') = commands_NT(strm)
            val FULL_SPAN = (#1(commands_SPAN), #2(commands_SPAN))
            in
              ((commands_RES), FULL_SPAN, strm')
            end
      fun conditional_PROD_1_SUBRULE_2_PRED (strm) = (case (lex(strm))
             of (Tok.ID(_), _, strm') => true
              | (Tok.KW_Print, _, strm') => true
              | (Tok.KW_IF, _, strm') => true
              | (Tok.KW_WHILE, _, strm') => true
              | (Tok.KW_CASE, _, strm') => true
              | _ => false
            (* end case *))
      val (SR2_RES, SR2_SPAN, strm') = EBNF.closure(conditional_PROD_1_SUBRULE_2_PRED, conditional_PROD_1_SUBRULE_2_NT, strm')
      val (KW_END_RES, KW_END_SPAN, strm') = matchKW_END(strm')
      val FULL_SPAN = (#1(KW_IF_SPAN), #2(KW_END_SPAN))
      in
        (UserCode.conditional_PROD_1_ACT (SR1_RES, SR2_RES, exp_bool_RES, KW_ELSE_RES, KW_THEN_RES, KW_IF_RES, KW_END_RES, SR1_SPAN : (Lex.pos * Lex.pos), SR2_SPAN : (Lex.pos * Lex.pos), exp_bool_SPAN : (Lex.pos * Lex.pos), KW_ELSE_SPAN : (Lex.pos * Lex.pos), KW_THEN_SPAN : (Lex.pos * Lex.pos), KW_IF_SPAN : (Lex.pos * Lex.pos), KW_END_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts_REFC, tree_REFC, vars_REFC),
          FULL_SPAN, strm')
      end
fun declaration_NT (strm) = let
      val (TIPO_RES, TIPO_SPAN, strm') = matchTIPO(strm)
      val (ID_RES, ID_SPAN, strm') = matchID(strm')
      val (SEMI_RES, SEMI_SPAN, strm') = matchSEMI(strm')
      val FULL_SPAN = (#1(TIPO_SPAN), #2(SEMI_SPAN))
      in
        (UserCode.declaration_PROD_1_ACT (ID_RES, SEMI_RES, TIPO_RES, ID_SPAN : (Lex.pos * Lex.pos), SEMI_SPAN : (Lex.pos * Lex.pos), TIPO_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts_REFC, tree_REFC, vars_REFC),
          FULL_SPAN, strm')
      end
fun variables_NT (strm) = let
      fun variables_PROD_1_SUBRULE_1_NT (strm) = let
            val (declaration_RES, declaration_SPAN, strm') = declaration_NT(strm)
            val FULL_SPAN = (#1(declaration_SPAN), #2(declaration_SPAN))
            in
              ((declaration_RES), FULL_SPAN, strm')
            end
      fun variables_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
             of (Tok.TIPO(_), _, strm') => true
              | _ => false
            (* end case *))
      val (SR_RES, SR_SPAN, strm') = EBNF.closure(variables_PROD_1_SUBRULE_1_PRED, variables_PROD_1_SUBRULE_1_NT, strm)
      val (KW_endvars_RES, KW_endvars_SPAN, strm') = matchKW_endvars(strm')
      val FULL_SPAN = (#1(SR_SPAN), #2(KW_endvars_SPAN))
      in
        (UserCode.variables_PROD_1_ACT (SR_RES, KW_endvars_RES, SR_SPAN : (Lex.pos * Lex.pos), KW_endvars_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts_REFC, tree_REFC, vars_REFC),
          FULL_SPAN, strm')
      end
fun program_NT (strm) = let
      val (KW_title_RES, KW_title_SPAN, strm') = matchKW_title(strm)
      val (STR_RES, STR_SPAN, strm') = matchSTR(strm')
      val (SEMI_RES, SEMI_SPAN, strm') = matchSEMI(strm')
      val (KW_variables_RES, KW_variables_SPAN, strm') = matchKW_variables(strm')
      val (variables_RES, variables_SPAN, strm') = variables_NT(strm')
      val (KW_commands_RES, KW_commands_SPAN, strm') = matchKW_commands(strm')
      fun program_PROD_1_SUBRULE_1_NT (strm) = let
            val (commands_RES, commands_SPAN, strm') = commands_NT(strm)
            val FULL_SPAN = (#1(commands_SPAN), #2(commands_SPAN))
            in
              ((commands_RES), FULL_SPAN, strm')
            end
      fun program_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
             of (Tok.ID(_), _, strm') => true
              | (Tok.KW_Print, _, strm') => true
              | (Tok.KW_IF, _, strm') => true
              | (Tok.KW_WHILE, _, strm') => true
              | (Tok.KW_CASE, _, strm') => true
              | _ => false
            (* end case *))
      val (SR_RES, SR_SPAN, strm') = EBNF.closure(program_PROD_1_SUBRULE_1_PRED, program_PROD_1_SUBRULE_1_NT, strm')
      val FULL_SPAN = (#1(KW_title_SPAN), #2(SR_SPAN))
      in
        (UserCode.program_PROD_1_ACT (SR_RES, STR_RES, SEMI_RES, KW_title_RES, KW_variables_RES, KW_commands_RES, variables_RES, SR_SPAN : (Lex.pos * Lex.pos), STR_SPAN : (Lex.pos * Lex.pos), SEMI_SPAN : (Lex.pos * Lex.pos), KW_title_SPAN : (Lex.pos * Lex.pos), KW_variables_SPAN : (Lex.pos * Lex.pos), KW_commands_SPAN : (Lex.pos * Lex.pos), variables_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts_REFC, tree_REFC, vars_REFC),
          FULL_SPAN, strm')
      end
in
  (program_NT)
end
val program_NT =  fn s => unwrap (Err.launch (eh, lexFn, program_NT , true) s)

in (program_NT) end
  in
fun parse lexFn  s = let val (program_NT) = mk lexFn in program_NT s end

  end

end
