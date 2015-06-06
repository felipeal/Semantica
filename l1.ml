type variable = string;;

type operator =
    Sum | Diff | Mult | Div | Eq | Leq;;

type value = 
   Vnum of int | Vbool of bool;;

type expr = 
   Num of int | Bool of bool
   | Bop of expr * operator * expr
   | If of expr * expr * expr
   | Var of variable
   | Lam of variable * expr
   | App of expr * expr;;

exception NoRuleApplies ;;

let rec replace (x : variable) v (e : expr) : expr = match e with
    Bop(e1, op, e2) -> Bop(replace x v e1, op, replace x v e2)
    | If(e1, e2, e3) -> If(replace x v e1, replace x v e2, replace x v e3)
    | App(e1, e2) -> App(replace x v e1, replace x v e2)
    | Var(y) -> (if compare x y == 0 (* Substitui variável de mesmo nome *)
        then v
        else Var(y))
    | _ -> e;;

let rec step (e : expr) : expr = match e with
    
    (* Regra OP+ *)
    Bop(Num e1, Sum, Num e2) -> Num (e1 + e2)
    
    (* Regra OP- *)
    | Bop(Num e1, Diff, Num e2) -> Num (e1 - e2)
    
    (* Regra OP* *)
    | Bop(Num e1, Mult, Num e2) -> Num (e1 * e2)
    
    (* Regra OP/ *)
    | Bop(Num e1, Div, Num e2) -> Num (e1 / e2)

    (* Regra OP== Num   ai ele vai comparar funções? tipo, funções podem vir a ser expressoes com let*)
    | Bop(Num e1, Eq, Num e2) -> Bool (e1 = e2)
    
    (* Regra OP== Bool *)
    | Bop(Bool e1, Eq, Bool e2) -> Bool (e1 = e2)
    
    (* Regra OP<= *)
    | Bop(Num e1, Leq, Num e2) -> Bool (e1 <= e2)
    
    (* Regra de reescrita OP2 *)
    | Bop(Num e1, op, e2) ->
        let e2_step = step e2
            in Bop(Num e1, op, e2_step)
    
    (* Regra de reescrita OP1 *)
    | Bop(e1, op, e2) ->
        let e1_step = step e1
            in Bop(e1_step, op, e2)
            
    (* Regra IF1 *)
    | If(Bool true, e1, _) ->  e1
    
    (* Regra IF2 *)
    | If(Bool false, _, e2) ->  e2
    
    (* Regra de reescrita IF3 *)
    | If(e1, e2, e3) -> 
        let e1_step = step e1
            in If(e1_step, e2, e3)
    
    (* Regra BETA de Aplicação *)
    | App(Lam(x, e1), Num v) -> (* DEVE SER VALUE V *)
        replace x (Num(v)) e1 (* Substitui x por v na expressão e1 *)
    
    (* Regra de reescrita APP2 *)
    | App(Lam(x, Num v), e2) -> (* DEVE SER VALUE V *)
        let e2_step = step e2
            in App(Lam(x, (Num(v))), e2_step)
    
    (* Regra de reescrita APP1 *)
    | App(Lam(x, e1), e2) ->
        let e1_step = step e1
            in App(Lam(x, e1_step), e2)
    
    (* Nenhuma regra *)
    | _ -> raise NoRuleApplies;;

let rec eval (e: expr) : value = match e with
    Num e1 -> Vnum e1 
    | Bool e1 -> Vbool e1
    (*
    | Fun(_,_,_) -> e
    *)
    | _ -> eval (step e) ;;

let print_value (v : value) = match v with
    Vnum e1 -> print_int e1 ; print_string "\n"
    | Vbool true -> (print_string  "true\n")
    | Vbool false -> (print_string "false\n")
    | _ -> raise NoRuleApplies ;;

(* Testes *)

(* Expected: 1 2 3 4 *)
print_value (Vnum(1));;
let test_sum = Bop(Num(1), Sum, Num(1));;
print_value (eval test_sum);;
print_value (eval (Bop(Num(1), Sum, Num(2))));;
print_value (eval (Bop(test_sum, Sum, test_sum)));;

(* Expected: true false true false *)
let test_eq = Bop(test_sum, Eq, test_sum);;
print_value (eval test_eq);;
print_value (eval (Bop(test_sum, Eq, Num(0))));;
print_value (eval (Bop(Bool(true), Eq, Bool(true))));;
print_value (eval (Bop(Bool(true), Eq, Bool(false))));;

(* Expected: 1 2 3 4 *)
print_value (eval (If(Bool (true), Num(1), Num(0))));;
print_value (eval (If(Bool (false), Num(0), test_sum)));;
print_value (eval (If(Bool (true), Bop(Num(1), Sum, Num(2)), test_sum)));;
print_value (eval (If(test_eq, Num(4), Num(0))));;

let test_lambda_1 = Lam("x", Var("x"));;
let test_lambda_2 = Lam("x", (Bop(Var("x"), Sum, Var("x"))));;
print_value (eval (App(test_lambda_1, Num(1))));;
print_value (eval (App(test_lambda_2, Num(1))));;
print_value (eval (App(test_lambda_1, test_sum)));;