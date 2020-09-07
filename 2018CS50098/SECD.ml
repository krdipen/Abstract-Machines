exception EMPTY_STACK
exception UNDEFINED_VARIABLE
exception TYPE_MISSMATCHED

type exp = Var of string
         | Num of int
         | Bool of bool
         | Add of exp * exp
         | Sub of exp * exp
         | Mul of exp * exp
         | Eql of exp * exp
         | Cond of exp * exp * exp
         | Abst of string * exp
         | App of exp * exp ;;

type opcode = VAR of string
         | NUM of int
         | BOOL of bool
         | ADD | SUB | MUL | EQL
         | COND of (opcode list) * (opcode list)
         | ABST of string * (opcode list)
         | RTRN | APP ;;

type table = (string * ans) list
and ans = INT of int
        | BUL of bool
        | VCL of table * (string * (opcode list)) ;;

let rec search (r:table) (x:string) :ans = match r with
      [] -> raise UNDEFINED_VARIABLE
    | (s,a) :: xs -> if x = s then ( match a with
                                          INT(n) -> a
                                        | BUL(b) -> a
                                        | VCL(r,(f,g)) -> VCL((x,a)::r,(f,g)) ) else search xs x ;;

let rec compile (e:exp) :opcode list = match e with
      Var(x) -> [VAR(x)]
    | Num(n) -> [NUM(n)]
    | Bool(b) -> [BOOL(b)]
    | Add(e1,e2) -> (compile e1) @ (compile e2) @ [ADD]
    | Sub(e1,e2) -> (compile e1) @ (compile e2) @ [SUB]
    | Mul(e1,e2) -> (compile e1) @ (compile e2) @ [MUL]
    | Eql(e1,e2) -> (compile e1) @ (compile e2) @ [EQL]
    | Cond(e1,e2,e3) -> (compile e1) @ [COND((compile e2),(compile e3))]
    | Abst(x,e) -> [ABST(x,(compile e) @ [RTRN])]
    | App(e1,e2) -> (compile e1) @ (compile e2) @ [APP] ;;

let rec stkmc (s:ans list) (e:table) (c:opcode list) (d:((ans list) * table * (opcode list)) list) :ans = match c with
      [] -> (match s with
                  [] -> raise EMPTY_STACK
                | a :: a' -> a)
    | VAR(x) :: c' -> stkmc ((search e x)::s) e c' d
    | NUM(n) :: c' -> stkmc (INT(n)::s) e c' d
    | BOOL(b) :: c' -> stkmc (BUL(b)::s) e c' d
    | ADD :: c' -> (match s with
                          [] -> raise EMPTY_STACK
                        | a :: [] -> raise EMPTY_STACK
                        | INT(i1) :: INT(i2) :: a' -> stkmc (INT(i1+i2)::a') e c' d
                        | _ -> raise TYPE_MISSMATCHED )
    | SUB :: c' -> (match s with
                          [] -> raise EMPTY_STACK
                        | a :: [] -> raise EMPTY_STACK
                        | INT(i1) :: INT(i2) :: a' -> stkmc (INT(i2-i1)::a') e c' d
                        | _ -> raise TYPE_MISSMATCHED )
    | MUL :: c' -> (match s with
                          [] -> raise EMPTY_STACK
                        | a :: [] -> raise EMPTY_STACK
                        | INT(i1) :: INT(i2) :: a' -> stkmc (INT(i1*i2)::a') e c' d
                        | _ -> raise TYPE_MISSMATCHED )
    | EQL :: c' -> (match s with
                          [] -> raise EMPTY_STACK
                        | a :: [] -> raise EMPTY_STACK
                        | INT(i1) :: INT(i2) :: a' -> (if i1 = i2 then stkmc (BUL(true)::a') e c' d else stkmc (BUL(false)::a') e c' d )
                        | _ -> raise TYPE_MISSMATCHED )
    | COND(c1,c2) :: c' -> (match s with
                                  BUL(true) :: a' -> stkmc a' e (c1 @ c') d
                                | BUL(false) :: a' -> stkmc a' e (c2 @ c') d
                                | _ -> raise TYPE_MISSMATCHED )
    | ABST(x,c1) :: c' -> stkmc (VCL(e,(x,c1))::s) e c' d
    | APP :: c' -> (match s with
                          [] -> raise EMPTY_STACK
                        | a :: [] -> raise EMPTY_STACK
                        | INT(n) :: VCL(e1,(x,c1)) :: a' -> stkmc [] ((x,INT(n))::e1) c1 ((a',e,c')::d)
                        | _ -> raise TYPE_MISSMATCHED )
    | RTRN :: c' -> (match s with
                          [] -> raise EMPTY_STACK
                        | a :: a' -> (match d with
                                          [] -> raise EMPTY_STACK
                                        | (s',e',c1) :: d' -> stkmc (a::s') e' c1 d' )) ;;

(* # compile (App(Var("fact"),Num(7))) ;;
- : opcode list = [VAR "fact"; NUM 7; APP] *)

(* # compile (Abst("x",Cond(Eql(Num(1),Var("x")),Num(1),Mul(Var("x"),App(Var("fact"),Sub(Var("x"),Num(1))))))) ;;
- : opcode list = [ABST ("x",[NUM 1; VAR "x"; EQL; COND ([NUM 1], [VAR "x"; VAR "fact"; VAR "x"; NUM 1; SUB; APP; MUL]); RTRN])] *)

(* # stkmc [] [("fact",VCL([],("x",[NUM 1; VAR "x"; EQL; COND ([NUM 1], [VAR "x"; VAR "fact"; VAR "x"; NUM 1; SUB; APP; MUL]); RTRN])))] [VAR "fact"; NUM 7; APP] [] ;;
- : ans = INT 5040 *)

(* # compile (App(Var("fib"),Num(15))) ;;
- : opcode list = [VAR "fib"; NUM 15; APP] *)

(* # compile (Abst("x",Cond(Eql(Var("x"),Num(0)),Num(0),Cond(Eql(Var("x"),Num(1)),Num(1),Add(App(Var("fib"),Sub(Var("x"),Num(1))),App(Var("fib"),Sub(Var("x"),Num(2)))))))) ;;
- : opcode list = [ABST ("x",[VAR "x"; NUM 0; EQL; COND ([NUM 0], [VAR "x"; NUM 1; EQL; COND ([NUM 1], [VAR "fib"; VAR "x"; NUM 1; SUB; APP; VAR "fib"; VAR "x"; NUM 2; SUB; APP; ADD])]); RTRN])] *)

(* # stkmc [] [("fib",VCL([],("x",[VAR "x"; NUM 0; EQL; COND ([NUM 0], [VAR "x"; NUM 1; EQL; COND ([NUM 1], [VAR "fib"; VAR "x"; NUM 1; SUB; APP; VAR "fib"; VAR "x"; NUM 2; SUB; APP; ADD])]); RTRN])))] [VAR "fib"; NUM 15; APP] [] ;;
- : ans = INT 610 *)
