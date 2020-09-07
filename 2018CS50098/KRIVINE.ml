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

type table = (string * ans) list
and clos = table * exp
and ans = NUM of int
        | BOOL of bool
        | CLSR of clos ;;

let rec search (r:table) (x:string) :ans = match r with
      [] -> raise UNDEFINED_VARIABLE
    | (s,a) :: xs -> if x = s then a else search xs x ;;

let rec add (a1:ans) (a2:ans) :clos = match (a1,a2) with
      ( NUM(n1) , NUM(n2) ) -> ([],Num(n1+n2))
    | _ -> raise TYPE_MISSMATCHED ;;

let rec sub (a1:ans) (a2:ans) :clos = match (a1,a2) with
      ( NUM(n1) , NUM(n2) ) -> ([],Num(n1-n2))
    | _ -> raise TYPE_MISSMATCHED ;;

let rec mul (a1:ans) (a2:ans) :clos = match (a1,a2) with
      ( NUM(n1) , NUM(n2) ) -> ([],Num(n1*n2))
    | _ -> raise TYPE_MISSMATCHED ;;

let rec eql (a1:ans) (a2:ans) :clos = match (a1,a2) with
      ( NUM(n1) , NUM(n2) ) -> (if n1 = n2 then ([],Bool(true)) else ([],Bool(false)))
    | _ -> raise TYPE_MISSMATCHED ;;

let rec krivine (c:clos) (s:ans list) :ans = match c with
      (r,Var(x)) -> let a = (search r x) in (match a with
                                                  NUM (n) -> a
                                                | BOOL (b) -> a
                                                | CLSR (cl) -> (match cl with
                                                                    (r',Abst(f,g)) -> krivine ((x,a)::r',Abst(f,g)) s
                                                                  | _ -> krivine cl s ))
    | (r,Num(n)) -> NUM (n)
    | (r,Bool(b)) -> BOOL (b)
    | (r,Add(e1,e2)) -> krivine (add (krivine (r,e1) []) (krivine (r,e2) [])) s
    | (r,Sub(e1,e2)) -> krivine (sub (krivine (r,e1) []) (krivine (r,e2) [])) s
    | (r,Mul(e1,e2)) -> krivine (mul (krivine (r,e1) []) (krivine (r,e2) [])) s
    | (r,Eql(e1,e2)) -> krivine (eql (krivine (r,e1) []) (krivine (r,e2) [])) s
    | (r,Cond(e1,e2,e3)) -> let b = (krivine (r,e1) []) in (match b with
                                                             BOOL(true) -> krivine (r,e2) s
                                                           | BOOL(false) -> krivine (r,e3) s
                                                           | _ -> raise TYPE_MISSMATCHED )
    | (r,Abst(x,e)) -> (match s with
                            [] -> CLSR (c)
                          | d :: s' -> krivine ((x,d)::r,e) s')
    | (r,App(e1,e2)) -> krivine (r,e1) (CLSR(r,e2)::s) ;;

(* # krivine ([],Sub(Num(5),Num(4))) [] ;;
- : ans = NUM 1 *)

(* # krivine ([],Mul(Num(5),Num(4))) [] ;;
- : ans = NUM 20 *)

(* # krivine ([],Eql(Num(2),Num(2))) [] ;;
- : ans = BOOL true *)

(* # krivine ([],Eql(Num(2),Num(3))) [] ;;
- : ans = BOOL false *)

(* # krivine ([],Cond(Eql(Num(1),Num(2)),Num(1),Num(5))) [] ;;
- : ans = NUM 5 *)

(* # krivine ([],Cond(Eql(Num(1),Num(1)),Num(1),Num(5))) [] ;;
- : ans = NUM 1 *)

(* # krivine (["x",NUM(1)],Cond(Eql(Num(1),Var("x")),Num(1),Num(5))) [] ;;
- : ans = NUM 1 *)

(* # krivine (["x",NUM(5)],Cond(Eql(Num(1),Var("x")),Num(1),Num(5))) [] ;;
- : ans = NUM 5 *)

(* # krivine ([],App(Var("fact"),Num(2))) [] ;;
Exception: UNDEFINED_VARIABLE. *)

(* # krivine ([("fact",CLSR([],Abst("x",Cond(Eql(Num(1),Var("x")),Num(1),Mul(Var("x"),App(Var("fact"),Sub(Var("x"),Num(1))))))))],Var("fact")) [] ;;
- : ans =
CLSR
 ([("fact",
    CLSR
     ([],
      Abst ("x",
       Cond (Eql (Num 1, Var "x"), Num 1,
        Mul (Var "x", App (Var "fact", Sub (Var "x", Num 1)))))))],
  Abst ("x",
   Cond (Eql (Num 1, Var "x"), Num 1,
    Mul (Var "x", App (Var "fact", Sub (Var "x", Num 1)))))) *)

(* # krivine ([("fact",CLSR([],Abst("x",Cond(Eql(Num(1),Var("x")),Num(1),Mul(Var("x"),App(Var("fact"),Sub(Var("x"),Num(1))))))))],App(Var("fact"),Num(7))) [] ;;
- : ans = NUM 5040 *)

(* # krivine ([("fib",CLSR([],Abst("x",Cond(Eql(Var("x"),Num(0)),Num(0),Cond(Eql(Var("x"),Num(1)),Num(1),Add(App(Var("fib"),Sub(Var("x"),Num(1))),App(Var("fib"),Sub(Var("x"),Num(2)))))))))],App(Var("fib"),Num(15))) [] ;;
- : ans = NUM 610 *)
