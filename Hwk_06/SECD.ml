(* Lists, translations needs work *)

type value
  = Int of int
  | Bool of bool
  | List of value list
  | Closure of env * string * expr
  | Addv
  | Subv
  | Mulv
  | EqIntv
  | Ifv
  | Consv
  | Nilv
and expr
  = Var of string
  | App of expr * expr
  | Abs of string * expr
  | Apply

  | IntLit of int
  | Add of expr * expr
  | Adde
  | Sub of expr * expr
  | Sube
  | Mul of expr * expr
  | Mule
  | EqInt of expr * expr
  | EqInte

  | True
  | False
  | If of expr
  | Ife

  | Cons of expr * expr
  | Nil
  | Nile
  | Conse

  | Y

  | IfTE of expr * expr * expr
  | Where of expr * decl
  | WhereRec of expr * decl
and decl
  = Decl of string * string list * expr
and env
  = (string * expr) list

(* This should be changed, obviously, to your actual type for
   representing the S, E, C, and D components of the machine. 
 *)
type state = value list * env *  expr list * (value list * env *  expr list) list

(* Some exceptions you might chose to raise. *)
exception InvalidSyntax of string * expr
exception UndeclaredName of string
exception Fail of state

let addEnv (str:string) (v:value) (ce:env) : env =
	match v with
	| Int n -> (str,IntLit n)::ce
	| _ -> raise (Failure "Can't add to env")


let rec lookup (str:string) (env:env) : value =
	match env with
	| (x,IntLit xs)::rest -> if str = x then Int xs else lookup str rest
	| _ -> raise (UndeclaredName str)

let makeList (l:value) (r:value list) : value list =
  l :: r
(* 
let reverse l =
  let rec reverse_h l l2 =
  match l with
  | [] -> l2
  | x::xs -> reverse_h (x::l2) xs
  in reverse_h l [] 

let extract expr strlist =
  let rec extract_h expr strlist elist
  match expr,strlist with
  | Var t,[] -> Var t
  | App 

let rec translate (st:state) : value =
  match st with
  | (s,e, Where(expr,Decl(str,strlist,dexpr))::rest,d) -> extract(expr,reverse(strlist)) 
  | _ -> raise (Fail st) *)

let rec go (st:state) : value =
  match st with
  |(v::[],_,[],[])-> v
  |(v::[],_,[],(s,e,c)::rest) -> go (v::s, e, c, rest)
  |(s,e,(Var t)::rest, d) -> go ((lookup t e)::s,e,rest,d)
  |(s,e,(App (t,r))::rest,d) -> go (s,e, r::t::rest,d)
  |(s,e,(Abs (str, x))::rest,d) -> go ((Closure (e, str, x))::s,e,Apply::rest,d)
  |(s,e, (IntLit n)::rest,d) -> go ((Int n)::s,e,rest,d)
  |(s,e, (Add (l,r))::rest,d) -> go (s,e,(r::l::Adde::Apply::rest),d)
  |(s,e, (Sub (l,r))::rest,d) -> go (s,e,(r::l::Sube::Apply::rest),d)
  |(s,e, (Mul (l,r))::rest,d) -> go (s,e,(r::l::Mule::Apply::rest),d)
  |(s,e, (EqInt (l,r))::rest,d) -> go (s,e,(r::l::EqInte::Apply::rest),d)
  |(s,e, True::rest,d) -> go ((Bool true)::s,e,rest,d)
  |(s,e, False::rest,d) -> go ((Bool false)::s,e,rest,d)
  |(s,e, (Cons (l,r))::rest,d) -> go (s,e, (r::l::Conse::Apply::rest),d)
  |(s,e, Nil::rest,d) -> go (s,e,Nile::Apply::rest,d)
  |(s,e, If(ex)::rest,d) -> go (s,e,ex::Ife::Apply::rest,d)

  |(s,e,Adde::rest,d) -> go (Addv::s,e,rest,d)
  |(s,e,Sube::rest,d) -> go (Subv::s,e,rest,d)
  |(s,e,Mule::rest,d) -> go (Mulv::s,e,rest,d)
  |(s,e,EqInte::rest,d) -> go (EqIntv::s,e,rest,d)
  |(s,e,Conse::rest,d) -> go (Consv::s,e,rest,d)
  |(s,e,Nile::rest,d) -> go (Nilv::s,e,rest,d)
  |(s,e,Ife::rest,d) -> go (Ifv::s,e,rest,d)

  |(Addv::Int l::Int r::srest,e,Apply::crest,d) -> go (Int (l+r)::srest,e,crest,d)
  |(Subv::Int l::Int r::srest,e,Apply::crest,d) -> go (Int (l-r)::srest,e,crest,d)
  |(Mulv::Int l::Int r::srest,e,Apply::crest,d) -> go (Int (l*r)::srest,e,crest,d)
  |(EqIntv::Int l::Int r::srest,e,Apply::crest,d) -> go (Bool (l=r)::srest,e,crest,d)
  |(Closure (ce, str, x):: v:: srest, e, Apply::crest, d) -> go ([],(addEnv str v ce),x::[],(srest,e,crest)::d)
  |(Consv::l::List r::srest, e, Apply::crest,d) -> go ((List (makeList l r))::srest,e,crest,d)
  |(Nilv::srest, e, Apply::crest, d) -> go ((List [])::srest,e,crest,d)
  |(Ifv::Bool true::thener::elser::srest, e, Apply::crest,d) -> go (thener::srest,e,crest,d)
  |(Ifv::Bool false::thener::elser::srest, e, Apply::crest,d) -> go (elser::srest,e,crest,d)

  (* Failure to match *)
  | _ -> raise (Fail st) (* "Failed to match a case.")   *)

let eval e = go ([], [], e::[], []) 

(* Ensure that your 'expr' type allows these expressions to be created. 
   The OCaml-like syntax of the expressions is in quotes.
 *)

(* "42" *)
let e1 = IntLit 42
let v1 = eval e1

(* "42 + 1000" *)
let e2 = Add (IntLit 42, IntLit 1000)
let v2 = eval e2

(* "100 * 12" *)
let e3 = Mul (IntLit 100, IntLit 12)
let v3 = eval e3

(* "(\x . x) 42 " *)
let e4 = App ( Abs ("x", Var "x"), IntLit 42)
let v4 = eval e4

(* "(\x . x + 1000) 42" *)
let e6 = App ( Abs ("x", Add (Var "x", IntLit 1000)), IntLit 42)
let v6 = eval e6

(* "10 + 4 * (5 - 2)" *)
let e7 = Add (IntLit 10, Mul (IntLit 4, Sub (IntLit 5, IntLit 2)))
let v7 = eval e7

(* (* 3 :: [] *)
let e8 = Cons(IntLit 3, Nil)
let v8 = eval e8 *)

(* "<e7> :: <e6> :: []" *)
let e8 = Cons (e7, Cons (e6, Nil))
let v8 = eval e8

(* "if true then 1 else 2" *)
let e9 = App (If (True), Cons (IntLit 1, Cons (IntLit 2, Nil)))
let v9 = eval e9

(* "if false then 1 else 2" *)
let e10 = App (If (False), Cons (IntLit 1, Cons (IntLit 2, Nil)))
let v10 = eval e10

(* "1 = 2" *)
let e11 = EqInt (IntLit 1, IntLit 2)
let v11 = eval e11

let reverse l =
  let rec reverse_h l l2 =
  match l with
  | [] -> l2
  | x::xs -> reverse_h xs (x::l2) 
  in reverse_h l [] 

let rec whereTranslate (e:expr) =
  match e with 
  | Where(expr,Decl(str,[],dexpr)) -> App ( Abs (str, expr), dexpr) 
  | Where(App(t,r),Decl(str,x::xs,dexpr)) -> App (Abs (x, whereTranslate (Where(t, Decl(str,xs,dexpr))) ),r)
  | _ -> raise (InvalidSyntax ("whereTranslate",e))

let translate (e:expr) : expr =   
  match e with
  (* | IfTE(ifer,thener,elser) -> App (App (If (True), Cons ((Abs ("_", thener))), Cons ((Abs ("_", elser))), Nil)), "Dummy") *)
  (* | Where(expr,Decl(str,strlist,dexpr)) -> whereTranslate (Where(expr,Decl(str,reverse(strlist),dexpr))) *)
  (* | WhereRec(expr,Decl(str,strlist,dexpr)) ->  *)
  | _ -> raise (InvalidSyntax ("translate",e))

 (* Abs ("_", thener)
dummy string *)

(* "if true then 1 else 2" \
  (* | IfTE(ifer,thener,elser) -> App(If(ifer),App (App (Abs ("x", thener), IntLit 3),elser)) *)
*)
let e12 = IfTE (True, IntLit 1, IntLit 2)
let v12 = eval (translate e12)
(* #trace go;; go ([], [], (translate e12)::[], []);;  *)

(* "if false then 1 else 2" *)
(* let e13 = IfTE (False, IntLit 1, IntLit 2)
let v13 = eval (translate e13) *)

let e13 = IfTE (False, Var "joker", IntLit 2)
let v13 = eval (translate e13)
(* #trace go;; go ([], [], (translate e13)::[], []);;  *)


let factBody 
  = IfTE (EqInt (Var "n", IntLit 0), 
          IntLit 1, 
          Mul(Var "n", App(Var "f", Sub (Var "n", IntLit 1))) )

let factF = Abs ("f", Abs ("n", factBody))

(* "fact 4" *)
(* let e14 = App (App (Y, factF), IntLit 4)
let v14 = eval (translate e14) *)


(* "x + 100 where x = 42" *)
let e15 = Where (Add (Var "x", IntLit 100), Decl("x", [], IntLit 42))
let v15 = eval (translate e15)

(* "add3 3 4 5 where add3 x y z = x + y + z" *)
let e16 = Where (App (App (App (Var "add3", IntLit 3), IntLit 4), 
                      IntLit 5),
                 Decl ("add3", ["x"; "y"; "z"], 
                       Add (Mul (Var "x", Var "y"), Var "z")))
let v16 = eval (translate e16)

(* "f 4 where rec f n = ...factorial..." *)
let e17 = WhereRec (App (Var "f", IntLit 4),
                    Decl ("f", ["n"], factBody))
let v17 = eval (translate e17)