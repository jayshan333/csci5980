module type Set = sig
  type elem
  type set
  val empty : set
  val insert : elem -> set -> set
  val member : elem -> set -> bool
end

module type EQUAL =
sig
  type t
  val eq  : t -> t -> bool
end

module type ORDERED =
sig
  type t
  val eq : t -> t -> bool
  val lt : t -> t -> bool		
  val leq : t -> t-> bool	
end

module EqInt : (EQUAL with type t = int) =
struct
  type t = int
  let eq (x:t) (y:t) = x == y
end

module OrdInt : (ORDERED with type t = int)= 
struct
   type t = int
   let eq (x:t) (y:t) = x == y
   let lt (x:t) (y:t) = x < y
   let leq (x:t) (y:t) = x <= y
end

module ListSet (Element:EQUAL) : (Set with type elem = Element.t) =
struct
  type elem = Element.t
  type set = Element.t list
  let empty = []
  let rec insert (e:elem) (l:set) = match l with
  					  | [] -> [e]
  					  | x::xs -> x::(insert e xs)
  let rec member (e:elem) (l:set) =  match l with
  						   | [] -> false
  						   | x::xs -> if Element.eq e x then true else (member e xs)
end

module UnbalancedSet (Element:ORDERED) : (Set with type elem = Element.t) =
struct
  type elem = Element.t
  type tree = E | T of tree * elem * tree
  type set = tree
  
  let empty = E

  (* let rec member (e:elem) (t:set) = match t with
  									| E -> false
  									| T (a,y,b) -> if Element.lt e y then member e a
  												   else if Element.lt y e then member e b
  												   else true	 *)
(* g is my guess *)
  let rec member2 (e:elem) (t:set) (g:elem) = match t with
  											  | E -> Element.eq e g
  											  | T (a,y,b) -> if Element.leq e y then member2 e a y
  																				else member2 e b g

  let member (e:elem) (t:set)	= match t with
  								  | E -> false
  								  |	T (a,y,b) -> member2 e t y	

  let rec insert (e:elem) (t:set) = match t with
  									| E -> T (E,e,E)
  									| ((T (a,y,b)) as s) -> if Element.lt e y then T ((insert e a),y,b)
  															else if Element.lt y e then T (a,y,(insert e b))
  															else s

end															



module LS = ListSet(EqInt)

module TS = UnbalancedSet(OrdInt)

let rec mkLSet l = match l with
				| [] -> LS.empty
				| x::xs -> LS.insert x (mkLSet xs)

let tester1 = mkLSet [2;1;3]
let tester2 = mkLSet []

let t1 = LS.member 3 tester1
let t2 = LS.member 5 tester1
let t3 = LS.member 5 (LS.insert 5 tester1)
let t4 = LS.member 2 tester2
let t5 = LS.member 2 (LS.insert 2 tester2)

let rec mkTSet l = match l with 
        | [] -> TS.empty
        | x::xs -> TS.insert x (mkTSet xs)

let tester3 = mkTSet [2;1;3]

let t6 = TS.member 2 tester3
let t7 = TS.member 7 tester3
let t8 = TS.member 7 (TS.insert 7 tester3)
