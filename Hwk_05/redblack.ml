module type Set = sig
  type elem
  type set
  val empty : set
  val insert : elem -> set -> set
  val member : elem -> set -> bool (* * bool * elem list *)
(* false if blackk,,  true is red *)
  val fromOrdList : elem list -> set  
end

module type ORDERED =
sig
  type t
  val eq : t -> t -> bool
  val lt : t -> t -> bool		
  val leq : t -> t-> bool	
end

module OrdInt : (ORDERED with type t = int) = 
struct
   type t = int
   let eq (x:t) (y:t) = x == y
   let lt (x:t) (y:t) = x < y
   let leq (x:t) (y:t) = x <= y
end

module RedBlackSet (Element:ORDERED) : (Set with type elem = Element.t) = 
struct
	type elem = Element.t

	type color = R | B
	type tree = E | T of color * tree * elem * tree
	type set = tree

	let empty = E

	let rec member (x:elem) (t:set) = match t with
										| E -> false
										| T (c, a, y, b) -> if Element.lt x y then member x a
														else if Element.lt y x then member x b
														else true

	(* let rec member2 (x:elem) (t:set) (l:elem list) = match t with
													| E -> (false, false, [])
													| T (c, a, y, b) -> if Element.lt x y then member2 x a (y::l)
																		else if Element.lt y x then member2 x b (y::l)
																		(* else true *)
																		else match c with
																		| B -> (true, false, l)
																		| R -> (true, true, l)

	let member x t = member2 x t [] *)


	let balance (t:tree) = match t with
							| T (B, T (R, T (R, a, x, b), y, c), z, d) -> T (R, T (B, a, x, b), y, T (B, c, z, d))
							| T (B, T (R, a, x, T (R, b, y, c)), z, d) -> T (R, T (B, a, x, b), y, T (B, c, z, d))
							| T (B, a, x, T (R, T (R, b, y, c), z, d)) -> T (R, T (B, a, x, b), y, T (B, c, z, d))
							| T (B, a, x, T (R, b, y, T (R, c, z, d))) -> T (R, T (B, a, x, b), y, T (B, c, z, d))
							| T (c,a,y,b) -> T (c,a,y,b)
							| E -> E

	let rbalance (t:tree) = match t with
							| T (B, a, x, T (R, T (R, b, y, c), z, d)) -> T (R, T (B, a, x, b), y, T (B, c, z, d))
							| T (B, a, x, T (R, b, y, T (R, c, z, d))) -> T (R, T (B, a, x, b), y, T (B, c, z, d))
							| T (c,a,y,b) -> T (c,a,y,b)
							| E -> E

	let lbalance (t:tree) = match t with
							| T (B, T (R, T (R, a, x, b), y, c), z, d) -> T (R, T (B, a, x, b), y, T (B, c, z, d))
							| T (B, T (R, a, x, T (R, b, y, c)), z, d) -> T (R, T (B, a, x, b), y, T (B, c, z, d))
							| T (c,a,y,b) -> T (c,a,y,b)
							| E -> E

	let insert (x:elem) (s:set) = 
		let rec ins (t:set) = match t with
								| E -> T (R, E, x, E)
								| T (color, a, y, b) as s -> if Element.lt x y then lbalance (T (color, ins a, y, b))
														else if Element.lt y x then rbalance (T (color, a, y, ins b))
														else s
		in match ins s with
		| T (_, a, y, b) -> T (B, a, y, b)

	let rec fromOrdList (l:elem list) = match l with
											| [] -> E
											| x::xs -> insert x (fromOrdList xs)

end

module RB = RedBlackSet(OrdInt)

let rec mkRBSet l = match l with
				| [] -> RB.empty
				| x::xs -> RB.insert x (mkRBSet xs)

let tester1 = RB.fromOrdList [1;2;3;4;5]
let t1 = RB.member 1 tester1
let t2 = RB.member 2 tester1
let t3 = RB.member 3 tester1
let t4 = RB.member 4 tester1
let t5 = RB.member 5 tester1
let t6 = RB.member 6 tester1
let t7 = RB.member 7 tester1