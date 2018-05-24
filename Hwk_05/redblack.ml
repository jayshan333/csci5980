module type Set = sig
  type elem
  type set
  val empty : set
  val insert : elem -> set -> set
  val member : elem -> set -> bool  
end

module type ORDERED =
sig
  type t
  val eq : t -> t -> bool
  val lt : t -> t -> bool		
  val leq : t -> t-> bool	
end

module OrdInt : ORDERED = 
struct
   type t = int
   let eq (x:t) (y:t) = x == y
   let lt (x:t) (y:t) = x < y
   let leq (x:t) (y:t) = x <= y
end

module RedBlackSet (Element:ORDERED) : Set = 
struct
	type elem = Element.t

	type color = R | B
	type tree = E | T of color * tree * elem * tree
	type set = tree

	let empty = E

	let rec member (x:elem) (t:set) = match t with
										| E -> false
										| T (_, a, y, b) -> if Element.lt x y then member x a
														else if Element.lt y x then member x b
														else true

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

	let rec looper (l:elem list) (t:set) = match l with
											| [] -> t
											| x::xs -> looper xs (insert x t)

	let fromOrdList (l:elem list) = looper l E




end