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

module type Heap = 
sig
  module Elem : ORDERED
  type elem
  type heap
  
  val empty : heap
  val isEmpty : heap -> bool

  val insert : Elem.t -> heap -> heap
  val merge : heap -> heap -> heap 

  val findMin : heap -> Elem.t (* raise empty is heap is empty *)
  val deleteMin: heap -> heap (* raise empty is heap is empty *)
end

module LeftistHeap (Element:ORDERED with elem = Element.t) : Heap = 
struct
	module Elem = Element
	type elem = Element.t
	type heap = E | T of int * Elem.t * heap * heap

	let rank (h:heap) = match h with
						| E -> 0
						| T (x,_,_,_) -> x

	let makeT (x:Elem.t) (a:heap) (b:heap) = if rank a >= rank b then T((rank b + 1), x, a, b)
											                     else T((rank a + 1), x, a, b)

	let empty = E
	let isEmpty (h:heap) = h = empty

	let rec merge (h1:heap) (h2:heap) = match h1, h2 with
					 | h, E -> h
					 | E, h -> h
					 | T (_, x, a1, b1), T (_, y, a2, b2) -> if Elem.leq x y then makeT x a1 (merge b1 h2)
					                                                         else makeT y a2 (merge h1 b2)
	let insert (x:Elem.t) (h:heap) = merge (T (1, x, E, E)) h	

	let rec insert2 (x:Elem.t) (h:heap) = match h with
									| E -> T (1, x, E, E) 
	                       			| T(_, y, a, b) -> if Elem.leq x y then makeT x E h
	                       											   else makeT y a (insert2 x b)
	exception Empty

	let findMin (h:heap) = match h with
						   | E -> raise Empty
	                       | T(_, x, a, b) -> x
	let deleteMin (h:heap) = match h with
						   | E -> raise Empty
	                       | T(_, x, a, b) -> merge a b

	

	let rec convert2Heap (l:Elem.t list) = match l with
							| [] -> []
							| x::xs -> (T (1, x, E, E)) :: convert2Heap xs

	let rec comboMagic (l:heap list) = match l with
										| [] -> []
										| x::[] -> x::[]
										| x1::x2::xs -> merge x1 x2 :: comboMagic xs

	let rec looper (l:heap list) = match l with
								   | [] -> E 
								   | x::[] -> x
								   | ls -> looper(comboMagic(ls))

	let fromList (l:Elem.t list) = looper (convert2Heap l)

	(* convert2Heap O(n) time + looper
	looper runs in log(n) passes
	comboMagic likewise runs in log(n) passes
	merge 1 the first run 2 the second run and so on until either you are merge two equal things of n/2 or merge n-1 with something
	so i think that this would come out to be O(n) at the end.   *)

end


module LH = LeftistHeap(OrdInt)

let rec mkLHeap l = match l with
				| [] -> LH.empty
				| x::xs -> LH.insert x (mkLHeap xs)


