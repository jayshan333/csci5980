module type ORDERED =
sig
  type t
  val eq : t -> t -> bool
  val lt : t -> t -> bool		
  val leq : t -> t-> bool	
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

  val findMin : heap -> Elem.t (* raise EMPTY is heap is empty *)
  val deleteMin: heap -> heap  (* raise EMPTY is heap is empty *)

  val fromList: Elem.t list -> heap
end

module OrdInt : (ORDERED with type t = int) = 
struct
   type t = int
   let eq (x:t) (y:t) = x == y
   let lt (x:t) (y:t) = x < y
   let leq (x:t) (y:t) = x <= y
end

module LeftistHeap (Element:ORDERED) : (Heap with type Elem.t = Element.t) = 
struct
	module Elem = Element
	type elem = Element.t
	type heap = E | T of int * Elem.t * heap * heap

	exception EMPTY

	let rank (h:heap) : int = match h with
							  | E -> 0
							  | T (x,_,_,_) -> x

	let makeT (x:Elem.t) (a:heap) (b:heap) : heap = if rank a >= rank b then T((rank b + 1), x, a, b)
											                     		else T((rank a + 1), x, a, b)
	let empty = E

	let isEmpty (h:heap) : bool = h = empty

	let rec merge (h1:heap) (h2:heap) : heap = match h1, h2 with
											   | h, E -> h
											   | E, h -> h
											   | T (_, x, a1, b1), T (_, y, a2, b2) -> if Elem.leq x y then makeT x a1 (merge b1 h2)
											                                                           else makeT y a2 (merge h1 b2)
	(* let insert (x:Elem.t) (h:heap) = merge (T (1, x, E, E)) h	 *)

	let rec insert (x:Elem.t) (h:heap) : heap = match h with
												| E -> T (1, x, E, E) 
				                       			| T(_, y, a, b) -> if Elem.leq x y then makeT x E h
				                       											   else makeT y a (insert x b)

	let findMin (h:heap) : elem = match h with
							      | E -> raise EMPTY
		                          | T(_, x, a, b) -> x

	let deleteMin (h:heap) : heap = match h with
								    | E -> raise EMPTY
			                        | T(_, x, a, b) -> merge a b

	

	let rec convert2Heap (l:Elem.t list) : heap list = match l with
													   | [] -> []
													   | x::xs -> (T (1, x, E, E)) :: convert2Heap xs

	let rec merger (l:heap list) : heap list = match l with
												   | [] -> []
												   | x::[] -> x::[]
												   | x1::x2::xs -> merge x1 x2 :: merger xs

	let rec looper (l:heap list) : heap = match l with
									      | [] -> E 
									      | x::[] -> x
									      | ls -> looper(merger(ls))

	let fromList (l:Elem.t list) : heap = looper (convert2Heap l)

end


module LH = LeftistHeap(OrdInt)

let rec mkLHeap l = match l with
				| [] -> LH.empty
				| x::xs -> LH.insert x (mkLHeap xs)

let tester1 = mkLHeap [2;1;3]
let tester2 = mkLHeap [5;6;7;4;3;1]

let t1 = LH.isEmpty tester2 == false
let t2 = LH.isEmpty (LH.empty) == true
let t3 = LH.findMin tester1 == 1
let t4 = LH.findMin tester2 == 1
(* let t5 = LH.findMin (LH.empty) *)
let t6 = LH.findMin (LH.deleteMin tester1) == 2
let t7 = LH.findMin (LH.deleteMin tester2) == 3
(* let t8 = LH.findMin (LH.deleteMin LH.empty) *)
let t9 = LH.findMin (LH.insert 0 tester1) == 0
let t10 = LH.findMin (LH.deleteMin (LH.insert 2 tester2)) == 2

let tester3 = LH.fromList []
let tester4 = LH.fromList [1;2;3;4]
let tester5 = LH.fromList [5;4;3;2;9]

let t11 = LH.isEmpty tester3 == true
let t12 = LH.isEmpty tester4 == false
let t13 = LH.findMin tester4 == 1
let t14 = LH.findMin tester5 == 2
let t15 = LH.findMin (LH.deleteMin tester4) == 2
let t16 = LH.findMin (LH.deleteMin tester5) == 3
let t17 = LH.findMin (LH.deleteMin (LH.deleteMin tester5)) == 4
let t18 = LH.findMin (LH.deleteMin (LH.deleteMin (LH.deleteMin tester5))) == 5
let t19 = LH.findMin (LH.deleteMin (LH.deleteMin (LH.deleteMin (LH.deleteMin tester5)))) == 9
let t20 = LH.isEmpty (LH.deleteMin (LH.deleteMin (LH.deleteMin (LH.deleteMin (LH.deleteMin tester5))))) == true
