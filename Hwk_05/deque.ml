module type Deque = sig
  type 'a queue

  val empty : 'a queue
  val is_empty : 'a queue -> bool

  (* insert, inspect, amd remove the front element *)
  val cons : 'a -> 'a queue -> 'a queue
  val head : 'a queue -> 'a       (* raises EMPTY if queue is empty *)
  val tail : 'a queue -> 'a queue (* raises EMPTY if queue is empty *)

  (* insert, inspect, amd remove the rear element *)
  val snoc : 'a queue -> 'a -> 'a queue
  val last : 'a queue -> 'a       (* raises EMPTY if queue is empty *)
  val init : 'a queue -> 'a queue (* raises EMPTY if queue is empty *)
end

module DequeImp : Deque = 
struct
  type 'a queue = 'a list * 'a list
  exception EMPTY
  exception NOFIX
  exception FIXFAILURE

  let empty = ([],[])

  let is_empty (q:'a queue) = match q with
                  |([],[]) -> true
                  | _ -> false

  let count (l:'a list) : int = 
    let rec counter (l:'a list) (number:int) = match l with 
                                                | [] -> number
                                                | x::xs -> counter xs (number+1)
    in counter l 0

  let rev (l:'a list) : 'a list = 
    let rec reverser (oldl:'a list) (newl:'a list) = match oldl with
                                                     | [] -> newl
                                                     | x::xs -> reverser xs (x::newl)
    in reverser l []

  let split (l:'a list) (number:int) : ('a list * 'a list) =
    let rec spliter (left:'a list) (number:int) (front:'a list) = if number = 0 then (rev front, left)
                                                                  else match l with
                                                                       | x::xs -> spliter xs (number-1) (x::front)
                                                                       | [] -> raise EMPTY
      
    in spliter l number []
  
  let fix (q:'a queue) : 'a queue = match q with
                                    |([],l) -> let middle = (count l)/2 
                                                in (match split l middle with
                                                   | (xs,ys) -> (rev ys,xs))
                                    |(l,[]) -> let middle = (count l)/2
                                                in (match split l middle with
                                                   | (xs,ys) -> (xs,rev ys))
                                    | _ -> q

  let cons (x:'a) (q:'a queue) = match q with
                                 | (xs,ys) -> (x::xs,ys)
  
  let head (q:'a queue) = match q with 
                          | ([],[]) -> raise EMPTY
                          | ([], y::[]) -> y
                          | ([], ys) -> (match (fix q) with
                                            | (x1::xs1,ys1) -> x1
                                            | _ -> raise FIXFAILURE)
                          | (x::xs,_) -> x


  let tail (q:'a queue) = match q with
                          | ([],[]) -> raise EMPTY
                          | ([],y::[]) -> ([],[])
                          | ([],ys) -> (match (fix q) with
                                        | (x1::xs1,ys1) -> (xs1,ys1)
                                        | _ -> raise FIXFAILURE)
                          | (x::xs,ys) -> (xs,ys)

                                      

  let snoc (q:'a queue) (x:'a) = match q with
                                 | (xs,ys) -> (xs,x::ys)

  let last (q:'a queue) = match q with
                          | ([],[]) -> raise EMPTY
                          | (x::[],[]) -> x
                          | (xs,[]) -> (match (fix q) with
                                        | (xs1,y1::ys1) -> y1
                                        | _ -> raise FIXFAILURE)
                          | (_,y::ys) -> y


  let init (q:'a queue) = match q with
                          | ([],[]) -> raise EMPTY
                          | (x::[],[]) -> ([],[])
                          | (xs,[]) -> (match (fix q) with
                                        |(xs1,y1::ys1) -> (xs1,ys1)
                                        | _ -> raise FIXFAILURE)
                          | (xs,y::ys) -> (xs,ys)
end

module DQ = DequeImp

let tester1 = DQ.snoc DQ.empty 3 
let tester2 = DQ.snoc (DQ.snoc (DQ.snoc (DQ.snoc DQ.empty 1) 3) 5) 7
let tester3 = DQ.cons 2 (DQ.cons 4 (DQ.cons 6 (DQ.cons 8 DQ.empty)))

let t1 = DQ.head (DQ.tail tester2) == 3
let t2 = DQ.head (DQ.init tester2) == 1
let t3 = DQ.head tester1 == 3
let t4 = DQ.last tester1 == 3
let t5 = DQ.is_empty (DQ.tail tester1)
let t6 = DQ.head tester3 == 2
let t7 = DQ.last tester3 == 8
let t8 = DQ.head (DQ.init tester3) == 2
let t8 = DQ.last (DQ.init tester3) == 6
let t8 = DQ.last (DQ.init (DQ.init tester3))
let t9 = DQ.last (DQ.tail (DQ.init tester3)) == 6