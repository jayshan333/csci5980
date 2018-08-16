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
                                                in match split l middle with
                                                (xs,ys) -> (rev ys,xs)
                                    |(l,[]) -> let middle = (count l)/2
                                                in match split l middle with
                                                (xs,ys) -> (xs,rev ys)
                                    | _ -> raise NOFIX

  let cons (x:'a) (q:'a queue) = match q with
                                 | (xs,ys) -> (x::xs,ys)
  
  let rec head (q:'a queue) = match q with 
                              | (x::xs,_) -> x
                              | ([],[]) -> raise EMPTY
                              | ([], y::[]) -> y
                              | ([], y::ys) -> head (fix q)
                              | _ -> raise EMPTY

  let tail (q:'a queue) = match q with
                          | (x::xs,ys) -> (xs,ys)
                          | ([],ys) -> do i fix?

  let snoc (q:'a queue) (x:'a) = match q with
                                 | (xs,ys) -> (xs,x::ys)

  let last (q:'a queue) = match q with
                          | (_,y::ys) -> y

  let init (q:'a queue) = match q with
                          | (xs,y::ys) -> (xs,ys)
end

module DQ = DequeImp

let t1 = DQ.cons 3 DQ.empty
let t2 = DQ.cons false DQ.empty