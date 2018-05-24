module Queue = sig
	type 'a queue

	val empty : 'a queue
	val isEmpty : 'a queue -> bool

	val snoc: 'a queue -> 'a -> 'a queue
	val head: 'a queue -> 'a
	val tail: 'a queue -> 'a queue 
end

module type Deque = sig
  type 'a queue

  val empty : 'a queue
  val is_empty : 'a queue -> bool

  val cons : 'a -> 'a queue -> 'a queue
  val head : 'a queue -> 'a
  val tail : 'a queue -> 'a queue

  val snoc : 'a queue -> 'a -> 'a queue
  val last : 'a queue -> 'a
  val init : 'a queue -> 'a queue
end