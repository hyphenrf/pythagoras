[@@@ warning "-27-32" ]
module StreamF = struct
    (*This barely beats the Seq module in Batteries, beats Core.LazyList 
    by a lot and is 10x faster than a standard List *)
    
    type 'a stream =
      | Nil
      | Cons of 'a * (unit -> 'a stream)
	
    type 'a t = 'a stream
    
    let force x = x ()

    (*Lazy*)		   
    let rec map f s =
      match s with
      | Nil -> Nil
      | Cons(hd,tl) -> Cons(f hd, fun () -> map f (force tl))

    (*Lazy*)
    let rec map2 f s1 s2 =
      match s1,s2 with
      | Nil, Nil -> Nil
      | Cons(h1,t1), Cons(h2,t2) -> Cons(f h1 h2, fun () -> map2 f (force t1) (force t2))
      | _ -> failwith "index"

    let rec from n =
      Cons(n, fun () -> from (n + 1))

    let nats = from 0

    let rec from_to l h =
      if l <= h then
	Cons(l, fun () -> from_to (l + 1) h)
      else
	Nil

    let (--) = from_to

    (*Lazy!*)	 
    let rec take n s =
      match n with
      | 0 -> Nil
      | _ -> begin match s with
		   | Nil -> failwith "Length"
		   | Cons(hd,tl) -> Cons (hd, fun () -> take (n - 1) (force tl))
	     end

    let rec drop n s =
      match n with
      | 0 -> s
      | _ -> begin match s with
		   | Nil -> failwith "Length"
		   | Cons(hd,tl) -> drop (n - 1) (force tl)
	     end
	       
					 
    (*Super duper eager*)      
    let rec fold_left f acc s =
      match s with
      |Nil -> acc
      |Cons(hd,tl) -> fold_left f (f acc hd) (force tl)

    (*Eager af*)
    let to_list s =
      let rec loop x acc =
	match x with
	| Cons(hd,tl) -> loop (force tl) (hd::acc)
	| Nil -> List.rev acc
      in loop s []

    (*Evals up to point*)
    let rec item i s =
      match s,i with
      |Cons(hd,tl), 0 -> hd
      |Cons(hd,tl), n when n > 0 -> item (n-1) (force tl)
      |_ -> failwith "IndexOutOfBounds"

    (*Eager*)
    let take_to_list i s =
      let rec loop l index acc =
	match l with
	| Cons(hd,tl) when index < i -> loop (force tl) (index + 1) (hd::acc)
	| _ when index = i -> List.rev acc
	| _ -> []
      in loop s 0 []
	      
    let rec filter f s =
      match s with
      |Cons(hd,tl) when f hd -> Cons (hd, fun () -> filter f (force tl))
      |Cons(hd,tl) -> filter f (force tl)
      |Nil -> Nil

    let rec seq init step cond =
      if cond init then
	Cons(init, fun () -> seq (step init) step cond) 
      else
	Nil

    let rec int_seq l h by =
      if l < h then
	Cons(l, fun () -> int_seq (l + by) h by)
      else
	Nil

    (*Eager*)
    let length s =
      let rec loop str acc =
	match str with
	| Nil -> acc
	| Cons(hd,tl) -> loop (force tl) (acc + 1)
      in loop s 0

    let rec iter f stream =
      match stream with 
      | Nil -> ()
      | Cons(hd,tl) -> let () = f hd in
		       iter f (force tl)

    let iteri f stream =
      let rec loop s i =
	match s with
	| Nil -> ()
	| Cons(hd,tl) -> let () = f i hd in 
			 loop (force tl) (i + 1)
      in loop stream 0
	      
    let to_list stream =
      let rec loop str acc =
	match str with
	| Cons(hd,tl) -> loop (force tl) (hd::acc)
	| Nil -> List.rev acc
      in loop stream []

    let rec of_list = function
      | hd::tl -> Cons(hd, fun () -> of_list tl)
      | [] -> Nil

    let rec item i s = (*make this better *)
      match s,i with
      | Cons(hd,tl), 0 -> hd
      | Cons(hd,tl), n when n > 0 -> item (n - 1) (force tl)
      | _ -> failwith "Outofbounds"

    let rec append s1 s2 =
      match s1 with
      | Nil -> s2
      | Cons(hd,tl) -> Cons(hd, fun () -> append (force tl) s2)

    let (@) = append
		
    let rec flatten = function
      | Nil -> Nil
      | Cons(hd,tl) -> hd @ (flatten (force tl))

    let concat = flatten
		   
    let bind x f = map f x |> concat

    let flatmap = bind

    let (>>=) = bind

    let return x = Cons(x, fun () -> Nil)

    let init n f =
      let rec loop i =
	if i = n then
	  Nil
	else
	  Cons(f i, fun () -> loop (i + 1))
      in loop 0
  end

[@@@ warning ""]
