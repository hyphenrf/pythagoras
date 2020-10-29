(*
   pyth :: Int -> [(Int, Int, Int)]
   pyth n = [ (x, y, z)
            | x <- [1 .. n] 
            , y <- [x .. n] 
            , z <- [y .. n] 
            , x ^ 2 + y ^ 2 == z ^ 2 ]
*)

let pyth n =
    let
        s: (int*int*int) list ref = ref []
    in
    for x = 1 to n-1 do
    for y = x to n-1 do
    for z = y to n-1 do
        if x*x + y*y = z*z
        then s := !s @ [(x,y,z)]
    done done done;
    !s

(*****************************************************************************)
let flip = Fun.flip

(* NOTE that list comprehensions are functionally-equivalent to do-notation
 * which is just syntactic sugar for the monadic bind on the List container.
 * That means, we can treat lists as monads and get equivalent results *)

(* ALSO NOTE: that eager list comp is awfully ineffecient because the
 * intermediate lists have to be fully realised. Stream fusion optimisations
 * can't happen on this one, but they don't happen on ocaml clambda * anyway.
 * Maybe flmabda. *)

let pyth_monad n = let open List in
    let (let*) = flip concat_map in (* bind *)
    let (--) a b = init (b-a) ((+)a) in (* nice range syntax *)
    let (>-) cond v = if cond then [v] else [] in
    
    let* x = 1 -- n in
    let* y = x -- n in
    let* z = y -- n in
    x*x + y*y = z*z >- (x,y,z)

(*****************************************************************************)
[@@@warning "-32"]
module LazyList = struct
    type 'a node = Nil | Cons of 'a * 'a t
    and  'a t = 'a node lazy_t
    
    let empty: 'a t = lazy Nil
    let cons x xs : 'a t = lazy(Cons(x, xs))
    let return a :'a t = cons a empty

    let range a b = 
        let rec range' a b =
            if a < b then Cons(a, lazy(range' (a+1) b)) 
            else Nil 
        in
        lazy(range' a b)

    let rec foldr f (lazy t) init =
        match t with
            | Nil -> init
            | Cons(x, xs) -> f x (foldr f xs init)

    let to_list t = foldr List.cons t []
    
    let (@) l r = foldr cons l r
    let conc t = foldr (@) t empty
    let map f t = foldr (fun x xs -> cons (f x) xs) t empty

    let concat_map f t =
        map f t |> conc
    
    let (>-) guard v = if guard then return v else empty
end

let pyth_lazymonad n = let open LazyList in
  begin
    let ( let* ) = flip concat_map in
    let (--) = range in

    let* x = 1 -- n in
    let* y = x -- n in
    let* z = y -- n in
 
    x*x + y*y = z*z >- (x,y,z)
  end 
    |> to_list

include StreamF (* Stream Fusion library from the streamf gist *)
let pyth_seq n = let open StreamF in
  begin
    let ( let* ) = flatmap in

    let* x = 1 -- n in
    let* y = x -- n in
    let* z = y -- n in
 
    if x*x + y*y = z*z then return (x,y,z) else Nil
  end
    |> to_list

let pyth_iterator n = let open Iter in
  begin
    let ( let* ) = flip flat_map in

    let* x = 1 -- n in
    let* y = x -- n in
    let* z = y -- n in

    if x*x + y*y = z*z then return (x,y,z) else empty
  end
    |> to_list

let pyth_fusionstream n = let open Fusion.Stream in
  begin
    let ( let* ) = flip concat_map in
    let (--) a b = from (fun x -> if x<b then Some(x+1) else None) (Some a) in
    let empty = Stream((fun _->Done),()) in
    
    let* x = 1 -- n in
    let* y = x -- n in
    let* z = y -- n in

    if x*x + y*y = z*z then return (x,y,z) else empty
  end
    |> to_list

[@@@warning ""]
