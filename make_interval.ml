open Comparable;;
open Ordering;;

module type Interval_intf = sig
  type t;;
  type endpoint;;

  val create: endpoint -> endpoint -> t;;
  val is_empty: t -> bool;;
  val contains: t -> endpoint -> bool;;
  val intersect: t -> t -> t;;
end
;;


module Make_interval(Endpoint: Comparable) : Interval_intf = struct
  type t =
    | Interval of Endpoint.t * Endpoint.t
    | Empty
  ;;

  type endpoint = Endpoint.t;;

  type ord = Ordering.ord = | LT | EQ | GT;;

  let create low high =
    match Endpoint.compare low high with
    | GT -> Empty
    | _  -> Interval (low, high)
  ;;

  let is_empty = function
    | Empty      -> true
    | Interval _ -> false
  ;;

  let contains t x =
    match t with
    | Empty           -> false
    | Interval (l, h) ->
       match Endpoint.compare x l, Endpoint.compare x h with
       | GT, LT | EQ, LT | LT, EQ | EQ, EQ -> true
       | _ -> false
  ;;

  let intersect t1 t2 =
    let min x y =
      match Endpoint.compare x y with
      | LT | EQ -> x
      | _ -> y in
    let max x y = if min x y = x then y else x in
    match t1, t2 with
    | Empty, _ | _, Empty -> Empty
    | Interval (l1, h1), Interval (l2, h2) ->
      create (max l1 l2) (min h1 h2)
end
;;

module Int_interval =
  Make_interval(
      struct
        type t = int;;

        let compare n m =
          let module O = Ordering in
          if n > m then O.GT
          else if n < m then O.LT
          else O.EQ
      end
    )
;;

module Comparable_String = struct
  type t = String;;

  let compare s1 s2 =
    let module O = Ordering in
    if s1 > s2 then O.GT
    else if s1 < s2 then O.LT
    else O.EQ
end;;

module String_interval = Make_interval(Comparable_String);;
