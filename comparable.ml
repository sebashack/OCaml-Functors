open Ordering;;

module type Comparable = sig
  type t;;
  val compare : t -> t -> Ordering.ord;;
end
;;
