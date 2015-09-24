type dir = [ `Buy | `Sell ]

type t = {
  dir : dir;
  quantity : int;
  price : float;
  mutable cancelled : bool;
} [@@deriving fields]

