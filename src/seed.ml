module type S = sig
  type t
  type return
  val is_matched : t -> int -> bool
  val is_image : t -> int -> bool
  val assoc : t -> int -> return
  val int_of_return : return -> int
  val add : t -> int -> int -> t
  val empty : unit -> t
  val iter : (int -> int -> unit) -> t -> unit
end

module Seed: S = struct
  (*Provides an implementation of a seed*)
  module M = Map.Make(struct
      type t = int
      let compare = Pervasives.compare
    end)

  type t = int M.t

  type return =
    |None
    |Some of int

  let is_matched s k = M.mem k s

  let is_image s k = M.exists (fun x y -> y = k) s

  let assoc s k =
    try
      Some(M.find k s)
    with
      Not_found -> None

  let int_of_return r =
    match r with
      |None -> -1
      |Some(i) -> i

  let add s i v = M.add i v s

  let empty () = M.empty

  let iter f s = M.iter f s
end
