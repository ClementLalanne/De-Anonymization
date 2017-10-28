open Random

module type FPS = sig
  type t
  val create : float array -> t
  val run : t -> int
end

module FPS = struct
  (*Gives an implementation of a finite probalistic space using the cumulative
    function and gives the possibility (function run) to have independent issues
    of a racdom variable that takes her values in [0 .. n-1] under this
    probability.*)
  type t = float array

  let create t =
    Random.self_init ();
    let tp = Array.make (Array.length t) 0. in
    let sum = ref 0. in
    for k = 0 to (Array.length t) -1 do
      sum := !sum +. t.(k);
      tp.(k) <- !sum
    done;
    tp.((Array.length t) -1) <- 1.;
    tp

  let run p =
    let r = Random.float 1. in
    let k = ref 0 in
    while p.(!k) < r do
      incr k
    done;
    !k

end
