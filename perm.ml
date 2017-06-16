open Primary_graph

module type P = sig
  type t
  val applique : t -> int -> int
  val compose : t -> t -> t
  val inverse : t -> t
  val next : t -> t
  val identity : int -> t
  val max : int -> t
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val of_array : int array -> t
  val to_array : t -> int array
end

module Perm: P = struct
  (*Provides a basic implementation of permutations using an array and gives a
    brunch of basic functions on this data structure. We use the lexicographical
    order on the permutations to code the "next" function.*)
  type t = int array

  let applique p i = p.(i)

  let compose p1 p2 =
    assert (Array.length p1 = Array.length p2) ;
    let p = Array.make (Array.length p1) 0 in
    for k = 0 to (Array.length p1) - 1 do
      p.(k) <- p1.(p2.(k))
    done;
    p

  let inverse p =
    let n = Array.length p in
    let i = Array.make n 0 in
    for k = 0 to n-1 do
      i.(p.(k)) <- k
    done;
    i

  let identity n =
    let i = Array.make (n) 0 in
    for k = 0 to (n-1) do
      i.(k) <- k
    done;
    i

  let max n =
    let i = Array.make n 0 in
    for k = 0 to n-1 do
      i.(k) <- n-1-k
    done;
    i

  let next p =
    let n = Array.length p in
    let s = Array.make n p.(0) in
    assert (n > 0);
    let rec reverse t i j =
      if j-i > 0 then
        begin
        let x = t.(i) in
        t.(i) <- t.(j);
        t.(j) <- x;
        reverse t (i+1) (j-1);
      end
    in
    let j = ref 0 in
    for k = 0 to (n-2) do
      s.(k) <- p.(k);
      s.(k+1) <- p.(k+1);
      if p.(k) < p.(k+1) then
        j := k
    done;
    let k = ref !j in
    for i = (!j+1) to (n-1) do
      if p.(!j) < p.(i) then
        k := i;
    done;
    let x = s.(!k) in
    s.(!k) <- s.(!j);
    s.(!j) <- x;
    reverse s (!j+1) (n-1);
    s

  let equal p1 p2 =
    if (Array.length p1) = (Array.length p2) then
      begin
        let b = ref true in
        for k = 0 to (Array.length p1)-1 do
          b := !b && (p1.(k)=p2.(k));
        done;
        !b;
      end
    else
      false

  let compare p1 p2 =
    let n = Array.length p1 in
    assert (n = (Array.length p2));
    let c = ref 0 in
    for k = 0 to n-1 do
      if p1.(k) <> p2.(k) then
        incr c;
    done;
    !c

  let of_array t = Array.copy t

  let to_array p = Array.copy p

end

module Random_perm(Perm : P) = struct

  open Random

  let generate n =
    let a = Array.init n (fun x -> x) in
    Random.self_init ();
    let swap t i j =
      let x = t.(i) in
      t.(i) <- t.(j);
      t.(j) <- x
    in
    for j = n downto 1 do
      let k = Random.int j in
      swap a k (j-1)
    done;
    Perm.of_array a

end

module Perm_on_graph(Graph : G) (Perm : P) = struct
  let applique g p =
    let n = Graph.size g in
    let gp = Graph.create n in
    for k = 0 to n-1 do
      let l = Graph.neighbours g k in
      let kp = Perm.applique p k in
      let lp = List.filter (fun x -> x >= kp)
          (List.map (Perm.applique p) l) in
      List.iter (Graph.add_edge gp kp) lp
    done;
    gp

end
