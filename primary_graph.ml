open Graphics
open Random

module type G = sig
  type t
  val create : int -> t
  val add_edge : t -> int -> int -> unit
  val edges_list : t -> (int * int) list
  val spliter : t -> float -> float -> t * t
  val printer : t -> unit
end

module Graph : G = struct
  (*Provides a non oriented graph structure by adjasence list with labels on
    vertices*)
  type t = {mutable adj : int list array ; mutable labels : int array}
  let create n : t= {adj = Array.make n [] ; labels = Array.make n 0}
  let add_edge g v1 v2 =
    if v1 <> v2 then
      begin
      g.adj.(v1) <- v2 :: g.adj.(v1);
      g.adj.(v2) <- v1 :: g.adj.(v2);
    end
    else
      g.adj.(v1) <- v2 :: g.adj.(v1)
  let edges_list g =
    let n = Array.length g.labels in
    let l = ref [] in
    for i = 0 to (n-1) do
      List.iter (fun x -> if x >=i then l := (i,x) :: !l) g.adj.(i);
    done;
    !l

  let spliter (g : t) p1 p2 =
    (*Considering a graph g, samples the edges of g independently with
      probability p1 and p2 and returns the pair of graphs obtained*)
    Random.self_init ();
    let n = Array.length (g.adj) in
    let (g1, g2) = (create n, create n) in
    let rec aux l = match l with
      | [] -> ()
      | (s, d) :: q ->
        let r1 = Random.float 1. in
        if (p1 < r1) then
          add_edge g1 s d ;
        let r2 = Random.float 1. in
        if (p2 < r2) then
          add_edge g2 s d ;
    in
    aux (edges_list g) ;
    (g1 , g2 )

  let printer (g: t) =
    let n = Array.length g.adj in
    let pos = Array.make n (0, 0) in
    Random.self_init ();
    for k = 0 to n-1 do
      let x = Random.int 600 in
      let y = Random.int 400 in
      pos.(k) <- (x,y);
    done;
    for k = 0 to n-1 do
      let rec aux l = match l with
        |[] -> ()
        |t :: q ->
          Graphics.set_color red;
          Graphics.draw_circle (fst pos.(k)) (snd pos.(k)) 4;
          Graphics.moveto (fst pos.(k)) (snd pos.(k));
          Graphics.set_color blue;
          Graphics.lineto (fst pos.(t)) (snd pos.(t));
          aux q in
      aux g.adj.(k)
    done;


end
