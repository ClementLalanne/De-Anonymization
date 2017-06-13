open Graphics
open Random

module type G = sig
  type t
  type label
  val create : int -> t
  val size : t -> int
  val get_label : t -> int -> label
  val set_label : t -> int -> label -> unit
  val neighbours : t -> int -> int list
  val add_edge : t -> int -> int -> unit
  val edges_list : t -> (int * int) list
  val spliter : t -> float -> float -> t * t
  val automatic_printer : t -> unit
  val assisted_printer :
    t ->
    (int * int) array ->
    Graphics.color ->
    Graphics.color ->
    unit
  val map : t -> ( (int * label) -> (int * label)) -> t
  val iter : t -> ( (int * label) -> unit ) -> unit
  val compare : t -> t -> int
end

module Graph : G = struct
  (*Provides a non oriented graph structure by adjasence list with labels on
    vertices*)
  type t = {mutable adj : int list array ; mutable labels : int array}

  type label = int

  let create n : t= {adj = Array.make n [] ; labels = Array.make n 0}

  let size g = Array.length g.labels

  let get_label g v = g.labels.(v)

  let set_label g v l = g.labels.(v) <- l

  let neighbours g v = g.adj.(v)

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
        if (r1 < p1) then
          add_edge g1 s d ;
        let r2 = Random.float 1. in
        if (r2 < p2) then
          add_edge g2 s d ;
        aux q
    in
    aux (edges_list g) ;
    (g1 , g2 )

  let automatic_printer (g: t) =
    let n = Array.length g.adj in
    let pos = Array.make n (0, 0) in
    Random.self_init ();
    for k = 0 to n-1 do
      let x = Random.int 600 in
      let y = Random.int 400 in
      pos.(k) <- (x,y);
    done;
    for k = 0 to n-1 do
      Graphics.set_color red;
      Graphics.draw_circle (fst pos.(k)) (snd pos.(k)) 4;
      let rec aux l = match l with
        |[] -> ()
        |t :: q ->
          Graphics.moveto (fst pos.(k)) (snd pos.(k));
          Graphics.set_color blue;
          Graphics.lineto (fst pos.(t)) (snd pos.(t));
          aux q in
      aux g.adj.(k)
    done

  let assisted_printer (g: t) pos c1 c2=
    let n = Array.length g.adj in
    for k = 0 to n-1 do
      Graphics.set_color c1;
      Graphics.draw_circle (fst pos.(k)) (snd pos.(k)) 4;
      let rec aux l = match l with
        |[] -> ()
        |t :: q ->
          Graphics.moveto (fst pos.(k)) (snd pos.(k));
          Graphics.set_color c2;
          Graphics.lineto (fst pos.(t)) (snd pos.(t));
          aux q in
      aux g.adj.(k)
    done

  let map g f =
    let n = size g in
    let gp = create n in
    for k = 0 to n-1 do
      let (kp, lp) = f (k, g.labels.(k)) in
      gp.labels.(kp) <- lp;
      gp.adj.(kp) <- List.map
          (fun x -> fst (f (x, g.labels.(x)))) g.adj.(k)
    done;
    gp

  let iter g f =
    let n = size g in
    for k = 0 to n-1 do
      List.iter (fun x -> f (x, g.labels.(x))) g.adj.(k)
    done

  module IS = Set.Make(struct
      type t = int
      let compare = Pervasives.compare
    end)

  let compare g1 g2 =
    (*Makes the structural diffrernce between g1 and g2*)
    let n = size g1 in
    assert (n = size g2);
    let counter = ref 0 in
    for k = 0 to n-1 do
      let s1 = ref IS.empty in
      let s2 = ref IS.empty in
      List.iter (fun x -> s1 := IS.add x !s1) g1.adj.(k);
      List.iter (fun x -> s2 := IS.add x !s2) g1.adj.(k);
      counter :=
        !counter +
        (IS.cardinal (IS.diff !s1 !s2)) +
        (IS.cardinal (IS.diff !s2 !s1))
    done;
    !counter


end
