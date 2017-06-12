open Primary_graph
open Random
open FPS

let generator v =
  (*Creates a random graph following the configuration model. v is a vector
    giving giving the number of neighbours of the vertex.*)
  let g = Graph.create (Array.length v) in
  let todo = Array.copy v in
  let n = ref (Array.fold_right (fun x y -> x + y) todo 0) in
  while !n > 1 do
    let s = FPS.create
        (Array.map (fun x -> (float_of_int x /. (float_of_int !n))) todo) in
    let v1 = FPS.run s in
    decr n;
    todo.(v1) <- todo.(v1) -1;
    let s = FPS.create
        (Array.map (fun x -> (float_of_int x /. (float_of_int !n))) todo) in
    let v2 = FPS.run s in
    decr n;
    todo.(v2) <- todo.(v2) -1;
    Graph.add_edge g v1 v2
  done;
  g
