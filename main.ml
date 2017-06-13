open C_graph
open ER_graph
open PA_graph
open Perm
open Primary_graph
open FPS
open Graphics

let positions n =
  let v = Array.make n (0, 0) in
  Random.self_init ();
  for k = 0 to n-1 do
    let x = Random.int 600 in
    let y = Random.int 400 in
    v.(k) <- (x,y)
  done;
  v


let l =
  Graphics.open_graph " 600x400";
  let module ER = ER_graph(Graph) in
  let g = ER.generator 30 0.1 in
  let g1, g2 = Graph.spliter g 0.5 0.3 in
  let p = positions 100 in
  Graph.assisted_printer g1 p Graphics.red Graphics.blue;
  Unix.sleep 10;
  Graph.assisted_printer g2 p Graphics.red Graphics.green;
  Unix.sleep 10
