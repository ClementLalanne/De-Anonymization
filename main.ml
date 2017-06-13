open C_graph
open ER_graph
open PA_graph
open Perm
open Primary_graph
open FPS

let () =
  Graphics.open_graph " 600x400";
  let module PA = PA_graph(Graph) in
  let g = PA.generator 3 100 in
  Graph.printer g;
  Unix.sleep 20;
