open C_graph
open ER_graph
open PA_graph
open Perm
open Primary_graph
open FPS
open Graphics
open Pedarsani_Grossglauser
open Korula_Lattanzi
open Blackstrom_Dwork_Kleinberg

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
  let module ER = ER_graph(Graph) in
  let g = ER.generator 10 0.3 in
  let g1, g2 = Graph.spliter g 0.8 0.3 in
  print_int (Graph.compare g1 g2);
  print_newline ();
  let module A = PG_attack(Graph) (Perm) in
  let x = A.minimize g1 g2 in
  x ;;

print_int (fst l) ;;
print_endline ""
