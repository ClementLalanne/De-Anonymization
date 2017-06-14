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
open Seed

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
  let n = 200 in
  let g = ER.generator n 0.2 in
  let g1, g2 = Graph.spliter g 0.5 0.5 in
  let module A = KL_attack(Graph) (Seed) in
  Random.self_init ();
  let s = ref (Seed.empty ())in
  for k = 0 to (n)-1 do
    let r = Random.float 1. in
    if r < 0.2 then
      begin
        s := Seed.add !s k k
      end
  done;
  s := A.attack g1 g2 !s 3 n;
  Seed.iter (fun i j -> print_int i ;
              print_string" -> " ;
              print_int j;
              print_newline ()) !s ;
  ()
