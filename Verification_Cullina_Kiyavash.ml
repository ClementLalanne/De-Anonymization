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

module VCK(Graph : G) (Seed: S)= struct
  (*Provides a way to test if to tesults of Daniel Cullina and Negar Kiyavash
    in the paper "Improved Achievability and converse bounds for Erdos-Rényi
    Graph Matching"still work with the percolation algorithm of Korula and
    Lattanzi*)
  let run correlation density n_max l =
    let module ER = ER_graph(Graph) in
    let module A = KL_attack(Graph) (Seed) in
    let oc = open_out ("VCK/" ^
                       string_of_float density ^ "_" ^
                       string_of_float correlation ^ "_" ^
                       string_of_int n_max ^ "_" ^
                      string_of_float l) in
    for k = 10 to n_max do
      let p =  24. *. (Pervasives.log (float_of_int k) +. 1.) /.
               (correlation *. correlation *. l *. (float_of_int (k-2)) *.
                Pervasives.log (2.)) in
      let g = ER.generator k p in
      let g1, g2 = Graph.spliter g correlation correlation in
      Random.self_init ();
      let s = ref (Seed.empty ()) in
      for k = 0 to (k-1) do
        let r = Random.float 1. in
        if r < 0.2 then
          begin
            s := Seed.add !s k k
          end
      done;
      let sp = A.attack g1 g2 !s 3 (k*k) in
      let counter = ref 0 in
      for j = 0 to k-1 do
        if Seed.is_matched sp j then
          if j = (Seed.int_of_return (Seed.assoc sp j)) then
            incr counter
      done;
      let res = (float_of_int !counter) /. (float_of_int k) in
      Printf.fprintf oc "%f\n" res;
      Pervasives.flush oc
    done;
    close_out oc

end

let () =
  let module M = VCK(Graph) (Seed) in
  M.run 0.9 300. 300 0.2;
  ()
