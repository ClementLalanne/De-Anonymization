open Primary_graph
open Seed

module KL_attack(Graph: G) (Seed: S) = struct
  (*Attack based on the paper "An efficient reconciliation algorithm for social
    networks" of Nitish Korula and Silvio Lattanzi. This attack needs a seed
    and is based on the notion of similarity witnesses.*)
  let similarity_witnesses g1 g2 s v1 v2 =
    let n1 = Graph.neighbours g1 v1 in
    let n2 = Graph.neighbours g2 v2 in
    let counter = ref 0 in
    let f i =
      if Seed.is_matched s i then
        if List.mem (Seed.int_of_return (Seed.assoc s i)) n2 then
          incr counter
    in
    List.iter f n1;
    !counter

  let attack g1 g2 si t k =
    let s = ref si in
    let dm = ref 1 in
    Graph.iter
      (fun (x, y) -> dm := max !dm (Graph.degree g1 x)) g1;
      Graph.iter
        (fun (x, y) -> dm := max !dm (Graph.degree g2 x)) g2;
    let log2 x = (Pervasives.log x) /. (Pervasives.log 2.) in
    let m = int_of_float (log2 (float_of_int !dm)) in
    for i = 1 to k do
      for j = m downto 1 do

            let threshold = 1 lsl j-1 in
            let sw = ref t in
            let l = ref [] in
            for v1 = 0 to (Graph.size g1)-1 do
              for v2 = (Graph.size g2)-1 downto 0 do
                if (not (Seed.is_matched !s v1)) &&
                   (not (Seed.is_image !s v2)) then
                begin
                if (min (List.length (Graph.neighbours g1 v1))
                  (List.length (Graph.neighbours g2 v2))) >= threshold then
                  let n = similarity_witnesses g1 g2 !s v1 v2 in
                  if n >= !sw then
                    begin
                      sw := n;
                      l := (v1,v2) :: !l
                    end
          end
          done
        done;
        if !l <> [] then
          begin
            let (u,v) = List.hd !l in
            s := Seed.add !s u v;
          end
      done
    done;
    !s


end
