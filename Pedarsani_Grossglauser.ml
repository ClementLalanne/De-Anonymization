open Perm
open Primary_graph

module PG_attack(Graph: G) (Perm: P) = struct
  (*Attack based on the paper "On Privacy of Anonymized Networks" of Pedram
    Pedrasani and Matthias Grossglauser based on the minimization of objective
    function which is the structural difference between tho graphs on the same
    set of vertices.*)
  let minimize g1 g2 =
    let n = Graph.size g1 in
    assert (n = Graph.size g2);
    let p = ref (Perm.identity n) in
    let x = Graph.map (fun (x, y) -> (Perm.applique !p x, y)) g1 in
    let m = ref (Graph.compare x g2) in
    let l = ref [!p] in
    let max = Perm.max n in
    while not (Perm.equal max !p) do
      p := Perm.next !p;
      let x = Graph.map (fun (x, y) -> (Perm.applique !p x, y)) g1 in
      let mp = Graph.compare x g2 in
      if (mp = !m) then
        l := !p :: !l
      else
        begin
          if (mp < !m) then
            begin
              m := mp;
              l := [!p]
            end
        end
    done;
    (!m, !l)

end
