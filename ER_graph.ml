open Primary_graph
open Random

let generator n p =
  (*Generates a Erdos-Renyi graph in G(n,p)*)
  Random.self_init ();
  let g = Graph.create n in
  for i = 0 to (n-1) do
    for j = 0 to (i-1) do
      let x = Random.float 1. in
      if (x < p) then
        Graph.add_edge g i j
    done;
  done;
  g
