open Primary_graph
open Random

let generator v =
  (*Creates a random graph following the configuration model. v is a vector
    giving giving the number of neighbours of the vertex.*)
  let g = Graph.create (Array.length v) in
  g
