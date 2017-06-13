open Primary_graph
open Random
open FPS

module PA_graph(Graph : G)= struct
  let generator m n =
  (*Creates a graph with n vertices following the preferncial attachment
    construction path. m is the degree of every node at the moment where they
    are added*)
    let g = Graph.create n in
    let degrees = Array.make n 0 in
    let sumd = ref 0 in
    for k = 0 to m-1 do
      Graph.add_edge g 0 0;
      degrees.(k) <- degrees.(k) + 1;
      incr sumd
    done;
    for i = 1 to n-1 do
      for j = 0 to m-1 do
        let dp = Array.copy degrees in
        dp.(i) <- dp.(i) + 1;
        let p = Array.map
            (fun x -> (float_of_int x) /. (float_of_int !sumd)) dp in
        let ep = FPS.create p in
        let v = FPS.run ep in
        incr sumd;
        Graph.add_edge g i v;
      done;
    done;
    g

end
