main:
	ocamlc unix.cma graphics.cma FPS.ml perm.ml primary_graph.ml C_graph.ml PA_graph.ml ER_graph.ml main.ml

clean:
	rm *.cmi *.cmo a.out

realclean: clean
	rm -f *~

cleanall: realclean
