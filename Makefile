main:
	ocamlc unix.cma graphics.cma FPS.ml perm.ml primary_graph.ml C_graph.ml PA_graph.ml ER_graph.ml main.ml

git:
	git status
	git add C_graph.ml
	git add ER_graph.ml
	git add FPS.ml
	git add main.ml
	git add Makefile
	git add PA_graph.ml
	git add perm.ml
	git add primary_graph.ml
	git add README
	git commit -m "bug fixed"
	git remote push --all master

clean:
	rm *.cmi *.cmo a.out

realclean: clean
	rm -f *~

cleanall: realclean
