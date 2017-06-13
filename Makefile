main:
	ocamlbuild -libs unix,graphics main.byte

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
	git push *.ml master

clean:
	rm _build/*
	rmdir _build

realclean: clean
	rm -f *~

cleanall: realclean
