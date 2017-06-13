main:
	ocamlbuild -libs unix,graphics main.byte

clean:
	rm _build/*
	rmdir _build

realclean: clean
	rm -f *~

cleanall: realclean
