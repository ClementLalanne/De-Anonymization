main:
	ocamlbuild -libs unix,graphics main.byte

test:
	ocamlbuild -libs unix,graphics Verification_Cullina_Kiyavash.native

clean:
	rm _build/*
	rmdir _build

cleantests:
	rm VCK/*
	rm _build/*
	rmdir _build

realclean: clean
	rm -f *~

cleanall: realclean
