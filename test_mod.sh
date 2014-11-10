#!/bin/bash

ocamlbuild -clean
rm *.native
ocamlbuild ./test_mod.native -use-ocamlfind -pkgs batteries,pcre
if [ -f ./test_mod.native ]
then
	./test_mod.native
else
	echo "did not produce test_mod.native"
fi
