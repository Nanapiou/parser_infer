build:
	dune build

utop:
	dune utop src

run:
	dune exec main

.PHONY: test

clean:
	dune clean
