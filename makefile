release:
	dune build --profile release maze.bc.js

debug:
	dune build maze.bc.js

clean:
	dune clean
