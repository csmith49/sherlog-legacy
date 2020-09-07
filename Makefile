# building the executables
sherlog: lib bin/sherlog.ml
	@dune build bin/sherlog.exe
	@mv _build/default/bin/sherlog.exe sherlog

server: lib bin/server.ml
	@dune build bin/server.exe
	@mv _build/default/bin/server.exe server

# for entering interactive mode
.PHONY: live
live: lib
	@dune utop lib

# for cleaning
.PHONY: clean
clean:
	@dune clean
	@rm -rf _build sherlog