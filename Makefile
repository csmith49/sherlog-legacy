# building the executable
sherlog: lib bin/sherlog.ml
	@dune build bin/sherlog.exe
	@mv _build/default/bin/sherlog.exe sherlog

# for entering interactive mode
.PHONY: live
live: lib
	@dune utop lib

# for cleaning
.PHONY: clean
clean:
	@dune clean
	@rm -rf _build sherlog