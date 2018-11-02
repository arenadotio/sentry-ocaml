all: build

build:
	@dune build @install @examples

clean:
	@rm -rf `find . -name 'bisect*.out'` _coverage
	@dune clean

coverage: clean
	@BISECT_ENABLE=YES dune runtest
	@bisect-ppx-report -I _build/default/ -html _coverage/ \
	  `find . -name 'bisect*.out'`

install: build
	@dune install

test:
	@dune runtest --force

.PHONY: all build clean coverage test
