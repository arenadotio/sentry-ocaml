all: build

build:
	@dune build @install @examples

clean:
	@rm -rf `find . -name 'bisect*.out'` _coverage
	@dune clean

coverage: clean
	@BISECT_ENABLE=yes dune runtest
	@bisect-ppx-report send-to Coveralls

install: build
	@dune install

test:
	@dune runtest --force

.PHONY: all build clean coverage test
