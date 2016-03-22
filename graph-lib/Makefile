.PHONY: all
all: make/compiled/make_rkt.zo
	racket make/make.rkt

make/compiled/make_rkt.zo: make/make.rkt make/lib.rkt
	raco make make/make.rkt

.PHONY: clean
clean:
	rm -fr make/make docs
	find \( -path ./lib/doc/bracket -prune -and -false \) -or -name compiled -print0 | xargs -0 rm -rf

.PHONY: build-dep
build-dep:
	# datatype is only used as an example in the docs.
	raco pkg install --deps search-auto --update-deps --skip-installed alexis-util cover cover-coveralls debug datatype mischief
