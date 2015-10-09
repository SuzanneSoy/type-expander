.PHONY: all
all: make/make
	make/make

make/make: make/make.rkt make/lib.rkt
	raco exe make/make.rkt

.PHONY: clean
clean:
	rm -fr make/make docs coverage
	find \( -path ./lib/doc/bracket -prune -and -false \) -or -name compiled -print0 | xargs -0 rm -rf

.PHONY: build-dep
build-dep:
	raco pkg install --deps search-auto --update-deps --skip-installed alexis-util cover
