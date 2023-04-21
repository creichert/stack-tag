
.PHONY: all build test TAGS

all: test build

build:
	stack build --fast

test: TAGS
	@#stack test

clean:
	stack clean

TAGS: build
	stack exec -- stack-tag --verbose --no-cache
