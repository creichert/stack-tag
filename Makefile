
.PHONY: all build test TAGS

all: test build

build:
	stack build

test:
	stack test

clean:
	stack clean

TAGS: build
	stack exec stack-tag
