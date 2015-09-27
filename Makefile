
STKTAG=.stack-work/install/x86_64-linux/lts-3.0/7.10.2/bin/stack-tag

.PHONY: all build test TAGS

all: test build

build:
	stack build

test:
	stack test

clean:
	stack clean

TAGS: build
	$(STKTAG)
