
# Stack Tag

Create etags for a Haskell project and all it's dependencies based on the
stack resolver.

## Quick start

Create a new `TAGS` file:

    $ stack install stack-tag
    $ stack-tag

Update the `TAGS` file:

    $ stack-tag

## Features

- **accurate**

    Tag files are based on the current resolver. All generated tags will
    correspond to the exact matching version found in the active stack
    `resolver`.

- **cached**

    Tag files are cached. Dependencies only need to be downloaded and
    tagged once.

- **transitive dependencies**

    Transitive dependencies are tagged, including dependencies in
    `executable`, `test-suite` and `benchmark` stanzas.


**_how does this compare to [codex](https://github.com/aloiscochard/codex)?_**

Codex supports different project types and has more configuration options.

`stack-tag` only works w/ projects that have a `stack.yaml` but is very
easy to use.
