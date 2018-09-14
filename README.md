
# Stack Tag

Create etags & ctags for Haskell projects based on Stack snapshots.

## Quick start

    stack install stack-tag
    stack-tag && cp stack.tag TAGS

When you change a source file, run:

    stack-tag && cp stack.tag TAGS

## Features

- Tag files are based on snapshots. All generated tags will correspond to
  the exact matching version found in the active stack `resolver`.

- Tag files are cached. Dependencies only need to be downloaded once per
  snapshot.

- Transitive dependencies are tagged, including dependencies in
  `executable`, `test-suite` and `benchmark` stanzas.

## WIP

Still a work-in-progress. Currently, the only supported tagging format
is ETags and the only support tag executable is Hasktags.

On large projects, there is a significant amount of collision on
common names, such as `insert`, `map`, etc.

## TODO

- Support ctags and other tagging formats
- Handle `extra-deps` dependencies
- Handle `-installed` dependencies
- Cleanup logging
- Any way to tag operators?
- Tag only exposed-modules
