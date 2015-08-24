
# Stack Tag

Create ctags/etags for a Haskell project based on stack.yaml

	cd path/to/project
    stack-tag

## Features

- Tag files are based on snapshots. All generated tags will correspond
  to the exact matching version found in the active `resolver`

- Tag files are cached. Dependencies only need to be downloaded once
  per snapshot.

- Transitive dependencies are tagged, including dependencies in
  `executable`, `test-suite` and `benchmark` stanzas.

## WIP

Still a work-in-progress. Currently, the only supported tagging format
is ETags.

## TODO

- Support ctags and other tagging formats
- Handle `extra-deps` dependencies
- Handle `-installed` dependencies
- Cleanup logging
- Any way to tag operators?
- Tag only exposed-modules
