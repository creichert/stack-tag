
# Stack Tag

Create etags & ctags for a Haskell project and all it's dependencies
based on the stack resolver.

## Quick start

    stack install stack-tag
    stack-tag && cp stack.tag TAGS

When you change a source file, run:

    stack-tag && cp stack.tag TAGS

## Features

- Tag files are based on the current resolver. All generated tags will
  correspond to the exact matching version found in the active stack
  `resolver`.

- Tag files are cached. Dependencies only need to be downloaded once per
  resolver.

- Transitive dependencies are tagged, including dependencies in
  `executable`, `test-suite` and `benchmark` stanzas.


**_how does this compare to [codex](https://github.com/aloiscochard/codex)?_**

`stack-tag` is dead simple,

 is another Haskell source code tagging
tool designed to cache a project and all it's dependencies. Codex supports
