{-
Welcome to your new Dhall package-set!

Below are instructions for how to edit this file for most use
cases, so that you don't need to know Dhall to use it.

## Use Cases

Most will want to do one or both of these options:
1. Override/Patch a package's dependency
2. Add a package not already in the default package set

This file will continue to work whether you use one or both options.
Instructions for each option are explained below.

### Overriding/Patching a package

Purpose:
- Change a package's dependency to a newer/older release than the
    default package set's release
- Use your own modified version of some dependency that may
    include new API, changed API, removed API by
    using your custom git repo of the library rather than
    the package set's repo

Syntax:
where `entityName` is one of the following:
- dependencies
- repo
- version
-------------------------------
let upstream = --
in  upstream
  with packageName.entityName = "new value"
-------------------------------

Example:
-------------------------------
let upstream = --
in  upstream
  with halogen.version = "master"
  with halogen.repo = "https://example.com/path/to/git/repo.git"

  with halogen-vdom.version = "v4.0.0"
  with halogen-vdom.dependencies = [ "extra-dependency" ] # halogen-vdom.dependencies
-------------------------------

### Additions

Purpose:
- Add packages that aren't already included in the default package set

Syntax:
where `<version>` is:
- a tag (i.e. "v4.0.0")
- a branch (i.e. "master")
- commit hash (i.e. "701f3e44aafb1a6459281714858fadf2c4c2a977")
-------------------------------
let upstream = --
in  upstream
  with new-package-name =
    { dependencies =
       [ "dependency1"
       , "dependency2"
       ]
    , repo =
       "https://example.com/path/to/git/repo.git"
    , version =
        "<version>"
    }
-------------------------------

Example:
-------------------------------
let upstream = --
in  upstream
  with benchotron =
      { dependencies =
          [ "arrays"
          , "exists"
          , "profunctor"
          , "strings"
          , "quickcheck"
          , "lcg"
          , "transformers"
          , "foldable-traversable"
          , "exceptions"
          , "node-fs"
          , "node-buffer"
          , "node-readline"
          , "datetime"
          , "now"
          ]
      , repo =
          "https://github.com/hdgarrood/purescript-benchotron.git"
      , version =
          "v7.0.0"
      }
-------------------------------
    -- with ry-blocks =
  --   { dependencies =
  --     [ "aff"
  --     , "aff-promise"
  --     , "arrays"
  --     , "colors"
  --     , "console"
  --     , "control"
  --     , "datetime"
  --     , "debug"
  --     , "effect"
  --     , "either"
  --     , "enums"
  --     , "exceptions"
  --     , "foldable-traversable"
  --     , "foreign"
  --     , "foreign-generic"
  --     , "foreign-object"
  --     , "framer-motion"
  --     , "free"
  --     , "functions"
  --     , "heterogeneous"
  --     , "integers"
  --     , "interpolate"
  --     , "lists"
  --     , "literals"
  --     , "math"
  --     , "maybe"
  --     , "newtype"
  --     , "nullable"
  --     , "numbers"
  --     , "ordered-collections"
  --     , "partial"
  --     , "prelude"
  --     , "profunctor-lenses"
  --     , "psci-support"
  --     , "react-basic"
  --     , "react-basic-dom"
  --     , "react-basic-emotion"
  --     , "react-basic-hooks"
  --     , "react-testing-library"
  --     , "record"
  --     , "routing"
  --     , "routing-duplex"
  --     , "spec"
  --     , "spec-discovery"
  --     , "strings"
  --     , "tailrec"
  --     , "transformers"
  --     , "tuples"
  --     , "two-or-more"
  --     , "typelevel-peano"
  --     , "typelevel-prelude"
  --     , "unsafe-coerce"
  --     , "untagged-union"
  --     , "web-dom"
  --     , "web-events"
  --     , "web-html"
  --     , "web-uievents"
  --     ]
  --   , repo = "https://github.com/rowtype-yoga/ry-blocks.git"
  --   , version = "d26c8cfd579d7f7f602af2d0b614f87956a61acb"
  --   }

-}
let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.14.2-20210629/packages.dhall
        sha256:534c490bb73cae75adb5a39871142fd8db5c2d74c90509797a80b8bb0d5c3f7b

in  upstream
  with pose-core = ./packages/core/spago.dhall as Location
  with language-cst-parser =
    { dependencies =
      [ "arrays"
      , "console"
      , "const"
      , "debug"
      , "effect"
      , "either"
      , "filterable"
      , "foldable-traversable"
      , "free"
      , "functors"
      , "maybe"
      , "numbers"
      , "psci-support"
      , "strings"
      , "transformers"
      , "tuples"
      , "typelevel-prelude"
      ]
    , repo =
        "https://github.com/i-am-the-slime/purescript-language-cst-parser.git"
    , version = "range-fix"
    }
  with plumage =
    { dependencies =
      [ "arrays"
      , "console"
      , "effect"
      , "either"
      , "foldable-traversable"
      , "foreign"
      , "foreign-object"
      , "justifill"
      , "lists"
      , "maybe"
      , "newtype"
      , "nullable"
      , "orders"
      , "prelude"
      , "psci-support"
      , "react-basic"
      , "react-basic-dom"
      , "react-basic-hooks"
      , "record"
      , "transformers"
      , "tuples"
      , "typelevel-prelude"
      , "unsafe-coerce"
      ]
    , repo = "https://github.com/murmuras-tech/plumage.git"
    , version = "7f91c5725be5a4d8ffc78d9ea34319b50c1d952c"
    }
  with ry-blocks =
    { dependencies =
      [ "aff"
      , "aff-promise"
      , "arrays"
      , "colors"
      , "console"
      , "control"
      , "datetime"
      , "debug"
      , "effect"
      , "either"
      , "enums"
      , "exceptions"
      , "foldable-traversable"
      , "foreign"
      , "foreign-generic"
      , "foreign-object"
      , "framer-motion"
      , "free"
      , "functions"
      , "heterogeneous"
      , "integers"
      , "interpolate"
      , "lists"
      , "literals"
      , "math"
      , "maybe"
      , "newtype"
      , "nullable"
      , "numbers"
      , "ordered-collections"
      , "partial"
      , "prelude"
      , "profunctor-lenses"
      , "psci-support"
      , "react-basic"
      , "react-basic-dom"
      , "react-basic-emotion"
      , "react-basic-hooks"
      , "react-testing-library"
      , "record"
      , "routing"
      , "routing-duplex"
      , "spec"
      , "spec-discovery"
      , "strings"
      , "tailrec"
      , "transformers"
      , "tuples"
      , "two-or-more"
      , "typelevel-peano"
      , "typelevel-prelude"
      , "unsafe-coerce"
      , "untagged-union"
      , "web-dom"
      , "web-events"
      , "web-html"
      , "web-uievents"
      ]
    , repo = "https://github.com/rowtype-yoga/ry-blocks.git"
    , version = "3f732682f6c22119e1d3c0611264366ed512a575"
    }
