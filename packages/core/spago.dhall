{-
Welcome to a Spago project!
You can edit this file as you like.

Need help? See the following resources:
- Spago documentation: https://github.com/purescript/spago
- Dhall language tour: https://docs.dhall-lang.org/tutorials/Language-Tour.html

When creating a new Spago project, you can use
`spago init --no-comments` or `spago init -C`
to generate this file without the comments in this block.
-}
{ name = "shape"
, dependencies =
  [ "aff"
  , "argonaut-core"
  , "arrays"
  , "console"
  , "debug"
  , "effect"
  , "either"
  , "exceptions"
  , "foldable-traversable"
  , "foreign"
  , "functions"
  , "language-cst-parser"
  , "maybe"
  , "newtype"
  , "node-buffer"
  , "node-fs"
  , "node-fs-aff"
  , "node-path"
  , "nullable"
  , "prelude"
  , "psci-support"
  , "simple-json"
  , "spec"
  , "strings"
  , "transformers"
  , "tuples"
  , "uuid"
  ]
, packages = ./../../packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}