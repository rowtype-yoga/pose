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
{ name = "pose"
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
  , "foreign-generic"
  , "functions"
  , "language-cst-parser"
  , "maybe"
  , "newtype"
  , "node-buffer"
  , "node-fs"
  , "node-fs-aff"
  , "node-path"
  , "nullable"
  , "partial"
  , "prelude"
  , "psci-support"
  , "simple-json"
  , "spec"
  , "strings"
  , "transformers"
  , "tuples"
  , "unsafe-coerce"
  , "uuid"
  ]
, packages = ./../../packages.dhall
, sources = [ "src/**/*.purs" ]
}
