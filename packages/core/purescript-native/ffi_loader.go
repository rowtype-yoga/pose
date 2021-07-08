package ffi_loader

// Load PureScript standard library FFI packages. Comment out the ones you don't need for
// faster/smaller builds.

import (
	_ "github.com/i-am-the-slime/go-ffi/purescript-arrays"
	_ "github.com/i-am-the-slime/go-ffi/purescript-effect"
	_ "github.com/i-am-the-slime/go-ffi/purescript-integers"
	_ "github.com/i-am-the-slime/go-ffi/purescript-partial"
	_ "github.com/i-am-the-slime/go-ffi/purescript-prelude"
	_ "github.com/purescript-native/go-ffi/purescript-assert"
	_ "github.com/purescript-native/go-ffi/purescript-console"
	_ "github.com/purescript-native/go-ffi/purescript-enums"
	_ "github.com/purescript-native/go-ffi/purescript-exceptions"
	_ "github.com/purescript-native/go-ffi/purescript-foldable-traversable"
	_ "github.com/purescript-native/go-ffi/purescript-foreign"
	_ "github.com/purescript-native/go-ffi/purescript-functions"
	_ "github.com/purescript-native/go-ffi/purescript-globals"
	_ "github.com/purescript-native/go-ffi/purescript-lazy"
	_ "github.com/purescript-native/go-ffi/purescript-math"
	_ "github.com/purescript-native/go-ffi/purescript-random"
	_ "github.com/purescript-native/go-ffi/purescript-record"
	_ "github.com/purescript-native/go-ffi/purescript-refs"
	_ "github.com/purescript-native/go-ffi/purescript-simple-json"
	_ "github.com/purescript-native/go-ffi/purescript-st"
	_ "github.com/purescript-native/go-ffi/purescript-unfoldable"
	_ "github.com/purescript-native/go-ffi/purescript-unsafe-coerce"

	// Add your own FFI packages here.
	_ "project.localhost/ffi/purescript-aff"
	_ "project.localhost/ffi/purescript-debug"
	_ "project.localhost/ffi/purescript-go-os"
	_ "project.localhost/ffi/purescript-now"
	_ "project.localhost/ffi/purescript-spec"
	_ "project.localhost/ffi/purescript-strings"
	_ "project.localhost/ffi/purescript-unicode"
)
