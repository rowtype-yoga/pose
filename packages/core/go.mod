module project.localhost/example

go 1.16

replace project.localhost/purescript-native/ffi-loader => ./purescript-native

replace project.localhost/purescript-native/output => ./output

replace project.localhost/ffi => ./ffi

require (
	github.com/dlclark/regexp2 v1.4.0 // indirect
	github.com/i-am-the-slime/go-ffi v0.0.0-20210708182442-8ad06bcde03a // indirect
	github.com/purescript-native/go-runtime v0.1.2
	project.localhost/ffi v0.1.0 // indirect
	project.localhost/purescript-native/ffi-loader v0.0.0-00010101000000-000000000000 // indirect
	project.localhost/purescript-native/output v0.0.0-00010101000000-000000000000 // indirect
)
