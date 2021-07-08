package purescript_spec

import (
	"os"

	. "github.com/purescript-native/go-runtime"
)

func init() {
	exports := Foreign("Test.Spec.Runner")

	exports["exit"] = func(code_ Any) Any {
		return func() Any {
			code := code_.(int)
			os.Exit(code)
			return nil
		}
	}
}
