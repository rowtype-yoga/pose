package purescript_spec

import (
	"os"

	. "github.com/purescript-native/go-runtime"
)

func init() {
	exports := Foreign("Test.Spec.Console")

	exports["write"] = func(s_ Any) Any {
		return func() Any {
			s := s_.(string)
			os.Stdout.WriteString(s)
			return nil
		}
	}
}
