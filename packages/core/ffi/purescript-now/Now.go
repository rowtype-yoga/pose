package purescript_now

import (
	"time"

	. "github.com/purescript-native/go-runtime"
)

func init() {
	exports := Foreign("Effect.Now")

	exports["now"] = func() Any {
		return float64(time.Now().Unix())
	}

}
