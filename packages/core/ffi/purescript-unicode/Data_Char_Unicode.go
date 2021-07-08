package purescript_unicode

import (
	. "github.com/purescript-native/go-runtime"
)

func init() {
	exports := Foreign("Data.Char.Unicode")

	exports["withCharCode"] = func(f Any) Any {
		return func(c_ Any) Any {
			c := c_.(rune)
			return Apply(f, c).(string)
		}
	}
}
