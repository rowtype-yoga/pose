package purescript_go_language_server_protocol

import (
	"fmt"
	"reflect"
	"strconv"

	"github.com/tliron/glsp"

	protocol "github.com/tliron/glsp/protocol_3_16"
	serverpkg "github.com/tliron/glsp/server"

	. "github.com/purescript-native/go-runtime"
)

func init() {
	exports := Foreign("Go.LanguageServerProtocol")

	// EffectFn3
	exports["newServerImpl"] = func(handler Any, logBaseName Any, debug Any) Any {
		return serverpkg.NewServer(handler, logBaseName, debug)
	}

	exports["newHandlerImpl"] = func(fn Any) {
		var handler = protocol.Handler
		handler.Initialize = func(context *glsp.Context, params *protocol.InitializeParams) (interface{}, error) {
			clientCapabilities = &params.Capabilities

			if params.Trace != nil {
				protocol.SetTraceValue(*params.Trace)
			}

			serverCapabilities := Handler.CreateServerCapabilities()
			serverCapabilities.TextDocumentSync = protocol.TextDocumentSyncKindIncremental
			serverCapabilities.CompletionProvider = &protocol.CompletionOptions{}

			return &protocol.InitializeResult{
				Capabilities: serverCapabilities,
				ServerInfo: &protocol.InitializeResultServerInfo{
					Name:    "puccini-language-server",
					Version: &version.GitVersion,
				},
			}, nil
		}
	}

}

func display(depth int, path string, v reflect.Value) {
	if depth > 4 {
		return
	}
	switch v.Kind() {
	case reflect.Invalid:
		fmt.Printf("%s = invalid\n", path)
	case reflect.Slice, reflect.Array:
		for i := 0; i < v.Len(); i++ {
			display(depth+1, fmt.Sprintf("%s[%d]", path, i), v.Index(i))
		}
	case reflect.Struct:
		for i := 0; i < v.NumField(); i++ {
			fieldPath := fmt.Sprintf("%s.%s", path, v.Type().Field(i).Name)
			display(depth+1, fieldPath, v.Field(i))
		}
	case reflect.Map:
		for _, key := range v.MapKeys() {
			display(depth+1, fmt.Sprintf("%s[%s]", path,
				formatAtom(key)), v.MapIndex(key))
		}
	case reflect.Ptr:
		if v.IsNil() {
			fmt.Printf("%s = nil\n", path)
		} else {
			display(depth+1, fmt.Sprintf("(*%s)", path), v.Elem())
		}
	case reflect.Interface:
		if v.IsNil() {
			fmt.Printf("%s = nil\n", path)
		} else {
			fmt.Printf("%s.type = %s\n", path, v.Elem().Type())
			display(depth+1, path+".value", v.Elem())
		}
	default: // basic types, channels, funcs
		fmt.Printf("%s = %s\n", path, formatAtom(v))
	}
}

// formatAtom formats a value without inspecting its internal structure.
func formatAtom(v reflect.Value) string {
	switch v.Kind() {
	case reflect.Invalid:
		return "invalid"
	case reflect.Int, reflect.Int8, reflect.Int16,
		reflect.Int32, reflect.Int64:
		return strconv.FormatInt(v.Int(), 10)
	case reflect.Uint, reflect.Uint8, reflect.Uint16,
		reflect.Uint32, reflect.Uint64, reflect.Uintptr:
		return strconv.FormatUint(v.Uint(), 10)
	// ...floating-point and complex cases omitted for brevity...
	case reflect.Bool:
		return strconv.FormatBool(v.Bool())
	case reflect.String:
		return strconv.Quote(v.String())
	case reflect.Chan, reflect.Func, reflect.Ptr, reflect.Slice, reflect.Map:
		return v.Type().String() + " 0x" +
			strconv.FormatUint(uint64(v.Pointer()), 16)
	default: // reflect.Array, reflect.Struct, reflect.Interface
		return v.Type().String() + " value"
	}
}
