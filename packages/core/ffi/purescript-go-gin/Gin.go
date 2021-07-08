package purescript_go_gin

import (
	"net/http"

	"github.com/gin-gonic/gin"
	"github.com/gin-gonic/gin/binding"
	. "github.com/purescript-native/go-runtime"
)

func init() {
	exports := Foreign("Gin")

	exports["mkDefaultGin"] = func() Any {
		return gin.Default()
	}

	exports["mkHandler"] =
		func(handler_ Any) Any {
			return func(c *gin.Context) {
				Apply(handler_, c)
			}
		}

	exports["staticImpl"] = func(r_ Any) Any {
		return func(route_ Any) Any {
			return func(path_ Any) Any {
				return func() Any {
					route := route_.(string)
					path := path_.(string)
					r := r_.(*gin.Engine)
					r.Static(route, path)
					return nil
				}
			}
		}
	}

	exports["noRouteImpl"] = func(r_ Any) Any {
		return func(handler_ Any) Any {
			return func() Any {
				r := r_.(*gin.Engine)
				handler := handler_.(func(*gin.Context))
				r.NoRoute(handler)
				return nil
			}
		}
	}

	exports["getBodyImpl"] = func(ctx_ Any) Any {
		ctx := ctx_.(*gin.Context)
		var b Dict
		ctx.ShouldBindBodyWith(&b, binding.JSON)
		return b
	}

	exports["sendNoContentImpl"] =
		func(ctx_ Any) Any {
			ctx := ctx_.(*gin.Context)
			ctx.Status(http.StatusNoContent)
			return nil
		}

	exports["sendJsonImpl"] =
		func(code_ Any, body_ Any, ctx_ Any) Any {
			ctx := ctx_.(*gin.Context)
			code := code_.(int)
			body := body_.(Dict)
			ctx.JSON(code, body)
			return nil
		}

	exports["sendFileImpl"] = func(path_ Any) Any {
		return func(ctx_ Any) Any {
			ctx := ctx_.(*gin.Context)
			path := path_.(string)
			ctx.File(path)
			return nil
		}
	}

	exports["getImpl"] = func(r_ Any) Any {
		return func(path_ Any) Any {
			return func(handler_ Any) Any {
				return func() Any {
					r := r_.(*gin.Engine)
					path := path_.(string)
					handler := handler_.(func(*gin.Context))
					r.GET(path, handler)
					return nil
				}
			}
		}
	}

	exports["postImpl"] = func(r_ Any) Any {
		return func(path_ Any) Any {
			return func(handler_ Any) Any {
				return func() Any {
					r := r_.(*gin.Engine)
					path := path_.(string)
					handler := handler_.(func(*gin.Context))
					r.POST(path, handler)
					return nil
				}
			}
		}
	}

	exports["runImpl"] = func(r_ Any) Any {
		r := r_.(*gin.Engine)
		r.Run()
		return nil
	}

}
