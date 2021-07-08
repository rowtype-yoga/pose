package purescript_go_os

import (
	"bytes"
	"fmt"
	"io/ioutil"
	"os"
	"os/exec"
	"path/filepath"
	"strconv"
	"time"

	. "github.com/purescript-native/go-runtime"
)

func init() {
	exports := Foreign("OS")
	exports["sleep"] = func(millis_ Any) Any {
		return func() Any {
			var millis int = int(millis_.(float64))
			duration := time.Duration(millis) * time.Millisecond
			time.Sleep(duration)
			return nil
		}
	}

	exports["runEffectFn2"] = func(fn_ Any) Any {
		return func(a Any) Any {
			return func(b Any) Any {
				return func() Any {
					fn := fn_.(Fn2)
					return fn(a, b)
				}
			}
		}
	}

	exports["runEffectFn3"] = func(fn_ Any) Any {
		return func(a Any) Any {
			return func(b Any) Any {
				return func(c Any) Any {
					return func() Any {
						fn := fn_.(Fn3)
						return fn(a, b, c)
					}
				}
			}
		}
	}

	exports["runEffectFn4"] = func(fn_ Any) Any {
		return func(a Any) Any {
			return func(b Any) Any {
				return func(c Any) Any {
					return func(d Any) Any {
						return func() Any {
							fn := fn_.(Fn4)
							return fn(a, b, c, d)
						}
					}
				}
			}
		}
	}

	exports["runEffectFn5"] = func(fn_ Any) Any {
		return func(a Any) Any {
			return func(b Any) Any {
				return func(c Any) Any {
					return func(d Any) Any {
						return func(e Any) Any {
							return func() Any {
								fn := fn_.(Fn5)
								return fn(a, b, c, d, e)
							}
						}
					}
				}
			}
		}
	}

	exports["runEffectFn6"] = func(fn_ Any) Any {
		return func(a Any) Any {
			return func(b Any) Any {
				return func(c Any) Any {
					return func(d Any) Any {
						return func(e Any) Any {
							return func(f Any) Any {
								return func() Any {
									fn := fn_.(Fn6)
									return fn(a, b, c, d, e, f)
								}
							}
						}
					}
				}
			}
		}
	}

	exports["runEffectFn7"] = func(fn_ Any) Any {
		return func(a Any) Any {
			return func(b Any) Any {
				return func(c Any) Any {
					return func(d Any) Any {
						return func(e Any) Any {
							return func(f Any) Any {
								return func(g Any) Any {
									return func() Any {
										fn := fn_.(Fn7)
										return fn(a, b, c, d, e, f, g)
									}
								}
							}
						}
					}
				}
			}
		}
	}

	exports["runEffectFn8"] = func(fn_ Any) Any {
		return func(a Any) Any {
			return func(b Any) Any {
				return func(c Any) Any {
					return func(d Any) Any {
						return func(e Any) Any {
							return func(f Any) Any {
								return func(g Any) Any {
									return func(h Any) Any {
										return func() Any {
											fn := fn_.(Fn8)
											return fn(a, b, c, d, e, f, g, h)
										}
									}
								}
							}
						}
					}
				}
			}
		}
	}

	exports["runEffectFn9"] = func(fn_ Any) Any {
		return func(a Any) Any {
			return func(b Any) Any {
				return func(c Any) Any {
					return func(d Any) Any {
						return func(e Any) Any {
							return func(f Any) Any {
								return func(g Any) Any {
									return func(h Any) Any {
										return func(i Any) Any {
											return func() Any {
												fn := fn_.(Fn9)
												return fn(a, b, c, d, e, f, g, h, i)
											}
										}
									}
								}
							}
						}
					}
				}
			}
		}
	}

	exports["runEffectFn10"] = func(fn_ Any) Any {
		return func(a Any) Any {
			return func(b Any) Any {
				return func(c Any) Any {
					return func(d Any) Any {
						return func(e Any) Any {
							return func(f Any) Any {
								return func(g Any) Any {
									return func(h Any) Any {
										return func(i Any) Any {
											return func(j Any) Any {
												return func() Any {
													fn := fn_.(Fn10)
													return fn(a, b, c, d, e, f, g, h, i, j)
												}
											}
										}
									}
								}
							}
						}
					}
				}
			}
		}
	}

	exports["concatPaths"] = func(args_ Any) Any {
		args := args_.([]Any)
		args2 := make([]string, len(args))
		for i, arg := range args {
			args2[i] = arg.(string)
		}
		return filepath.Join(args2...)
	}

	exports["fromSlash"] = func(name_ Any) Any {
		name := name_.(string)
		return filepath.FromSlash(name)
	}

	exports["writeStringToFileImpl"] = func(just Any, nothing Any, toWrite_ Any, filePath_ Any) Any {
		filePath := filePath_.(string)
		toWrite := toWrite_.(string)
		fileHandle, err := os.Create(filePath)
		if err != nil {
			return Apply(just, err.Error())
		}
		defer fileHandle.Close()
		_, err = fileHandle.WriteString(toWrite)
		if err != nil {
			return Apply(just, err.Error())
		}
		fileHandle.Sync()
		return nothing
	}

	exports["mkdirAllImpl"] = func(just Any, nothing Any, path_ Any) Any {
		path := path_.(string)
		err := os.MkdirAll(path, os.ModePerm)
		if err != nil {
			return Apply(just, err.Error())
		}
		return nothing

	}

	exports["folderExistsImpl"] = func(folderName_ Any) Any {
		folderName := folderName_.(string)
		info, err := os.Stat(folderName)
		if os.IsNotExist(err) || !info.IsDir() {
			return false
		}
		return true
	}

	exports["fileExistsImpl"] = func(fileName_ Any) Any {
		fileName := fileName_.(string)
		info, err := os.Stat(fileName)
		if os.IsNotExist(err) || info.IsDir() {
			return false
		}
		return true
	}

	exports["lookPathImpl"] =
		func(just Any, nothing Any, name_ Any) Any {
			name := name_.(string)
			res, err := exec.LookPath(name)
			if err != nil {
				return nothing
			}
			return Apply(just, res)
		}

	exports["readFileAsStringImpl"] = func(left Any, right Any, path_ Any) Any {
		path := path_.(string)
		dat, err := ioutil.ReadFile(path)
		if err != nil {
			return Apply(left, err.Error())
		}
		return Apply(right, string(dat))
	}

	exports["execImpl"] =
		func(left Any, right Any, mkResult Any, name_ Any, args_ Any) Any {
			name := name_.(string)
			args := args_.([]Any)
			args2 := make([]string, len(args))
			for i, arg := range args {
				args2[i] = arg.(string)
			}

			cmd := exec.Command(name, args2...)
			fmt.Printf("Cmd %v \n", cmd)

			var stdout, stderr bytes.Buffer
			cmd.Stdout = &stdout
			cmd.Stderr = &stderr

			err := cmd.Run()
			if err != nil {
				return Apply(left, err)
			}
			outStr, errStr := string(stdout.Bytes()), string(stderr.Bytes())
			return Apply(right, Apply(mkResult, outStr, errStr))
		}

	exports["fromStringAsImpl"] = func(just Any) Any {
		return func(nothing Any) Any {
			return func(radix_ Any) Any {
				return func(s_ Any) Any {
					radix := radix_.(int)
					s := s_.(string)
					res, err := strconv.ParseInt(s, 0, radix)
					if err != nil {
						return nothing
					}
					return Apply(just, res)
				}
			}
		}
	}

	exports["stdinAsString"] = func() Any {
		bytes, _ := ioutil.ReadAll(os.Stdin)
		return string(bytes)
	}

}
