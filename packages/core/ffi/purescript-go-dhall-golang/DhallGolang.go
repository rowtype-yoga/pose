package purescript_go_dhall_golang

import (
	"errors"
	"fmt"
	"reflect"

	"github.com/philandstuff/dhall-golang/v6/parser"
	"github.com/philandstuff/dhall-golang/v6/term"
	. "github.com/purescript-native/go-runtime"
)

func init() {
	exports := Foreign("Dhall.Golang")

	exports["parseSpagoDhall_"] = func(left Any, right Any, just Any, nothing Any, path_ Any) Any {

		var errs []Any
		path := path_.(string)

		res, err := parser.ParseFile(path)
		if err != nil {
			errs = append(errs, errors.New("Could not parse file "+path))
			return Apply(left, errs)
		}
		result := Dict{}
		switch root := res.(type) {
		case term.RecordLit:

			// name
			name, err := parseString("name", root["name"])
			if err != nil {
				errs = append(errs, err)
			}
			result["name"] = name

			// dependencies
			dependencies, err := parseStringArray("dependencies", root["dependencies"])
			if err != nil {
				errs = append(errs, err)
			}
			result["dependencies"] = dependencies

			// packages
			packages, err := parseImportAsString("packages", root["packages"])
			if err != nil {
				errs = append(errs, err)
			}
			result["packages"] = packages

			// sources
			sources, err := parseStringArray("sources", root["sources"])
			if err != nil {
				errs = append(errs, err)
			}
			result["sources"] = sources

		default:
			errs = append(errs, errors.New(path+" is a valid .dhall file but does not have a record at the top-level"))

		}
		if errs != nil && len(errs) != 0 {
			return Apply(left, errs)
		}
		fmt.Println(result)
		return Apply(right, result)
	}

	exports["parsePackagesDhall_"] = func(left Any, right Any, just Any, nothing Any, path_ Any) Any {

		var errs []Any
		path := path_.(string)

		res, err := parser.ParseFile(path)
		if err != nil {
			errs = append(errs, errors.New("Could not parse file "+path))
			return Apply(left, errs)
		}
		result := Dict{}
		fmt.Println(reflect.TypeOf(res))
		switch lets := res.(type) {
		case term.Let:

			var mkPackage Any = nil
			var upstream Any = nil
			var additions Any = nil
			for _, binding := range lets.Bindings {
				switch binding.Variable {

				case "mkPackage":
					url, err := parseImportAsString("mkPackage", binding.Value)
					if err != nil {
						// [TODO]: There could be some malformed shit
						// errs = append(errs, err)
						mkPackage = nothing
					} else {
						mkPackage = Apply(just, Dict{"url": *url})
					}

				case "upstream":
					url, err := parseImportAsString("upstream", binding.Value)
					if err != nil {
						errs = append(errs, err)
					}
					if url != nil {
						upstream = Dict{"url": *url}
					}
					fmt.Println("\n\n\n\n", binding, "\n\n\n")

				case "additions":
					var adds []Any = nil
					err := parseRecord("additions", binding.Value, func(rec term.RecordLit) {
						for name, addition := range rec {
							local, localErr := parseLocalDep("addition "+name, addition)
							if localErr != nil {
								// parse the alternative
								mkPackage, mkPackageErr := parseMkPackage("addition "+name, addition)
								if mkPackageErr != nil {
									errs = append(errs, mkPackageErr)
									errs = append(errs, localErr)
								} else { // isMkPackage
									adds = append(adds, Apply(right, mkPackage))
								}
							} else { // isLocal
								adds = append(adds, Apply(left, local))
							}
						}
					})
					if err != nil {
						errs = append(errs, err)
					}
					if additions != nil {
						additions = adds
					}

				default:
					errs = append(errs, errors.New("Unexpected binding "+binding.Variable))
				}

			}
			if mkPackage == nil {
				errs = append(errs, errors.New("Missing binding mkPackage"))
			} else {
				result["mkPackage"] = mkPackage
			}
			if upstream == nil {
				errs = append(errs, errors.New("Missing binding upstream"))
			} else {
				result["upstream"] = upstream
			}
			if additions == nil {
				errs = append(errs, errors.New("Missing binding additions"))
			} else {
				result["additions"] = additions
			}

			// fmt.Println(lets, reflect.TypeOf(lets))
		default:
			errs = append(errs, errors.New(path+" is a valid .dhall file but does not have the required bindings mkPackage, upstream, and additions"))
		}
		fmt.Println(result)
		if errs != nil && len(errs) != 0 {
			return Apply(left, errs)
		}
		return Apply(right, result)
	}

}

func parseLocalDep(keyName string, t term.Term) (*string, error) {
	return parseImportAsString(keyName, t)
}

func parseMkPackage(keyName string, t term.Term) (Dict, error) {
	result := Dict{}
	// Parses from right to left
	versionApp, versionAppErr := parseApp("version", t)
	if versionAppErr != nil {
		return nil, versionAppErr
	}
	version, versionErr := parseString("version", versionApp.Fn)
	if versionAppErr != nil {
		return nil, versionErr
	}
	result["version"] = version

	addressApp, addressAppErr := parseApp("address", versionApp.Arg)
	if addressAppErr != nil {
		return nil, addressAppErr
	}
	address, addressErr := parseString("address", addressApp.Fn)
	if addressErr != nil {
		return nil, addressErr
	}
	result["address"] = address

	dependenciesApp, dependenciesAppErr := parseApp("dependencies", addressApp.Arg)
	if dependenciesAppErr != nil {
		return nil, dependenciesAppErr
	}
	dependencies, dependenciesErr := parseStringArray("dependencies", dependenciesApp.Fn)
	if dependenciesErr != nil {
		return nil, dependenciesErr
	}
	result["dependencies"] = dependencies

	fnNameApp, fnNameAppErr := parseApp("function name", dependenciesApp.Arg)
	if fnNameAppErr != nil {
		return nil, fnNameAppErr
	}
	fnName, fnNameErr := parseString("function name", fnNameApp.Fn)
	if fnNameErr != nil {
		return nil, fnNameErr
	}
	if *fnName != "mkPackage" {
		return nil, errors.New("Used function " + *fnName + " instead of mkFunction")
	}

	return result, nil
}

func parseApp(keyName string, t term.Term) (*term.App, error) {
	value, isApp := t.(term.App)
	if !isApp {
		return nil, errors.New(keyName + " is not a function application")
	}
	return &value, nil
}

func parseRecord(keyName string, t term.Term, parse func(term.RecordLit)) error {
	valueRec, valueIsRecord := t.(term.RecordLit)
	if !valueIsRecord {
		return errors.New(keyName + " is not a record")
	}
	parse(valueRec)
	return nil
}

func parseImportAsString(keyName string, t term.Term) (*string, error) {
	valueLit, valueIsImport := t.(term.Import)
	if !valueIsImport {
		return nil, errors.New(keyName + " is not an import")
	}
	n := valueLit.String()
	return &n, nil
}

func parseString(keyName string, t term.Term) (*string, error) {
	valueLit, valueIsString := t.(term.TextLit)
	if !valueIsString {
		return nil, errors.New(keyName + " is not a string")
	}
	n := valueLit.String()
	return &n, nil
}

func parseStringArray(name string, t term.Term) (*[]string, error) {
	nonEmpty, isNonEmpty := t.(term.NonEmptyList)
	_, isEmpty := t.(term.EmptyList)
	if !isNonEmpty && !isEmpty {
		return nil, errors.New(name + " is neither empty nor non-empty")
	}
	length := len(nonEmpty)
	dependencies := make([]string, length)
	for idx, dep := range nonEmpty {
		str, ok := dep.(term.TextLit)
		if !ok {
			return nil, errors.New(name + "Encountered non string in dependencies")
		}
		dependencies[idx] = str.String()
	}
	return &dependencies, nil
}
