// Copyright 01-Apr-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package conf

import (
	"fmt"
	"github.com/dedeme/golib/file"
	"github.com/dedeme/golib/json"
)

const (
	lcPath = "lcPath"
	lang   = "lang"
)

var path string

func Initialize(confPath string) {
	path = confPath
	if !file.Exists(path) {
		data := map[string]json.T{
			lcPath: json.Ws(""),
			lang:   json.Ws("es"),
		}
		write(data)
	}
}

func write(data map[string]json.T) {
	file.WriteAll(path, json.Wo(data).String())
}

func read() map[string]json.T {
	return json.FromString(file.ReadAll(path)).Ro()
}

func writeField(key string, value json.T) {
	data := read()
	data[key] = value
	write(data)
}

func readField(key string) (js json.T) {
	js, ok := read()[key]
	if !ok {
		panic(fmt.Sprintf("Field '%v' not found", key))
	}
	return
}

/// Returns location path
func Lib(lib string) string {
  if lib == "" {
    lib = readField(lcPath).Rs();
  }
  if lib == "" {
    lib = "@";
  }

  writeField(lcPath, json.Ws(lib))

	return lib
}

/// Returns lang
func Lang() json.T {
	return readField(lang)
}

/// Sets default location path
func SetLang(js json.T) {
	writeField(lang, js)
}
