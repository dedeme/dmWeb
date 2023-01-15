// Copyright 05-Sep-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Settings data base.
package conf

import (
	"github.com/dedeme/ktlib/file"
	"github.com/dedeme/ktlib/js"
	"github.com/dedeme/ktlib/path"
)

const (
	langKey = "lang"
	menuKey = "menu"
)

var fpath string

// Auxiliar function
func write(cf map[string]string) {
	file.Write(fpath, js.Wo(cf))
}

// Auxiliar function
func read() map[string]string {
	return js.Ro(file.Read(fpath))
}

// Auxiliar function
func getField(key string) (value string, ok bool) {
	value, ok = read()[key]
	return
}

// Auxiliar function
func setField(key string, js string) {
	cf := read()
	cf[key] = js
	write(cf)
}

// Intializes table.
//    parent: Parent directory of "Conf.tb".
func Initialize(parent string) {
	fpath = path.Cat(parent, "Conf.tb")
	if !file.Exists(fpath) {
		write(map[string]string{
			langKey: js.Ws("es"),
		})
	}
}

// Gets web language.
func Lang() string {
	value, _ := getField(langKey)
	return js.Rs(value)
}

// Sets web language.
//    lang: 'es' or 'en'.
func SetLang(lang string) {
	setField(langKey, js.Ws(lang))
}

// Gets menu option.
func Menu() string {
	value, ok := getField(menuKey)
	if ok {
		return js.Rs(value)
	}
	return ""
}

// Sets menu option.
//    option: A valid option menu.
func SetMenu(option string) {
	setField(menuKey, js.Ws(option))
}
