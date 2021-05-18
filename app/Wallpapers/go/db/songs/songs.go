// Copyright 07-May-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Songs data base.
package songs

import (
	"github.com/dedeme/golib/json"
	"io/ioutil"
	"strings"
)

var path string // Path of original songs

// Initialize data base
func Initialize() {
	path = "/dm/musica/relax"
}

// Returns a  JSONized slice of strings.
func Read() json.T {
	var ss []string
	infs, err := ioutil.ReadDir(path)
	if err != nil {
		panic(err)
	}

	for _, inf := range infs {
		if strings.HasSuffix(inf.Name(), ".mp3") {
			ss = append(ss, inf.Name())
		}
	}

	if len(ss) == 0 {
		panic("Songs not found")
	}

	var r []json.T
	for _, s := range ss {
		r = append(r, json.Ws(s))
	}
	return json.Wa(r)
}
