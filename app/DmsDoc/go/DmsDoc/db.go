// Copyright 19-06-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Data base
package main

import (
	"github.com/dedeme/golib/file"
	"github.com/dedeme/golib/json"
	"path"
)

func readSettings() json.T {
	p := path.Join(HOME, "data.db")
	if !file.Exists(p) {
		return json.Wo(map[string]json.T{
			"conf": json.Wo(map[string]json.T{
				"show":   json.Wb(true),
				"lang":   json.Ws("es"),
				"source": json.Ws("@"),
			}),
			"paths": json.Wa([]json.T{}),
		})
	}

	r := json.FromString(file.ReadAll(p))
	ob := r.Ro()
	es := ob["paths"].Ra()
	for i, e := range es {
		fs := e.Ra()
		// e[1] = path,
		// e[2] = isValid,
		// e[3] = isShown
		if file.IsDirectory(fs[1].Rs()) {
			fs[2] = json.Wb(true)
		} else {
			fs[2] = json.Wb(false)
			fs[3] = json.Wb(false)
		}
		es[i] = json.Wa(fs)
	}
	ob["paths"] = json.Wa(es)
	return json.Wo(ob)
}

func writeSettings(data json.T) {
	p := path.Join(HOME, "data.db")
	file.WriteAll(p, data.String())
}
