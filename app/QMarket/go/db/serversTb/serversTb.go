// Copyright 18-Oct-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Servers table.
package serversTb

import (
	"github.com/dedeme/QMarket/data/server"
	"github.com/dedeme/golib/file"
	"github.com/dedeme/golib/json"
	"path"
)

var fpath string

// Initializes table.
//    parent: Parent directory.
func Initialize(parent string) {
	fpath = path.Join(parent, "Servers.tb")
	if !file.Exists(fpath) {
		Write(server.NewTable())
	}
}

// Writes table.
func Write(t *server.TableT) {
	file.WriteAll(fpath, t.ToJs().String())
}

// Reads table
func Read() *server.TableT {
	return server.TableFromJs(json.FromString(file.ReadAll(fpath)))
}
