// Copyright 01-Apr-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package db

import (
	"github.com/dedeme/GoDoc/db/conf"
	"github.com/dedeme/GoDoc/db/plibs"
	"github.com/dedeme/golib/cgi"
	"github.com/dedeme/golib/file"
	"path"
)

func Initialize() {
	dataPath := path.Join(cgi.Home(), "data")
	if !file.Exists(dataPath) {
		file.Mkdirs(dataPath)
	}

	conf.Initialize(path.Join(dataPath, "conf.db"))
	plibs.Initialize(
    path.Join(dataPath, "plibs.db"),
    path.Join(dataPath, "slibs.db"),
  )
}
