// Copyright 29-Apr-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Data base initializacion
package db

import (
	"github.com/dedeme/Wallpapers/db/danceSongs"
	"github.com/dedeme/Wallpapers/db/picts"
	"github.com/dedeme/Wallpapers/db/sels"
	"github.com/dedeme/Wallpapers/db/songs"
	"github.com/dedeme/golib/cgi"
	"github.com/dedeme/golib/file"
	"path"
)

var dpath string

func Initialize() {
	dpath = path.Join(cgi.Home(), "data")
	if !file.Exists(dpath) {
		file.Mkdir(dpath)
	}

	sels.Initialize(dpath)
	picts.Initialize(dpath)
	songs.Initialize()
	danceSongs.Initialize(dpath)
}
