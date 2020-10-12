// Copyright 05-Sep-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Pool data base.
package poolDb

import (
	"fmt"
	"github.com/dedeme/MrBackup/data/cts"
	"github.com/dedeme/MrBackup/db/log"
	"github.com/dedeme/golib/file"
	"path"
)

func findOutdated(dir string, tm int64) (all, outdated int) {
	for _, e := range file.List(dir) {
		p := path.Join(dir, e.Name())
		if file.IsDirectory(p) {
			a, o := findOutdated(p, tm)
			all += a
			outdated += o
			continue
		}
		if e.Mode().IsRegular() {
			all++
			if file.LastModification(p) > tm {
				outdated++
			}
		}
	}
	return
}

func testFiles() (files, outdatedDirs, outdatedFiles int) {
	pool := cts.MrBackupTargets[0]
	for _, fs := range file.List(pool) {
		dir := fs.Name()
		pdir, ok := readPathTxt(pool, dir)
		if !ok {
			continue
		}

		tgzs := filterTgz(file.List(path.Join(pool, dir)))
		lastName := ""
		lastTime := int64(0)
		for _, e := range tgzs {
			if e > lastName {
				lastName = e
				lastTime = file.LastModification(path.Join(pool, dir, e))
			}
		}

		all, outdated := findOutdated(pdir, lastTime)
		files += all
		if outdated > 0 {
			log.Error(fmt.Sprintf(
				"El directorio (%v) esta obsoleto.", dir,
			))
			outdatedDirs++
			outdatedFiles += outdated
		}
	}
	return
}
