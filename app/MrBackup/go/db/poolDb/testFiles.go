// Copyright 05-Sep-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Pool data base.
package poolDb

import (
	"fmt"
	"github.com/dedeme/MrBackup/data/cts"
	"github.com/dedeme/MrBackup/db/log"
	"github.com/dedeme/ktlib/file"
	"github.com/dedeme/ktlib/path"
)

func findOutdated(dir string, tm int64) (all, outdated int) {
	for _, fname := range file.Dir(dir) {
		p := path.Cat(dir, fname)
		if file.IsDirectory(p) {
			a, o := findOutdated(p, tm)
			all += a
			outdated += o
			continue
		}
		if file.IsRegular(p) {
			all++
			if file.Tm(p) > tm {
				outdated++
			}
		}
	}
	return
}

func testFiles() (files, outdatedDirs, outdatedFiles int) {
	pool := cts.MrBackupTargets[0]
	for _, dir := range file.Dir(pool) {
		pdir, ok := readPathTxt(pool, dir)
		if !ok {
			continue
		}

		tgzs := filterTgz(file.Dir(path.Cat(pool, dir)))
		lastName := ""
		lastTime := int64(0)
		for _, e := range tgzs {
			if e > lastName {
				lastName = e
				lastTime = file.Tm(path.Cat(pool, dir, e))
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
