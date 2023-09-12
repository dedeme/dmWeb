// Copyright 08-Oct-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package poolDb

import (
	"github.com/dedeme/MrBackup/data/cts"
	"github.com/dedeme/MrBackup/data/testRs"
	"github.com/dedeme/ktlib/file"
	"github.com/dedeme/ktlib/js"
	"github.com/dedeme/ktlib/path"
	"os"
)

func lastTgzOk(base, pool, n string) bool {
	baseTgzs := filterTgz(file.Dir(path.Cat(base, n)))
	poolTgzs := filterTgz(file.Dir(path.Cat(pool, n)))
	if len(baseTgzs) == 0 {
		return len(poolTgzs) == 0
	}

	lastBase := baseTgzs[0]
	for _, e := range baseTgzs {
		if e > lastBase {
			lastBase = e
		}
	}
	lastPool := poolTgzs[0]
	for _, e := range poolTgzs {
		if e > lastPool {
			lastPool = e
		}
	}

	return lastBase == lastPool
}

func equalsPathTxt(base, pool, n string) bool {
	tx, ok := readPath(base, n, path.Cat(base, n, "path.txt"))
	tx2, ok2 := readPath(pool, n, path.Cat(pool, n, "path.txt"))
	if ok {
		return tx == tx2
	}
	return ok == ok2
}

func globalTest() map[string]*testRs.T {
	rs := map[string]*testRs.T{}
	poolBase := cts.MrBackupTargets[0]
	var dirNames []string
	for _, dirName := range file.Dir(poolBase) {
		t := testRs.New()
		dirNames = append(dirNames, dirName)
		dir := path.Cat(poolBase, dirName)
		if !file.IsDirectory(dir) {
			continue
		}

		ftxt := path.Cat(dir, "path.txt")
		if info, err := os.Stat(ftxt); err == nil && info.Mode().IsRegular() {
			t.WithPathTxt = true
			tx := file.Read(ftxt)
			t.Path = js.Rs(tx)
			if tx != "" && file.IsDirectory(t.Path) {
				t.PathOk = true
			}
		}

		fbig := path.Cat(dir, "big")
		if file.Exists(fbig) {
			t.IsBig = true
		}

		if len(file.Dir(dir)) > 1 {
			t.WithBackups = true
		}

		rs[dirName] = t
	}

	for i, pool := range cts.MrBackupTargets {
		if i == 0 {
			continue
		}
		var ldirNames []string
		for _, dirName := range file.Dir(pool) {
			ldirNames = append(ldirNames, dirName)

			isMissing := true
			for _, n := range dirNames {
				if n == dirName {
					isMissing = false
					break
				}
			}
			if isMissing {
				t := testRs.New()
				t.NotInBase = true
				rs[dirName] = t
			}
		}

		for _, n := range dirNames {
			isMissing := true
			for _, ln := range ldirNames {
				if n == ln {
					if !lastTgzOk(poolBase, pool, n) || !equalsPathTxt(poolBase, pool, n) {
						t := rs[n]
						t.Synchronized = false
					}
					isMissing = false
				}
			}
			if isMissing {
				t := rs[n]
				t.IsMissing = true
			}
		}
	}

	return rs
}
