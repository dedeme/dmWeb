// Copyright 05-Oct-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Pool data base.
package poolDb

import (
	"fmt"
	"github.com/dedeme/MrBackup/data/cts"
	"github.com/dedeme/MrBackup/data/testRs"
	"github.com/dedeme/MrBackup/db/log"
	"github.com/dedeme/ktlib/file"
	"github.com/dedeme/ktlib/js"
	"github.com/dedeme/ktlib/path"
	"github.com/dedeme/ktlib/str"
	"github.com/dedeme/ktlib/sys"
)

// Read file 'path.txt'
func readPath(pool, dir, f string) (path string, ok bool) {
	defer func() {
		if err := recover(); err != nil {
			log.Error(fmt.Sprintf(
				"El archivo 'path.txt' del directorio (%v)\n"+
					"en el depósito (%v) no es válido.",
				dir, pool,
			))
		}
	}()

	path = js.Rs(file.Read(f))
	ok = true
	return
}

// Reads path.txt testing the file and its parent.
func readPathTxt(pool, dir string) (pth string, ok bool) {
	d := path.Cat(pool, dir)
	if !file.IsDirectory(d) {
		log.Error(fmt.Sprintf(
			"Archivo (%v) del depósito (%v) no es un directorio.", dir, pool,
		))
		return
	}
	p := path.Cat(d, "path.txt")
	if !file.Exists(p) {
		log.Error(fmt.Sprintf(
			"Falta 'path.txt' en (%v) del depósito (%v).", dir, pool,
		))
		return
	}
	pth, ok = readPath(pool, dir, p)
	if !ok {
		return
	}
	if !file.IsDirectory(pth) {
		log.Error(fmt.Sprintf(
			"El archivo de 'path.txt' (%v)\n"+
				"en (%v) del depósito (%v) no es un directorio.",
			pth, dir, pool,
		))
		ok = false
		return
	}
	return
}

// Filters 'fnames' selecting files '.tgz'
func filterTgz(fnames []string) []string {
	var r []string
	for _, nm := range fnames {
		if str.Ends(nm, ".tgz") {
			r = append(r, nm)
		}
	}
	return r
}

// Tests directories.
func TestDirs() (pools, poolsBad, dirs, dirsBad int) {
	pools, poolsBad, dirs, dirsBad = testDirs() // In testDirs.go
	return
}

// Tests files (Only use the pool 'base')
func TestFiles() (files, outdatedDirs, outdatedFiles int) {
	files, outdatedDirs, outdatedFiles = testFiles() // In testFiles.go
	return
}

// Update backups
func Update() {
	update() // in update.go
}

// Update backups of a directory.
func UpdateDir(id string) {
	for _, pool := range cts.MrBackupTargets {
		updateDir(pool, id) // in update.go
	}
}

// Global test
func GlobalTest() map[string]*testRs.T {
	return globalTest() // in globalTest.go
}

// Shows every pool directory.
func ShowDirs(id string) {
	for _, e := range cts.MrBackupTargets {
		d := path.Cat(e, id)
		if file.IsDirectory(d) {
			sys.Cmd("thunar", d)
		} else {
			sys.Cmd("thunar", e)
		}
	}
}

// Copies other directory to base.
func CopyToBase(id string) {
	source := ""
	for i := 1; i < len(cts.MrBackupTargets); i++ {
		pool := cts.MrBackupTargets[i]
		d := path.Cat(pool, id)
		if file.IsDirectory(d) {
			source = pool
			break
		}
	}
	if source != "" && source != cts.MrBackupTargets[0] {
		file.Copy(path.Cat(source, id), cts.MrBackupTargets[0])
	}
}

// Copies base to other directories.
func CopyFromBase(id string) {
	source := path.Cat(cts.MrBackupTargets[0], id)
	if file.IsDirectory(source) {
		for i := 1; i < len(cts.MrBackupTargets); i++ {
			pool := cts.MrBackupTargets[i]
			file.Del(path.Cat(pool, id))
			file.Copy(source, pool)
		}
	}
}

// Creates path.txt
func CreatePathTxt(id string) {
	for _, pool := range cts.MrBackupTargets {
		d := path.Cat(pool, id)
		if file.IsDirectory(d) {
			file.Write(path.Cat(d, "path.txt"), js.Ws(""))
		}
	}
}

// Adds directory
func AddDir(id string) {
	for _, pool := range cts.MrBackupTargets {
		d := path.Cat(pool, id)
		file.Mkdir(d)
		file.Write(path.Cat(d, "path.txt"), js.Ws(""))
	}
}

// Delete a directory
func Delete(id string) {
	for _, pool := range cts.MrBackupTargets {
		d := path.Cat(pool, id)
		file.Del(d)
	}
}

// Changes directory identifier.
func ChangeDir(oldId, newId string) {
	for _, pool := range cts.MrBackupTargets {
		d := path.Cat(pool, oldId)
		if file.IsDirectory(d) {
			newD := path.Cat(pool, newId)
			file.Rename(d, newD)
		}
	}
}

// Changes directory calification of big.
func ChangeBig(id string) {
	f := path.Cat(cts.MrBackupTargets[0], id, "big")
	exists := false
	if file.Exists(f) {
		exists = true
	}

	for _, pool := range cts.MrBackupTargets {
		f := path.Cat(pool, id, "big")
		if exists {
			if file.Exists(f) {
				file.Del(f)
			}
		} else {
			if !file.Exists(f) {
				file.Write(f, "")
			}
		}
	}
}

// Changes path.txt
func ChangePath(id, newPath string) {
	for _, pool := range cts.MrBackupTargets {
		d := path.Cat(pool, id)
		if file.IsDirectory(d) {
			file.Write(path.Cat(d, "path.txt"), js.Ws(newPath))
		}
	}
}
