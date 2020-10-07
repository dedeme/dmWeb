// Copyright 05-Sep-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Pool data base.
package poolDb

import (
	"fmt"
	"github.com/dedeme/MrBackup/db/log"
	"github.com/dedeme/golib/file"
	"github.com/dedeme/golib/json"
	"os"
	"path"
	"strings"
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

	path = json.FromString(file.ReadAll(f)).Rs()
	ok = true
	return
}

// Reads path.txt testing the file and its parent.
func readPathTxt(pool, dir string) (pth string, ok bool) {
	d := path.Join(pool, dir)
	if !file.IsDirectory(d) {
		log.Error(fmt.Sprintf(
			"Archivo (%v) del depósito (%v) no es un directorio.", dir, pool,
		))
		return
	}
	p := path.Join(d, "path.txt")
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
			dir, pool,
		))
		ok = false
		return
	}
	return
}

// Filters 'fs' selecting files '.tgz'
func filterTgz(fs []os.FileInfo) []string {
	var r []string
	for _, e := range fs {
		nm := e.Name()
		if strings.HasSuffix(nm, ".tgz") {
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
