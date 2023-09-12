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

// Tests directory condition (1).
//    - Pool exists.
//    - Directory is an existent directory
//    - Directory contains a correct 'path.txt'
func testDir1(pool string) (files, bads int) {
	if !file.Exists(pool) {
		log.Error(fmt.Sprintf(
			"Depósito (%v) no encontrado.", pool,
		))
		files = 1
		bads = 1
		return
	}

	for _, fname := range file.Dir(pool) {
		files++
		_, ok := readPathTxt(pool, fname)
		if !ok {
			bads++
		}
	}
	return
}

// Tests directory condition (2).
//    - Directories of pool are the same that directories of base.
func testDir2(base []string, poolId string, poolDirs []string) (r int) {
	for _, e1 := range base {
		missing := true
		for _, e2 := range poolDirs {
			if e2 == e1 {
				missing = false
				break
			}
		}
		if missing {
			log.Error(fmt.Sprintf(
				"El directorio (%v) falta en (%v)", e1, poolId,
			))
			r++
		}
	}

	for _, e1 := range poolDirs {
		missing := true
		for _, e2 := range base {
			if e2 == e1 {
				missing = false
				break
			}
		}
		if missing {
			log.Error(fmt.Sprintf(
				"El directorio (%v) sobra en (%v)", e1, poolId,
			))
			r++
		}
	}

	return
}

type nameSize struct {
	name string
	size int64
}

func mkMapNameSize(pool string) map[string]*nameSize {
	r := map[string]*nameSize{}
	for _, fname := range file.Dir(pool) {
		d := fname
		tgzs := filterTgz(file.Dir(path.Cat(pool, d)))
		if len(tgzs) == 0 {
			r[d] = &nameSize{name: "", size: 0}
			continue
		}
		name := tgzs[0]
		for _, e := range tgzs {
			if e > name {
				name = e
			}
		}
		r[d] = &nameSize{name: name, size: file.Size(path.Cat(pool, d, name))}
	}
	return r
}

func testDirs() (pools, poolsBad, dirs, dirsBad int) {
	pools = len(cts.MrBackupTargets)
	base := cts.MrBackupTargets[0]
	files, bads := testDir1(base)
	dirs = files
	if bads > 0 {
		poolsBad = pools
		dirsBad = bads
		return
	}

	var baseDirs []string
	for _, fname := range file.Dir(base) {
		baseDirs = append(baseDirs, fname)
	}

	var poolsOk []string
	for i := 1; i < pools; i++ {
		pool := cts.MrBackupTargets[i]
		_, bads := testDir1(pool)
		if bads > 0 {
			poolsBad++
			dirsBad += bads
			continue
		}

		var poolDirs []string
		for _, fname := range file.Dir(pool) {
			poolDirs = append(baseDirs, fname)
		}
		bads = testDir2(baseDirs, pool, poolDirs)
		if bads > 0 {
			poolsBad++
			dirsBad += bads
			continue
		}
		poolsOk = append(poolsOk, pool)
	}

	baseMap := mkMapNameSize(base)
	for _, pool := range poolsOk {
		poolMap := mkMapNameSize(pool)
		bads := 0
		for k, v := range baseMap {
			pv := poolMap[k]
			if v.name != pv.name {
				log.Error(fmt.Sprintf(
					"El archivo (%v) no es el último de (%v) en (%v)",
					v.name, k, pool,
				))
				bads++
				continue
			}
			if v.size != pv.size {
				log.Error(fmt.Sprintf(
					"Incorrecto tamaño del archivo (%v) de (%v) en (%v).\n"+
						"Esperado: %v. Real: %v.",
					v.name, k, pool, v.size, pv.size,
				))
				bads++
				continue
			}
		}
		if bads > 0 {
			poolsBad++
			dirsBad += bads
		}
	}
	return
}
