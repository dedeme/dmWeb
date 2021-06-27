// Copyright 05-Oct-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Pool data base.
package poolDb

import (
	"errors"
	"fmt"
	"github.com/dedeme/MrBackup/data/cts"
	"github.com/dedeme/MrBackup/db/log"
	"github.com/dedeme/golib/date"
	"github.com/dedeme/golib/file"
	"github.com/dedeme/golib/sys"
	"os"
	"path"
	"sort"
	"strings"
	"time"
)

func tar(source, target string) (err error) {
	cwd, err := os.Getwd()
	if err != nil {
		return
	}
	defer os.Chdir(cwd)
	defer func() {
		if e := recover(); e != nil {
			err = errors.New("Fail in 'tar' command")
		}
	}()
	os.Chdir(path.Dir(source))
	o, ee := sys.Cmd("tar", "-czf", target, path.Base(source))
	if len(o) > 0 || len(ee) > 0 {
		err = errors.New(string(o) + "\n" + string(ee))
		return
	}

	return
}

func clean(dir string) {
	dnow := date.Now()
	now := dnow.String()

	var all []string
	for _, e := range file.List(dir) {
		if !e.Mode().IsRegular() {
			f := path.Join(dir, e.Name())
			file.Remove(f)
			log.Error(fmt.Sprintf("Deleted %v\n", f))
		}
		all = append(all, e.Name())
	}

  isBig := false
	var tgzs []string
	for _, e := range all {
		if e == "path.txt" {
			continue
		}
    if e == "big" {
      isBig = true
      continue
    }
		if !strings.HasSuffix(e, ".tgz") {
			f := path.Join(dir, e)
			file.Remove(f)
			log.Error(fmt.Sprintf("Deleted %v\n", f))
			continue
		}
		date := e[:len(e)-4]
		_, err := time.Parse("20060102", date)
		if err != nil {
			f := path.Join(dir, e)
			file.Remove(f)
			log.Error(fmt.Sprintf("Deleted %v\n", f))
			continue
		}
		if date < "20190101" || date > now {
			f := path.Join(dir, e)
			file.Remove(f)
			log.Error(fmt.Sprintf("Deleted %v\n", f))
			continue
		}
		tgzs = append(tgzs, date)
	}
	sort.Slice(tgzs, func(i, j int) bool {
		return tgzs[i] > tgzs[j]
	})

  if isBig {
    if len(tgzs) > 3 {
      nowM := now[:6]
      nowY := now[:4]
      var toDelete []string
      day := true
      month := true
      year := true
      saved := 0
      for _, tgz := range tgzs {
        if day {
          day = false
          saved++
          continue
        }
        if month {
          if tgz[:6] == nowM {
            toDelete = append(toDelete, tgz)
            continue
          }
          month = false
          saved++
          continue
        }
        if year {
          if tgz[:4] == nowY {
            toDelete = append(toDelete, tgz)
            continue
          }
          year = false
          saved++
          continue
        }
        toDelete = append(toDelete, tgz)
      }
      if saved < 3 {
        toDelete = toDelete[3 - saved:]
      }
      for _, tgz := range toDelete {
        f := path.Join(dir, tgz+".tgz")
        file.Remove(f)
      }
    }
  } else {
    nowD := dnow.Add(-7).String()
    lastM := now[:6]
    nowY := now[:4]
    lastY := nowY
    for _, e := range tgzs {
      if e >= nowD {
        continue
      }
      eY := e[:4]
      if eY == nowY {
        eM := e[:6]
        if eM != lastM {
          lastM = eM
          continue
        }
      } else if eY != lastY {
        lastY = eY
        continue
      }
      f := path.Join(dir, e+".tgz")
      file.Remove(f)
    }
  }
}

func backup(source, target string) (ok bool) {
	fileName := date.Now().String() + ".tgz"
	tmpDir := file.TempDir("MrBackup")
	defer file.Remove(tmpDir)

	tmpFile := path.Join(tmpDir, fileName)
	if e := tar(source, tmpFile); e != nil {
		log.Error(fmt.Sprintln(e))
		return
	}

	for _, pool := range cts.MrBackupTargets {
		poolDir := path.Join(pool, target)
		if e := file.Copy(tmpFile, poolDir); e != nil {
			log.Error(fmt.Sprintln(e))
			return
		}
		clean(poolDir)
	}

	ok = true
	return
}

func shouldUpdate(dir string, tm int64) bool {
	for _, e := range file.List(dir) {
		p := path.Join(dir, e.Name())
		if file.IsDirectory(p) {
			if shouldUpdate(p, tm) {
				return true
			}
			continue
		}
		if e.Mode().IsRegular() && file.LastModification(p) > tm {
			return true
		}
	}
	return false
}

func updateDir(pool, dir string) {
	pdir, ok := readPathTxt(pool, dir)
	if !ok {
		f := path.Join(pool, dir)
		if !file.IsDirectory(f) {
			file.Remove(f)
			log.Error(fmt.Sprintf("Deleted %v\n", f))
		}

		return // skip directory without 'path.txt'
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

	if shouldUpdate(pdir, lastTime) {
		log.Info(dir + ": Updating...")
		if backup(pdir, dir) {
			log.Info(dir + ": Updated.")
		} else {
			log.Error(dir + ": Updating FAILED!!!")
		}
	}
}

func update() {
	log.Info("Updating backups...")
	pool := cts.MrBackupTargets[0]
	for _, fs := range file.List(pool) {
		dir := fs.Name()
		updateDir(pool, dir)
	}
	log.Info("Bakups updated")
}
