// Copyright 05-Oct-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Pool data base.
package poolDb

import (
	"errors"
	"fmt"
	"github.com/dedeme/MrBackup/data/cts"
	"github.com/dedeme/MrBackup/db/log"
	"github.com/dedeme/ktlib/time"
	"github.com/dedeme/ktlib/file"
	"github.com/dedeme/ktlib/sys"
	"github.com/dedeme/ktlib/path"
	"os"
	"sort"
	"strings"
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
	os.Chdir(path.Parent(source))
	o, ee := sys.Cmd("tar", "-czf", target, path.Base(source))
	if len(o) > 0 || len(ee) > 0 {
		err = errors.New(string(o) + "\n" + string(ee))
		return
	}

	return
}

func clean(dir string) {
	dnow := time.Now()
	now := time.ToStr(dnow)

	var all []string
	for _, fname := range file.Dir(dir) {
    p := path.Cat(dir, fname)
		if file.IsDirectory(p) {
			file.Del(p)
			log.Error(fmt.Sprintf("Deleted %v\n", p))
		}
    all = append(all, fname)
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
			f := path.Cat(dir, e)
			file.Del(f)
			log.Error(fmt.Sprintf("Deleted %v\n", f))
			continue
		}
		date := e[:len(e)-4]
		_, ok := time.FromStrOp(date)
		if !ok {
			f := path.Cat(dir, e)
			file.Del(f)
			log.Error(fmt.Sprintf("Deleted %v\n", f))
			continue
		}
		if date < "20190101" || date > now {
			f := path.Cat(dir, e)
			file.Del(f)
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
        f := path.Cat(dir, tgz+".tgz")
        file.Del(f)
      }
    }
  } else {
    nowD := time.ToStr(time.AddDays(dnow, -7))
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
      f := path.Cat(dir, e+".tgz")
      file.Del(f)
    }
  }
}

func backup(source, target string) (ok bool) {
	fileName := time.ToStr(time.Now()) + ".tgz"
	tmpDir := file.Tmp("/tmp", "MrBackup")
  file.Mkdir(tmpDir)
	defer file.Del(tmpDir)

	tmpFile := path.Cat(tmpDir, fileName)
	if e := tar(source, tmpFile); e != nil {
		log.Error(fmt.Sprintln(e))
		return
	}

	for _, pool := range cts.MrBackupTargets {
		poolDir := path.Cat(pool, target)
		file.Copy(tmpFile, poolDir)
		clean(poolDir)
	}

	ok = true
	return
}

func shouldUpdate(dir string, tm int64) bool {
	for _, fname := range file.Dir(dir) {
		p := path.Cat(dir, fname)
		if file.IsDirectory(p) {
			if shouldUpdate(p, tm) {
				return true
			}
			continue
		}
		if file.IsRegular(p) && file.Tm(p) > tm {
			return true
		}
	}
	return false
}

func updateDir(pool, dir string) {
	pdir, ok := readPathTxt(pool, dir)
	if !ok {
		f := path.Cat(pool, dir)
		if !file.IsDirectory(f) {
			file.Del(f)
			log.Error(fmt.Sprintf("Deleted %v\n", f))
		}

		return // skip directory without 'path.txt'
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
	for _, dir := range file.Dir(pool) {
		updateDir(pool, dir)
	}
	log.Info("Bakups updated")
}
