// Copyright 05-Sep-2020 ºDeme
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
  "io"
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

//  Copy a file.
//    source: Can be a regular file or a directory.
//    target: - If source is a directory, target must be the target parent
//              directory.
//            - If source is a regular file, target can be the target parent
//              directory or a regular file.
//  NOTE: Target will be overwritte if already exists.
func copy(source, target string) (err error) {
	defer func() {
		if e := recover(); e != nil {
			switch e.(type) {
			case error:
				err = e.(error)
			default:
				err = fmt.Errorf("Fail copying '%v' to '%v'", source, target)
			}
		}
	}()

	if file.IsDirectory(source) {
    p := path.Join(target, path.Base(source))
    if file.Exists(p) {
      if !file.IsDirectory(p) {
        err = fmt.Errorf(
          "Copying '%v' to '%v', when the later exists and is not a directory",
          source, p,
        )
        return
      }
      file.Remove(p)
    }
    file.Mkdirs(p)

    for _, e := range file.List(source) {
      if err = copy(path.Join(source, e.Name()), p); err != nil {
        return
      }
    }
    return
	}

  if file.IsDirectory(target) {
    target = path.Join(target, path.Base(source))
  }

	sourcef, err := os.Open(source)
	if err != nil {
		return
	}

	targetf, err := os.Create(target)
	if err != nil {
    return
  }

	defer sourcef.Close()
	defer targetf.Close()

	buf := make([]byte, 8192)
	for {
    var n int
		n, err = sourcef.Read(buf)
		if err != nil && err != io.EOF {
			return
		}
		if n == 0 {
      err = nil
			break
		}

		if _, err = targetf.Write(buf[:n]); err != nil {
      return
		}
	}

  return
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

  if e := copy(tmpFile, "/home/deme/tmp"); e != nil {
		log.Error(fmt.Sprintln(e))
		return
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

func update() {
	log.Info("Updating backups...")
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

		if shouldUpdate(pdir, lastTime) {
			log.Info(dir + ": Updating...")
			if backup(pdir, dir) {
				log.Info(dir + ": Updated.")
			} else {
				log.Error(dir + ": Updating FAILED!!!")
			}
		}
	}
	log.Info("Bakups updated")
}
