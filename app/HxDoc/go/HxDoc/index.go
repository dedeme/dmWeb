// Copyright 22-Jun-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Main javascript page
package main

import (
	"github.com/dedeme/golib/cgi"
	"github.com/dedeme/golib/file"
	"github.com/dedeme/golib/json"
	"path"
	"strings"
  "sort"
)

type indexFileEntry = struct {
  id string;
  doc string;
}

func indexReadFile(pt string) *indexFileEntry {
  base := path.Base(pt)
	doc := ""

  file.Lines(pt, func(l string) bool {
		l = strings.TrimSpace(l)
    if strings.HasPrefix(l, "///") {
      l = strings.TrimSpace(l[3:])
      ix := strings.IndexByte(l, '.')
      if ix == -1 {
        doc = l
      } else {
        doc = l[0: ix + 1]
      }
      return true
    }
    return false
	})

	return &indexFileEntry{base[0 : len(base)-3], doc}
}

func indexReadDir(level int, prefix, pt string) [][]string {
  tabs := strings.Repeat("&nbsp;&nbsp;&nbsp;&nbsp;", level);

	var r [][]string
	var files []*indexFileEntry
	var dirs []string

	for _, d := range file.List(pt) {
		name := d.Name()
		newPath := path.Join(pt, name)
		if file.IsDirectory(newPath) {
			dirs = append(dirs, newPath)
		} else if strings.HasSuffix(name, ".hx") {
			files = append(files, indexReadFile(newPath))
		}
	}

  sort.Slice(files, func(i, j int) bool {
    return strings.ToUpper(files[i].id) < strings.ToUpper(files[j].id)
  })
  for _, f := range files {
    p := f.id
    if prefix != "" {
      p = path.Join(prefix, f.id)
    }
    r = append(r, []string{tabs + f.id, p, f.doc})
  }

  sort.Slice(dirs, func(i, j int) bool {
    return strings.ToUpper(dirs[i]) < strings.ToUpper(dirs[j])
  })
  for _, d := range dirs {
    id := path.Base(d);
    p := id
    if prefix != "" {
      p = path.Join(prefix, id)
    }
    r = append(r, []string{tabs + id, "", ""})
    r = append(r, indexReadDir(level + 1, p, d)...)
  }

	return r
}

func indexProcess(ck string, mrq map[string]json.T) string {
	path := cgi.RqString(mrq, "path")
	var list []json.T
	for _, e := range indexReadDir(0, "", path) {
		var ejs []json.T
		for _, f := range e {
			ejs = append(ejs, json.Ws(f))
		}
		list = append(list, json.Wa(ejs))
	}
	return cgi.Rp(ck, map[string]json.T{"entries": json.Wa(list)})
}
