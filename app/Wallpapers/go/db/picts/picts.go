// Copyright 29-Apr-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Pictures data base.
package picts

import (
	"github.com/dedeme/Wallpapers/data/pict"
	"github.com/dedeme/golib/file"
	"github.com/dedeme/golib/json"
	"io/ioutil"
	"path"
	"strings"
)

var fpath string

// Initialize data base
func Initialize(parentDir string) {
	fpath = path.Join(parentDir, "picts.db")

	if !file.Exists(fpath) {
		var pictures []json.T
		file.WriteAll(fpath, json.Wa(pictures).String())
	}
}

func update() {
	original := readPictList()
	var data []*pict.T
	for _, e := range json.FromString(file.ReadAll(fpath)).Ra() {
		data = append(data, pict.FromJs(e))
	}
	var newData []*pict.T
	for _, o := range original {
		ok := false
		for _, d := range data {
			if d.Id() == o {
				newData = append(newData, d)
				ok = true
				break
			}
		}
		if !ok {
			newData = append(newData, pict.New(o))
		}
	}
	Write(newData)
}

func readPictList() []string {
	var r []string
	infs, err := ioutil.ReadDir("/dm/fondosEscritorio/jpg")
	if err != nil {
		panic(err)
	}

	for _, inf := range infs {
		if strings.HasSuffix(inf.Name(), ".jpg") {
			r = append(r, inf.Name())
		}
	}

	return r
}

// Returns picture list.
func ReadJs() json.T {
	update()
	return json.FromString(file.ReadAll(fpath))
}

// Returns picture list.
func Read() (r []*pict.T) {
	for _, e := range ReadJs().Ra() {
		r = append(r, pict.FromJs(e))
	}
	return
}

// Write picture list
func Write(pictures []*pict.T) {
	var a []json.T
	for _, e := range pictures {
		a = append(a, e.ToJs())
	}
	file.WriteAll(fpath, json.Wa(a).String())
}

// Set level of picture 'id'.
func SetLevel(id string, level int) {
	Write(pict.SetLevel(Read(), id, level))
}
