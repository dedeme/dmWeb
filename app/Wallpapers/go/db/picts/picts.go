// Copyright 29-Apr-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Pictures data base.
package picts

import (
	"github.com/dedeme/Wallpapers/data/cts"
	"github.com/dedeme/Wallpapers/data/pict"
	"github.com/dedeme/Wallpapers/data/sighter"
	"github.com/dedeme/golib/file"
	"github.com/dedeme/golib/json"
	"io/ioutil"
	"path"
	"strconv"
	"strings"
)

var dir string

func fpath(group string) string {
	return path.Join(dir, "picts"+group+".db")
}

// Initialize data base
func Initialize(parentDir string) {
	dir = parentDir
}

func GetGroups() (r []string) {
	for i := 0; i < cts.PICTURE_GROUPS; i++ {
		r = append(r, strconv.Itoa(i))
	}
	return
}

func update(group string) {
	p := fpath(group)
	if !file.Exists(p) {
		Write(group, []*pict.T{})
	}

	var oldPictures []*pict.T
	var newPictures []*pict.T
	for _, pictureJs := range json.FromString(file.ReadAll(p)).Ra() {
		oldPictures = append(oldPictures, pict.FromJs(pictureJs))
	}
	for _, pictureId := range readPictList(group) {
		ok := false
		for _, picture := range oldPictures {
			if picture.Id() == pictureId {
				newPictures = append(newPictures, picture)
				ok = true
				break
			}
		}
		if !ok {
			newPictures = append(newPictures, pict.New(pictureId))
		}
	}

	Write(group, newPictures)
}

func readPictList(group string) []string {
	var pictures []string
	infs, err := ioutil.ReadDir(path.Join("/dm/fondosEscritorio/jpg", group))
	if err != nil {
		panic(err)
	}

	for _, inf := range infs {
		if strings.HasSuffix(inf.Name(), ".jpg") {
			pictures = append(pictures, inf.Name())
		}
	}

	if len(pictures) == 0 {
		panic("Pictures not found in group " + group)
	}

	return pictures
}

// Returns pictures of a group.
func ReadJs(group string) json.T {
	update(group)
	return json.FromString(file.ReadAll(fpath(group)))
}

// Returns picture groups.
func Read(group string) []*pict.T {
	var pictures []*pict.T
	for _, pictureJs := range ReadJs(group).Ra() {
		pictures = append(pictures, pict.FromJs(pictureJs))
	}
	return pictures
}

// Write picture Groups
func Write(group string, pictures []*pict.T) {
	var picturesJs []json.T
	for _, picture := range pictures {
		picturesJs = append(picturesJs, picture.ToJs())
	}

	file.WriteAll(fpath(group), json.Wa(picturesJs).String())
}

// Set level of picture 'group'-'id'.
func SetLevel(group string, id string, level int) {
	pictures := pict.SetLevel(Read(group), id, level)
	Write(group, pictures)
}

// Returns the next picture
func Next() (group string, picture *pict.T) {
	gr, p := sighter.Next(
		GetGroups,
		func(group string) []sighter.T {
			return pict.ToSighters(Read(group))
		},
		func(group string, ss []sighter.T) {
			Write(group, pict.FromSighters(ss))
		},
	)
	group = gr
	picture = p.(*pict.T)
	return
}

// Return total sights and shown sights.
func ShownSights() (totalSights, shownSights int) {
	for _, g := range GetGroups() {
		for _, p := range Read(g) {
			totalSights += p.Level()
			shownSights += p.Sights()
		}
	}
	return
}
