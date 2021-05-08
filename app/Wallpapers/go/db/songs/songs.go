// Copyright 07-May-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Songs data base.
package songs

import (
	"github.com/dedeme/Wallpapers/data/song"
	"github.com/dedeme/golib/file"
	"github.com/dedeme/golib/json"
	"github.com/dedeme/golib/sys"
	"io/ioutil"
	"path"
	"strconv"
	"strings"
)

var fpath string // Path of data base
var fcurrent string // Path of current song
var opath string // Path of original songs

// Initialize data base
func Initialize(parentDir string) {
	fpath = path.Join(parentDir, "songs.db")
  fcurrent = path.Join(parentDir, "currentSong", "song.mp3")
	opath = "/dm/musica/relax"

	if !file.Exists(fpath) {
		var ss []json.T
		file.WriteAll(fpath, json.Wa(ss).String())
	}
}

func update() {
	original := readSongList()
	var data []*song.T
	for _, e := range json.FromString(file.ReadAll(fpath)).Ra() {
		data = append(data, song.FromJs(e))
	}
	var newData []*song.T
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
			out, _ := sys.Cmd("ffprobe", "-show_entries", "format=duration",
				"-i", path.Join(opath, o))

			s := string(out)
			start := strings.Index(s, "duration=") + 9
			end := strings.Index(s, ".")
			time, err := strconv.Atoi(s[start:end])
			if start == -1 || end == -1 || err != nil || end <= start {
				continue
			}
			newData = append(newData, song.New(o, time))
		}
	}
	Write(newData)
}

func readSongList() []string {
	var r []string
	infs, err := ioutil.ReadDir(opath)
	if err != nil {
		panic(err)
	}

	for _, inf := range infs {
		if strings.HasSuffix(inf.Name(), ".mp3") {
			r = append(r, inf.Name())
		}
	}

	return r
}

// Returns song list.
func ReadJs() json.T {
	update()
	return json.FromString(file.ReadAll(fpath))
}

// Returns song list.
func Read() (r []*song.T) {
	for _, e := range ReadJs().Ra() {
		r = append(r, song.FromJs(e))
	}
	return
}

// Write song list
func Write(ss []*song.T) {
	var a []json.T
	for _, e := range ss {
		a = append(a, e.ToJs())
	}
	file.WriteAll(fpath, json.Wa(a).String())
}

// Set level of song 'id'.
func SetLevel(id string, level int) {
	Write(song.SetLevel(Read(), id, level))
}

