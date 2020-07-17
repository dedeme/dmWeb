// Copyright 09-Jul-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// News tables
package newss

import (
	"github.com/dedeme/News/data/cts"
	"github.com/dedeme/News/data/newsEntry"
	"github.com/dedeme/golib/file"
	"github.com/dedeme/golib/json"
	"path"
)

func add(p string, old, news []*newsEntry.T) {
	for _, e := range news {
		isNew := true
		for _, e2 := range old {
			if e.Url == e2.Url {
				isNew = false
				break
			}
		}
		if isNew {
			old = append(old, e)
		}
	}
	var a []json.T
	for _, e := range old {
		a = append(a, e.ToJs())
	}
	file.WriteAll(p, json.Wa(a).String())
}

// Local -----------------------------------------------------------------------

func localPath() string {
  return path.Join(cts.LOCAL_DATA, "Newss.tb")
}

// Initializes and reset Local data base.
func LocalInit() {
	file.WriteAll(localPath(), "[]")
}

// Returns local news when called locally.
func LocalRead() []*newsEntry.T {
	var r []*newsEntry.T
	for _, e := range json.FromString(file.ReadAll(localPath())).Ra() {
		r = append(r, newsEntry.FromJs(e))
	}
	return r
}

// Returns local news when called from Web.
func LocalReadJs() json.T {
	if !file.Exists(localPath()) {
		panic("newLocalPath is not initialized. Call 'News run'")
	}
	return json.FromString(file.ReadAll(localPath()))
}

// Writes local news.
func LocalWrite(news []*newsEntry.T) {
	var r []json.T
  for _, e := range news {
    r = append(r, e.ToJs())
  }
  file.WriteAll(localPath(), json.Wa(r).String())
}

// Adds local news.
func LocalAdd(news []*newsEntry.T) {
	add(localPath(), LocalRead(), news)
}

// Web -------------------------------------------------------------------------

func webPath() string {
  return path.Join(cts.WEB_DATA, "Newss.tb")
}

// Initializes Web data base.
func WebInit() {
  if !file.Exists(webPath()) {
    file.WriteAll(webPath(), "[]")
  }
}

// Resets Web data base and initializes with local news..
func WebReset() {
  local := LocalRead()
  web := WebRead()
  for _, l := range local {
    for _, w := range web {
      if l.Url == w.Url {
        l.UserEval = w.UserEval
        break
      }
    }
  }
  WebWrite(local)
}

// Returns web news.
func WebRead() []*newsEntry.T {
	var r []*newsEntry.T
	for _, e := range json.FromString(file.ReadAll(webPath())).Ra() {
		r = append(r, newsEntry.FromJs(e))
	}
	return r
}

// Returns web news.
func WebReadJs() json.T {
	return json.FromString(file.ReadAll(webPath()))
}

// Writes web news.
func WebWrite(news []*newsEntry.T) {
	var r []json.T
  for _, e := range news {
    r = append(r, e.ToJs())
  }
  file.WriteAll(webPath(), json.Wa(r).String())
}

// Write web news
func WebAdd(news []*newsEntry.T) {
	add(webPath(), WebRead(), news)
}

// Modify entry if exists
func Modify(entry *newsEntry.T) {
  es := WebRead()
  for _, e := range es {
    if e.Url == entry.Url {
      e.UserEval = entry.UserEval
      break
    }
  }
  WebWrite(es)
}
