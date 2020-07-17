// Copyright 13-Jul-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// El Confidencial reader
package elconfidencial

import (
	"fmt"
	"github.com/dedeme/News/data/ev"
	"github.com/dedeme/News/data/newsEntry"
	"github.com/dedeme/News/data/webPage"
	"github.com/dedeme/News/net/reader"
	"github.com/dedeme/golib/date"
	"strings"
)

func pgStart(pg string) int {
	return 0
}

func newRead(
	wpg *webPage.T, pg string,
) (e *newsEntry.T, rest string) {
	source := wpg.SourceId
	author := source

	e = nil
	p := strings.Index(pg, "class=\"art-tit fs-")
	if p == -1 {
		return
	}
	pg = pg[p+1:]
	rest = pg
	p = strings.Index(pg, " title=\"")
	if p == -1 {
		return
	}
	pg = pg[p+8:]
	p = strings.Index(pg, "\"")
	if p == -1 {
		return
	}
	text := pg[:p]
	pg = pg[p:]
	p = strings.Index(pg, "href=\"")
	if p == -1 {
		return
	}
	pg = pg[p+6:]
	p = strings.Index(pg, "\"")
	if p == -1 {
		return
	}
	url := pg[:p]
	pg = pg[p:]
  rest = pg
  p = strings.Index(pg, "journalist>")
  if p != -1 {
    pg = pg[p+11:]
    p = strings.Index(pg, "<")
    if p!=-1 {
      author = pg[:p]
      rest = pg[p+1:]
    }
  }

	e = &newsEntry.T{
		Date:     date.Now(),
		Text:     text,
		Source:   source,
		Author:   author,
		Url:      url,
		Seval:    0.5,
		Aeval:    0.5,
		Weval:    0.5,
		UserEval: ev.NONE,
	}

	return
}

func Read(wpg *webPage.T) []*newsEntry.T {
	url := wpg.Url
	var r []*newsEntry.T
	pg := reader.ReadPage(url)
	start := pgStart(pg)
	if start < 0 {
		panic(fmt.Sprintf("Start of page not found (%v)", url))
	}

	pg = pg[start+1:]
	var e *newsEntry.T
	for {
		e, pg = newRead(wpg, pg)
		if pg == "" {
			break
		}
		if e != nil {
			r = append(r, e)
		}
	}

	return r
}
