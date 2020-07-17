// Copyright 09-Jul-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Web page reader
package net

import (
	"errors"
	"fmt"
	"github.com/dedeme/News/data/newsEntry"
	"github.com/dedeme/News/data/webPage"
	"github.com/dedeme/News/net/elconfidencial"
	"github.com/dedeme/News/net/eldiario"
	"github.com/dedeme/News/net/meneame"
	"github.com/dedeme/News/net/publico"
)

// Reads a Web page.
func Read(pg *webPage.T) (news []*newsEntry.T, err error) {
	defer func() {
		if er := recover(); er != nil {
			err = errors.New(fmt.Sprint(er))
		}
	}()

	if pg.SourceId == "ElDiario" {
		news = eldiario.Read(pg)
		return
	}
	if pg.SourceId == "ElConfidencial" {
		news = elconfidencial.Read(pg)
		return
	}
	if pg.SourceId == "Publico" {
		news = publico.Read(pg)
		return
	}
	if pg.SourceId == "Meneame" {
		news = meneame.Read(pg)
		return
	}

	panic(fmt.Sprintf("Unkown page '%v'", pg.Url))
	return
}
