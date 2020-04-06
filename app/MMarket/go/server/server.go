// Copyright 04-Apr-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Main entry of communications server
package server

import (
	"context"
	"fmt"
	"github.com/dedeme/MMarket/data/cts"
	"github.com/dedeme/MMarket/db/log"
	"github.com/dedeme/MMarket/server/hub"
	liblog "github.com/dedeme/golib/log"
	"io/ioutil"
	"net/http"
)

var stoped bool

type actorHandler struct {
	srv   *http.Server
	actor chan (func())
}

func (h *actorHandler) ServeHTTP(w http.ResponseWriter, r *http.Request) {
	rqb, _ := ioutil.ReadAll(r.Body)
	rq := string(rqb)

	switch rq {
	case "test":
		fmt.Fprintf(w, "ok")
	case "end":
		fn := func() {
			stoped = true
			if err := h.srv.Shutdown(context.Background()); err != nil {
				// Error from closing listeners, or context timeout:
				fmt.Fprintf(w, "HTTP server Shutdown: %v", err)
				log.Error(fmt.Sprintf("HTTP server Shutdown: %v", err))
				<-h.actor // receive here beacause sever was not stopped
			}
			log.Info("Server stoped")
		}
		h.actor <- fn // Receive in Run, after call Server to start
	default:
		fmt.Fprintf(w, process(h.actor, rq))
	}
}

func process(ac chan (func()), rq string) (rp string) {
	defer func() {
		if err := recover(); err != nil {
			msg := fmt.Sprint(err)
			log.Error(msg)
			rp = msg
		}
	}()

	rp = hub.Process(ac, rq)
	return
}

func Run(ac chan (func())) {
	srv := &http.Server{
		Addr: ":" + cts.PORT,
	}

	http.Handle("/", &actorHandler{srv, ac})
	if err := srv.ListenAndServe(); err != http.ErrServerClosed {
		// Error starting or closing listener:
		liblog.Fatalf("HTTP server ListenAndServe: %v", err)
	}
	<-ac // Receive after shutdown
}

func Stoped() bool {
	return stoped
}
