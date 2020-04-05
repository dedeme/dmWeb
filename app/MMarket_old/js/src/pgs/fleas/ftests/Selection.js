// Copyright 10-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/**
    Fleas selection tests page.
**/

import Domo from "../../../dmjs/Domo.js"; //eslint-disable-line
import Client from "../../../dmjs/Client.js"; //eslint-disable-line
import Ui from "../../../dmjs/Ui.js";
import Maybe from "../../../dmjs/Maybe.js";
import {_} from "../../../I18n.js";
import Flog from "../../../data/flea/Flog.js";
import Fmodel from "../../../data/flea/Fmodel.js"; //eslint-disable-line
import LogEntry from "../../../data/LogEntry.js";

const $ = e => Ui.$(e);

/**
    Fleas Tests page.
**/
export default class Selection {
  /**
      @private
      @param {!Client} client
      @param {!Fmodel} model
      @param {!Flog} log
  **/
  constructor (client, model, log) {
    this._client = client;
    this._model = model;
    this._log = log;

    this._timer = Maybe.nothing;
    this._textArea = $("textarea").att("spellcheck", false)
      .att("readOnly", true)
      .att("rows", 25).att("cols", 85);
    this._wg = $("div");
    this.view();
  }

  /**
      @return {!Domo}
  **/
  get wg () {
    return this._wg;
  }

  // View ----------------------------------------------------------------------

  /**
      @private
  **/
  view () {
    this._wg.removeAll()
      .add($("table").att("align", "center")
        .add($("tr")
          .add($("td")
            .add(Ui.link(() => { this.start() }).klass("link")
              .text(_("Start")))
            .add($("span").html("&nbsp;&nbsp;&nbsp;&nbsp;"))
            .add(Ui.link(() => { this.stop() }).klass("link")
              .text(_("Stop")))))
        .add($("tr")
          .add($("td")
            .add(this._textArea)))
      );
  }

  // Control -------------------------------------------------------------------

  async start () {
    const self = this;
    async function loop () {
      const rp = await self._client.send({
        "module": "Fleas",
        "source": "Selection",
        "rq": "continue",
        "logId": self._log.id
      });
      const log = Maybe.fromJs(rp["log"]);
      if (log.isNothing()) {
        if (self._timer.isJust()) clearInterval(self._timer.fromJust());
        self._timer = Maybe.nothing;
        self._textArea.value("End process.\n" + self._textArea.value());
      } else {
        self._textArea.value(
          log.fromJust().map(e => LogEntry.fromJs(e).toString()).join("\n")
        );
        self.view();
      }
    }

    const rp = await self._client.send({
      "module": "Fleas",
      "source": "Selection",
      "rq": "start",
      "modelId": this._model.id,
      "logId": self._log.id,
      "error1": _("Model '%0' not found")
    });
    if (rp["error"] !== "") {
      alert(rp["error"]);
    } else {
      await loop();
      this._timer = Maybe.just(setInterval(loop, 2000));
    }
  }

  async stop () {
    if (this._timer.isNothing()) return;

    if (confirm(_("Stop process?"))) {
      await this._client.send({
        "module": "Fleas",
        "source": "Selection",
        "rq": "stop",
        "logId": this._log.id
      });
      clearInterval(this._timer.fromJust());
      this._timer = Maybe.nothing;
      this.view();
    }
  }

  // Static --------------------------------------------------------------------

  /**
      @param {!Client} client
      @param {!Fmodel} model
      @return {!Promise<!Selection>}
  **/
  static async mk (client, model) {
    const rp = await client.send({
      "module": "Fleas",
      "source": "Selection",
      "rq": "logId"
    });
    const id = rp["logId"];
    return new Selection(client, model, new Flog(id, []));
  }

}


