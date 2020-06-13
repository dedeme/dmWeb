// Copyright 21-May-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import Domo from "../../../dmjs/Domo.js"; //eslint-disable-line
import Client from "../../../dmjs/Client.js"; //eslint-disable-line
import Ui from "../../../dmjs/Ui.js";
import Maybe from "../../../dmjs/Maybe.js";
import {_} from "../../../I18n.js";
import Cts from "../../../data/Cts.js";
import Msg from "../../../wgs/Msg.js";
import Flog from "../../../data/flea/Flog.js";
import Fmodel from "../../../data/flea/Fmodel.js"; //eslint-disable-line
import LogRow from "../../../data/LogRow.js";

const $ = e => Ui.$(e);

/**
    Fleas Tests page.
**/
export default class Selection {
  /**
      @private
      @param {!Domo} wg
      @param {!Fmodel} model
      @param {!Flog} log
  **/
  constructor (wg, model, log) {
    this._wg = wg;
    this._model = model;
    this._log = Maybe.just(log);

    this._timer = Maybe.nothing;
    this._textArea = $("textarea")
      .att("spellcheck", false)
      .att("readOnly", true)
      .att("rows", 25)
      .att("cols", 120)
    ;
    this.view();
  }

  // View ----------------------------------------------------------------------

  /**
      @private
  **/
  view () {
    this._wg
      .removeAll()
      .add($("table")
        .att("align", "center")
        .add($("tr")
          .add($("td")
            .add(Ui.link(() => { this.start() })
              .klass("link")
              .text(_("Start")))
            .add($("span")
              .html("&nbsp;&nbsp;&nbsp;&nbsp;"))
            .add(Ui.link(() => { this.stop() })
              .klass("link")
              .text(_("Stop")))))
        .add($("tr")
          .add($("td")
            .add(this._textArea)))
      );
  }

  // Control -------------------------------------------------------------------

  async start () {
    const self = this;
    if (self._log.isNothing()) {
      alert(_("Play ground stoped.\nRestart doing click in 'Selection'."));
      return;
    }
    async function loop () {
      const lid = self._log.isNothing() ? "" : self._log.fromJust().id;
      const rp = await Cts.client.send({
        "module": "fleas",
        "source": "ftests/selection",
        "rq": "continue",
        "logId": lid
      });
      const log = Maybe.fromJs(rp["log"]);
      if (log.isNothing()) {
        self._log = Maybe.nothing;
        if (self._timer.isJust()) clearInterval(self._timer.fromJust());
        self._timer = Maybe.nothing;
        self._textArea.value("End process.\n" + self._textArea.value());
      } else {
        const l = log.fromJust();
        self._textArea.value(
          l.map(e => LogRow.fromJs(e).format(115)).join("\n")
        );
        self.view();
      }
    }

    const rp = await Cts.client.send({
      "module": "fleas",
      "source": "ftests/selection",
      "rq": "start",
      "modelId": this._model.id,
      "logId": self._log.fromJust().id,
    });
    if (!rp["ok"]) {
      Msg.error(Cts.failMsg, () => {});
    } else {
      await loop();
      this._timer = Maybe.just(setInterval(loop, 2000));
    }
  }

  async stop () {
    if (this._timer.isNothing() || this._log.isNothing()) return;

    if (confirm(_("Stop process?"))) {
      await Cts.client.send({
        "module": "fleas",
        "source": "ftests/selection",
        "rq": "stop",
        "logId": this._log.fromJust().id
      });
      this._log = Maybe.nothing;
    }
  }

  // Static --------------------------------------------------------------------

  /**
      @param {!Domo} wg
      @param {!Fmodel} model
      @return {!Promise<!Selection>}
  **/
  static async mk (wg, model) {
    const rp = await Cts.client.send({
      "module": "fleas",
      "source": "ftests/selection",
      "rq": "logId"
    });
    const /** string */ id = rp["logId"];
    return new Selection(wg, model, new Flog(id, []));
  }

}


