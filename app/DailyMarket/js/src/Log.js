// Copyright 05-Sept-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

//eslint-disable-next-line
import Main from "./Main.js";
import {_} from "./I18n.js";
import Ui from "./dmjs/Ui.js";

const $ = Ui.$;

/** Update page. */
export default class Log {
  /**
   * @param {!Main} main Main
   */
  constructor (main) {
    /**
     * @private
     * @type {!Main}
     */
    this._main = main;

    this._button = $("div").style("padding-bottom:4px");

    this._showWarnings = false;

    this._frame = $("textArea").att("cols", 110).att("rows", 30)
      .att("readOnly", true);
  }

  /** @private */
  showData () {
    function ftx (msg, rest) {
      if (rest.length === 0) {
        return msg + "\n";
      }
      if (rest.length <= 80) {
        return ftx(msg + rest, "");
      }
      return ftx(
        msg + rest.substring(0, 80) + "\n" + "                      ",
        rest.substring(80)
      );
    }
    function format (entry) {
      return entry.date.substring(0, 5) + "(" + entry.hour + ") | " +
        (entry.isError ? "=" : "-") + " | " +
        ftx("", entry.msg);
    }
    let tx = "";
    this._main.data.log.slice(0).reverse()
      .filter(l => this._showWarnings ? true : l.isError)
      .forEach(l => {
        tx += format(l);
      });
    this._frame.value(tx);

    if (this._showWarnings) {
      this._button.removeAll()
        .add(Ui.link(() => {
          this._showWarnings = false;
          this.showData();
        }).add(Ui.img("minus")));
    } else {
      this._button.removeAll()
        .add(Ui.link(() => {
          this._showWarnings = true;
          this.showData();
        }).add(Ui.img("plus")));
    }
  }

  /**
   * @return {void}
   */
  show () {
    this._main.dom.show(Main.summaryPageId, $("div").style("text-align:center;")
      .add($("div").klass("head").style("padding-bottom:4px")
        .html(_("Log")))
      .add(this._button)
      .add(this._frame)
    );
    this.showData();
  }
}

