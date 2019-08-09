// Copyright 26-Jul-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import Domo from "../dmjs/Domo.js"; //eslint-disable-line
import Ui from "../dmjs/Ui.js";
import Dec from "../dmjs/Dec.js";
import {Mark} from "../Calc.js"; //eslint-disable-line
import {PreviousGroup} from "../Data.js"; //eslint-disable-line
import {_} from "../I18n.js";

const $ = Ui.$;

class Row {
  /**
   * @param {!Mark} mark
   * @param {!Domo} img
   */
  constructor (mark, img) {
    this.mark = mark;
    this.img = img;
  }
}

export default class TRanking {
  /**
   * @param {number} division
   * @param {!PreviousGroup} previousG
   * @param {!Array<!Mark>} current
   */
  constructor (division, previousG, current) {
    const previous = (division === 1) ? previousG.first
      : (division === 2) ? previousG.second : previousG.third;

    this._rows = current.map((m, mi) => {
      let dif = null;
      for (let pi = 0; pi < previous.length; ++pi) {
        if (previous[pi] === m.nick) {
          dif = pi - mi;
          break;
        }
      }

      let img = Ui.img("eq");
      if (dif === null) {
        if (division === 1) {
          img = Ui.led("#4050a0");
        } else if (division === 3) {
          img = Ui.led("#a05040");
        } else if (previousG.first.includes(m.nick)) {
          img = Ui.led("#a05040");
        } else {
          img = Ui.led("#4050a0");
        }
      } else if (dif > 3) {
        img = Ui.img("up2");
      } else if (dif > 0) {
        img = Ui.img("up");
      } else if (dif < -3) {
        img = Ui.img("down2");
      } else if (dif < 0) {
        img = Ui.img("down");
      }
      return new Row(m, img);
    });
    this._len = previous.length;
  }

  /** @return {!Domo} */
  get wg () {
    const len = this._len;
    const padding = "padding-left: 4px;padding-right: 4px;";

    return $("table").klass("border").style("border-collapse : collapse;")
      .add($("tr")
        .add($("td").klass("header"))
        .add($("td").klass("header").html(_("Nick")))
        .add($("td").klass("header").html(_("Points")))
        .add($("td").klass("header").style("text-align:right").html(_("Dif"))))
      .adds(this._rows.map((row, i) => {
        const fmt = i < 3 ? padding + "background-color: rgb(180, 190, 200);"
          : i >= len - 3 ? padding + "background-color: rgb(200, 190, 180);"
            : padding
        ;
        const m = row.mark;
        return $("tr")
          .add($("td").style(fmt + "border-right: 1px solid rgb(110,130,150);")
            .add(row.img))
          .add($("td").style(fmt).klass("nick").html(m.nick))
          .add($("td")
            .style(
              fmt +
              "text-align:right;" +
              "border-left: 1px solid rgb(110,130,150);" +
              "border-right: 1px solid rgb(110,130,150);"
            ).html(m.points))
          .add($("td").style(fmt + "text-align:right")
            .html(new Dec(m.dif, 4).toEu()))
        ;
      }));
  }
}
