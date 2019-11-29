// Copyright 28-Nov-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// eslint-disable-next-line
import Client from "../dmjs/Client.js";
import Domo from "../dmjs/Domo.js"; //eslint-disable-line
import Ui from "../dmjs/Ui.js";
// import Dec from "../dmjs/Dec.js";
import Main from "../Main.js";  //eslint-disable-line
import Menu from "../wgs/Menu.js";
import {_} from "../I18n.js";

const $ = e => Ui.$(e);

/** Ranking Main page. */
export default class RankingMain {
  /**
   * @param {!Main} main
   */
  constructor (main) {
    this._main = main;

    this._menu = new Menu(false);

    this._listTd = $("div").klass("frame");
    this._infoTd = $("div");
  }

  /** @return {!Main} */
  get main () {
    return this._main;
  }

  // VIEW ----------------------------------------
  // TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT

  /** @private */
  mkMenu () {
    const menu = this._menu;
    menu.reset();

    menu.addRight(Menu.mkOption(
      "_management_", _("Management"),
      () => location.assign(Main.urlBase)
    ).klass("link"));
    menu.addRight(Menu.separator());
    menu.addRight(Menu.mkOption(
      "_update_", _("Update"),
      () => this.dataUpdate()
    ).klass("link"));
  }

  /** @return {void} */
  show () {
    this.mkMenu();
    this.main.view.removeAll()
      .add(this._menu.wg)
      .add($("table").klass("main")
        .add($("tr")
          .add($("td").style(
            "width: 5px;white-space: nowrap;vertical-align:top"
          ).add(this._listTd))
          .add($("td").style("vertical-align:top").add(this._infoTd))))
    ;

    this.update();
  }

  // CONTROL -------------------------------------
  // TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT

  /**
   * @private
   * @return {void}
   */
  update () {
    alert("Update");
  }

  /**
   * @private
   * @return {!Promise}
   */
  async dataUpdate () {
    const rq = {
      "module": "ranking",
      "rq": "dataUpdate"
    };
    await Main.client.rq(rq);
    this.update();
  }
}
