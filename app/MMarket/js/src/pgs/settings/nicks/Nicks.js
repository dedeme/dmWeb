// Copyright 09-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/**
    Nicks page.
**/

import Maybe from "../../../dmjs/Maybe.js";
import Ui from "../../../dmjs/Ui.js";
import Domo from "../../../dmjs/Domo.js";  //eslint-disable-line
import {Menu, MenuEntry} from "../../../dmjs/Menu.js";
import {_, _args} from "../../../I18n.js";
import Cts from "../../../data/Cts.js";
import Msg from "../../../wgs/Msg.js";
import Nick from "../../../data/Nick.js";
import List from "./List.js";
import Editor from "./Editor.js";

const $ = e => Ui.$(e);

/**
    @private
    @param {!Array<!Nick>} list
    @return {string}
**/
function mkStatistics (list) {
  const total = list.length;
  let sel = 0;
  list.forEach(nk => { if (nk.isSel) ++sel; });
  return _args(_("Total: %0. Selected: %1."), String(total), String(sel));
}

/**
    Nicks page.
**/
export default class Nicks {

  /**
      @param {!Domo} wg
      @param {number} model
      @param {!Array<!Nick>} nicks
      @param {!Object<string, number>} volumes
  **/
  constructor (wg, model, nicks, volumes) {
    this._wg = wg;
    this._model = model;
    this._nicks = nicks;
    this._volumes = volumes;
    this._withVolume = true;
    this._selectedNick = Maybe.nothing;

    this.view();
  }

  // View ----------------------------------------------------------------------

  /**
    @private
    @return void
  **/
  view () {
    const lopts = [
      new MenuEntry(
        Maybe.nothing, $("span").text(mkStatistics(this._nicks))
      )
    ];
    const ropts = [
      Menu.toption("volume", _("Volume"), () => this.list(true)),
      Menu.separator(),
      Menu.toption("list", _("List"), () => this.list(false))
    ];
    const menuSelected = this._selectedNick.isNothing()
      ? this._withVolume ? "volume" : "list"
      : ""
    ;
    const menu = new Menu(lopts, ropts, menuSelected);

    const inputDiv = $("div");
    const menuDiv = $("div");
    const bodyDiv = $("div");

    this._selectedNick.ifElse(
      nick => {
        let model = this._nicks.find(e => e.id === this._model);
        if (model === undefined) model = this._nicks[0];
        Editor.mk(
          inputDiv, menuDiv, bodyDiv, this._nicks, nick, model
        ).then(ed => {
          if (ed.isNothing()) {
            this._selectedNick = Maybe.nothing;
            Nicks.mk(this._wg);
          }
        });
      },
      () => {
        // eslint-disable-next-line
        new List(
          inputDiv,
          menuDiv,
          bodyDiv,
          this,
          this._model,
          this._nicks,
          this._volumes,
          this._withVolume
        );
      }
    );

    this._wg
      .removeAll()
      .add($("table")
        .klass("main")
        .add($("tr")
          .add($("td")
            .style("width:5px")
            .att("rowspan", 2)
            .add(inputDiv))
          .add(menu.wg))
        .add($("tr")
          .add($("td")
            .add(menuDiv)))
        .add($("tr")
          .add($("td")
            .att("colspan", 2)
            .add(bodyDiv))))
    ;

  }

  // Control -------------------------------------------------------------------

  /**
    @private
    @param {boolean} withVolume
    @return void
  **/
  list (withVolume) {
    this._withVolume = withVolume;
    this._selectedNick = Maybe.nothing;
    this.view();
  }

  /**
    @param {!Nick} nick
    @return void
  **/
  selectNick (nick) {
    this._selectedNick = Maybe.just(nick);
    this.view();
  }

  /**
    @param {string} nickName
    @return void
  **/
  async addNick (nickName) {
    if (nickName === "") {
      Msg.error(_("Nick name is missing"));
      return;
    }
    if (this._nicks.some(e => e.name === nickName)) {
      Msg.error(_args(_("Nick '%0' is duplicated"), nickName));
      return;
    }

    const rp = await Cts.client.ssend({
      "module": "settings",
      "source": "nicks",
      "rq": "add",
      "nickName": nickName
    });
    if (!rp["ok"]) {
      Msg.error(Cts.failMsg);
    }
    Nicks.mk(this._wg);
  }

  /**
    @param {!Nick} nick
    @return void
  **/
  async setModel (nick) {
    await Cts.client.ssend({
      "module": "settings",
      "source": "nicks",
      "rq": "setModel",
      "id": nick.id
    });
    Nicks.mk(this._wg);
  }

  /**
    @param {!Nick} nick
    @return void
  **/
  async setIsSel (nick) {
    await Cts.client.ssend({
      "module": "settings",
      "source": "nicks",
      "rq": "setIsSel",
      "id": nick.id,
      "value": !nick.isSel
    });
    Nicks.mk(this._wg);
  }

  /**
    @param {!Nick} nick
    @return void
  **/
  async del (nick) {
    if (!confirm(_args(_("Delete '%0'?"), nick.name))) {
      return;
    }
    await Cts.client.ssend({
      "module": "settings",
      "source": "nicks",
      "rq": "del",
      "id": nick.id
    });
    Nicks.mk(this._wg);
  }

  // Static --------------------------------------------------------------------

  /**
      @param {!Domo} wg
      @return {!Promise<!Nicks>}
  **/
  static async mk (wg) {
    const rp = await Cts.client.ssend({
      "module": "settings",
      "source": "nicks",
      "rq": "idata"
    });
    const /** number **/ model =
      rp["model"];
    const /** !Array<!Nick> */ nicks =
      rp["nicks"].map(e => Nick.fromJs(e));
    const /** !Object<string, number> */ volumes =
      rp["volumes"];
    return new Nicks(wg, model, nicks, volumes);
  }
}
