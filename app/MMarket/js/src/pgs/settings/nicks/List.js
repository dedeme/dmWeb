// Copyright 27-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import Ui from "../../../dmjs/Ui.js";
import Domo from "../../../dmjs/Domo.js";  //eslint-disable-line
import ModalBox from "../../../dmjs/ModalBox.js";
import Nick from "../../../data/Nick.js";  //eslint-disable-line
import {_} from "../../../I18n.js";
import {Menu} from "../../../dmjs/Menu.js";
import Cts from "../../../data/Cts.js";
import Msg from "../../../wgs/Msg.js";
import Wnick from "./wgs/Wnick.js";
import Nicks from "./Nicks.js";  //eslint-disable-line

const $ = e => Ui.$(e);

/**
    Nick list page.
**/
export default class List {
  /**
      @param {!Domo} inputDiv
      @param {!Domo} menuDiv
      @param {!Domo} bodyDiv
      @param {!Nicks} nicksPg
      @param {number} model
      @param {!Array<!Nick>} nicks
      @param {!Object<string, number>} volumes
      @param {boolean} withVolume
  **/
  constructor (
    inputDiv,
    menuDiv,
    bodyDiv,
    nicksPg,
    model,
    nicks,
    volumes,
    withVolume,
  ) {
    this._inputDiv = inputDiv;
    this._menuDiv = menuDiv;
    this._bodyDiv = bodyDiv;
    this._nicksPg = nicksPg;
    this._model = model;
    if (withVolume) {
      nicks.sort((e1, e2) => volumes[e1.name] < volumes[e2.name] ? 1 : -1);
    } else {
      nicks.sort((e1, e2) => e1.name > e2.name ? 1 : -1);
    }
    this._nicks = nicks;
    this._volumes = volumes;
    this._withVolume = withVolume;

    this._msgWait = $("div");
    this._newNick = Ui.field("newBt")
      .style("width:100px")
    ;
    this.view();
  }

  // View ----------------------------------------------------------------------

  /**
      @private
      @return void
  **/
  view () {
    this._inputDiv
      .removeAll()
      .add($("table")
        .klass("home")
        .add($("tr")
          .add($("td")
            .add(this._newNick)))
        .add($("tr")
          .add($("td")
            .style("text-align:center")
            .add($("button")
              .att("id", "newBt")
              .text(_("New"))
              .on("click", () => {
                this._nicksPg.addNick(this._newNick.value().trim());
              })))))
    ;

    const lopts = [];
    const ropts = [
      Menu.toption("download", _("Download"), () => this.download()),
      Menu.separator(),
      Menu.toption("test", _("Test"), () => this.test())
    ];
    const menu = new Menu(lopts, ropts, "");
    this._menuDiv
      .removeAll()
      .add(menu.wg)
    ;

    if (this._nicks.length === 0) {
      this._bodyDiv
        .removeAll()
        .add($("table")
          .att("align", "center")
          .klass("frame")
          .add($("tr")
            .add($("td")
              .text(_("Without Nicks")))))
      ;
    } else {
      const cols = 6;
      const rows = Math.ceil(this._nicks.length / cols);
      const list = [];

      for (let c = 0; c < cols; ++c) {
        for (let r = 0; r < rows; ++r) {
          list[c + r * cols] = this._nicks[r + c * rows];
        }
      }

      const tb = $("table")
        .att("align", "center")
        .klass("frame")
        .style("border-collapse : collapse;")
      ;
      let n = 0;
      let tr = $("tr");
      list.forEach(nk => {
        if (n > 0 && n % cols === 0) {
          tb.add(tr);
          tr = $("tr");
        }
        const wg = $("div");
        if (nk !== undefined) {
          //eslint-disable-next-line
          new Wnick(
            wg,
            this._nicksPg,
            nk,
            nk.id === this._model,
            this._volumes[nk.name]
          );
        }
        tr.add($("td")
          .style("width:100px;text-align:center;border-right: solid 1px;")
          .add(wg)
        );
        ++n;
      });
      tb.add(tr);

      this._bodyDiv
        .removeAll()
        .add(this._msgWait)
        .add(tb)
      ;
    }
  }

  /** @private */
  setWait (nickName) {
    this._msgWait.removeAll();

    if (nickName !== "") {
      const box = new ModalBox(
        $("div")
          .add($("div")
            .style("text-align:center")
            .add(Ui.img("wait2.gif").klass("frame")))
          .add($("div").style("text-align:center").html(nickName)),
        false
      );
      this._msgWait.add(box.wg);
      box.show(true);
    }
  }

  // Control -------------------------------------------------------------------

  /**
      @private
      @return void
  **/
  async download () {
    let error = false;
    let warning = false;
    const nicks = this._nicks;
    for (let i = 0; i < nicks.length; ++i) {
      if (nicks[i].id === this._model) {
        const tmp = nicks[0];
        nicks[0] = nicks[i];
        nicks[i] = tmp;
        break;
      }
    }

    for (const nk of this._nicks) {
      this.setWait(nk.name);

      const rp = await Cts.client.ssend({
        "module": "settings",
        "source": "nicks/list",
        "rq": "download",
        "nickId": nk.id
      });

      if (rp["result"] === "error") {
        error = true;
      } else if (rp["result"] === "warning") {
        warning = true;
      }
    }
    this.setWait("");
    if (error) {
      if (warning) {
        Msg.error(_("Errors and warnings found."));
      } else {
        Msg.error(_("Errors found."));
      }
    } else if (warning) {
      Msg.info(_("Warning found."));
    } else {
      Msg.ok(_("Download ok."));
    }
  }

  /**
      @private
      @return void
  **/
  async test () {
    let error = false;
    let warning = false;
    for (const nk of this._nicks) {
      this.setWait(nk.name);

      const rp = await Cts.client.ssend({
        "module": "settings",
        "source": "nicks/list",
        "rq": "test",
        "nickName": nk.name
      });

      if (rp["result"] === "error") {
        error = true;
      } else if (rp["result"] === "warning") {
        warning = true;
      }
    }
    this.setWait("");
    if (error) {
      if (warning) {
        Msg.error(_("Errors and warnings found."));
      } else {
        Msg.error(_("Errors found."));
      }
    } else if (warning) {
      Msg.info(_("Warning found."));
    } else {
      Msg.ok(_("Test ok."));
    }
  }

}
