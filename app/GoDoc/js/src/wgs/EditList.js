// Copyright 03-Apr-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import Domo from "../dmjs/Domo.js"; //eslint-disable-line
import Ui from "../dmjs/Ui.js";
import PLibRow from "../data/PLibRow.js";  //eslint-disable-line
import Client from "../dmjs/Client.js"; //eslint-disable-line
import {_} from "../I18n.js";
import SimpleList from "./SimpleList.js";

const $ = e => Ui.$(e);

/**
    @private
    @param {!Array<!PLibRow>} libs
    @return {string}
**/
function editorFormat (libs) {
  let tx = "";
  libs.forEach(e => {
    tx += e.path + "\n";
  });
  return tx;
}

/**
    Path list
**/
export default class EditList {
  /**
      @param {!Client} client
      @param {boolean} isPersonal
      @param {!Array<!PLibRow>} libs
      @param {function(!Array<string>):Promise<void>} updateLibs
  **/
  constructor (client, isPersonal, libs, updateLibs) {
    this._client = client;
    this._isPersonal = isPersonal;
    libs.sort((e1, e2) => e1.path > e2.path ? 1 : -1);
    this._libs = libs;
    this._updateLibs = updateLibs;

    this._isEdit = false;
    this._ed = $("textarea").klass("frame2").att("spellcheck", false);
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
    if (this._isEdit) {
      const w = String(this._wg.e.offsetWidth);
      let nh = this._wg.e.offsetHeight;
      if (nh < 100) nh = 100;
      const h = String(nh);
      this._ed.style(`width:${w}px;height:${h}px`)
        .text(editorFormat(this._libs));
      this._wg.removeAll()
        .add($("div").style("text-align:right")
          .add(Ui.link(() => { this.cancel() }).klass("link")
            .text(_("Cancel")))
          .add($("span").text(" | "))
          .add(Ui.link(() => { this.accept() }).klass("link")
            .text(_("Accept"))))
        .add(this._ed);
    } else {
      this._wg.removeAll()
        .add($("div").style("text-align:center")
          .add(Ui.link(() => { this.edit() }).klass("link").text(_("Edit"))))
        .add($("div").style("text-align:left").klass("frame3")
          .adds(this.mkRows()));
    }
  }

  /**
      @private
      @return {!Array<!Domo>}
  **/
  mkRows () {
    const libs = this._libs;
    const rows = [];
    let first = true;
    let previous = "";
    for (let i = 0; i < libs.length; ++i) {
      const lib = libs[i];
      if (first) first = false;
      else rows.push($("br"));
      rows.push(Ui.link(() => { this.clicked(lib) }).klass("link")
        .setStyle("font-family", "monospace")
        .att("title", lib.path)
        .html(SimpleList.format(previous, lib.path)));
      previous = lib.path;
    }
    return rows;
  }

  // Control -------------------------------------------------------------------

  /**
    @private
    @param {!PLibRow} lib
    @return {void}
  **/
  clicked (lib) {
    const path = this._isPersonal ? lib.path : "*" + lib.path;
    location.assign("?" + path);
  }

  /**
      @private
  **/
  edit () {
    this._isEdit = true;
    this.view();
  }

  /**
      @private
  **/
  accept () {
    let paths = this._ed.value().split("\n");
    paths.sort();
    let previous = "";
    paths = paths.filter(e => {
      if (e === "" || e === previous) return false;
      previous = e;
      return true;
    });
    this._updateLibs(paths);
  }

  /**
      @private
  **/
  cancel () {
    this._isEdit = false;
    this.view();
  }

}
