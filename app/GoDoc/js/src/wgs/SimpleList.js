// Copyright 03-Apr-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import Domo from "../dmjs/Domo.js"; //eslint-disable-line
import Ui from "../dmjs/Ui.js";
import PLibRow from "../data/PLibRow.js";  //eslint-disable-line
import Cts from "../Cts.js";
import {_} from "../I18n.js";

const $ = e => Ui.$(e);

/**
    Path list
**/
export default class SimpleList {
  /**
      @param {boolean} isPersonal
      @param {!Array<!PLibRow>} libs
  **/
  constructor (isPersonal, libs) {
    this._isPersonal = isPersonal;
    libs.sort((e1, e2) => e1.pond < e2.pond ? 1 : -1);
    this._libs = libs.slice(0, Cts.mostUsedEntries)
      .sort((e1, e2) => e1.path > e2.path ? 1 : -1);

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

    this._wg.removeAll()
      .add($("div").style("text-align:center")
        .add(Ui.link(() => { this.goRoot() }).klass("link").text(_("Root"))))
      .add($("div").style("text-align:left").klass("frame2")
        .adds(rows));
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
  goRoot () {
    const path = this._isPersonal ? "/" : "*/";
    location.assign("?" + path);
  }

  // Static --------------------------------------------------------------------

  /**
      @param {string} previous
      @param {string} lib
      @return {string}
  **/
  static format (previous, lib) {
    function extract (p) {
      let r = p;

      const ix = p.lastIndexOf("/");
      if (ix === -1) return p;
      r = r.substring(0, ix);

      const ix2 = r.lastIndexOf("/");
      if (ix2 === -1) return p;
      return p.substring(ix2 + 1);
    }

    previous = extract(previous);
    lib = extract(lib);
    let blanks = 0;
    while (true) {
      const pi = previous.indexOf("/");
      if (pi === -1) break;
      const li = lib.indexOf("/");
      if (li === pi && previous.substring(0, pi) === lib.substring(0, li)) {
        blanks += pi + 1;
        previous = previous.substring(pi + 1);
        lib = lib.substring(li + 1);
        continue;
      }
      break;
    }
    return "&nbsp;".repeat(blanks) + lib;
  }


}
