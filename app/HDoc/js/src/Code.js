// Copyright 04-Feb-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import Client from "./dmjs/Client.js"; // eslint-disable-line
import Ui from "./dmjs/Ui.js";
import Domo from "./dmjs/Domo.js"; // eslint-disable-line
import Maybe from "./dmjs/Maybe.js";
import MkCode from "./data/MkCode.js";

const $ = e => Ui.$(e);

/**
    @param {number} digits
    @param {number} n
    @return {string}
**/
function nformat (digits, n) {
  const r = "          " + String(n);
  return r.substring(r.length - digits);
}

/**
    @param {string} oldS
    @param {string} newS
    @param {string} inS
    @return {string}
**/
function replace (oldS, newS, inS) {
  return inS.split(oldS).join(newS);
}

/**
    Code page.
**/
export default class Code {

  /**
      @param {!Client} client
      @param {string} lib
      @param {string} modName
      @param {number} line
      @param {string} tx
  **/
  constructor (client, lib, modName, line, tx) {
    this._client = client;
    this._lib = lib;
    this._modName = modName;
    this._line = line;
    this._tx = tx;
    this._link = "?" + lib + "&" + modName;

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
      @return {void}
  **/
  view () {
    const tx = MkCode.run(
      replace(
        "<", "&lt;",
        replace(
          " ", "&nbsp;",
          replace("&", "&amp;", this._tx)))
    );
    const lsTx = tx.split("\n");
    while (lsTx.length > 0 && lsTx[lsTx.length - 1].trim() === "") lsTx.pop();
    const lineDigits = String(lsTx.length).length;
    const nline = this._line;
    const tdl = $("td").klass("prel");
    const tdr = $("td").klass("prer");

    let txl = "";
    let txr = "";
    lsTx.forEach((l, i) => {
      const ix = i + 1;
      let lineL = nformat(lineDigits, ix);
      if (ix === nline) lineL = "<span id='hp'>" + lineL + "</span>";
      txl += lineL + "<br>";
      txr += l + "<br>";
    });
    tdl.html(txl);
    tdr.html(txr);

    $("@title").text(this._modName);
    this._wg.removeAll()
      .add($("table").klass("frame")
        .add($("tr")
          .add($("td").html(`<a href="${this._link}">${this._modName}`))))
      .add($("br"))
      .add($("table")
        .att("border", 0)
        .att("width", "100%")
        .att("cellspacing", 0)
        .add($("tr")
          .add(tdl)
          .add(tdr)))
      .add($("div").style("height:1200px"))
      .add(Ui.upTop("up"))
    ;
  }

  // Control -------------------------------------------------------------------

  /**
      Jump to line.
  **/
  jump () {
    const el = $("#hp").e;
    if (this._line > 0 && el !== null) el.scrollIntoView();
  }

  /**
      @param {!Client} client
      @param {string} lib
      @param {string} modName
      @param {number} line
      @param {string} filePath
      @return {!Promise<!Maybe<!Code>>}
  **/
  static async mk (client, lib, modName, line, filePath) {
    const rp = await client.send({
      "source": "Code",
      "path": filePath
    });
    return Maybe.fromJs(rp["code"]).fmap(tx =>
      new Code(client, lib, modName, line, tx)
    );
  }

}
