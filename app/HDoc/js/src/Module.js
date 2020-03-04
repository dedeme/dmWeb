// Copyright 02-Feb-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import Client from "./dmjs/Client.js"; // eslint-disable-line
import It from "./dmjs/It.js";
import Ui from "./dmjs/Ui.js";
import Domo from "./dmjs/Domo.js"; // eslint-disable-line
import Maybe from "./dmjs/Maybe.js";
import {Mod} from "./data/Mod.js"; // eslint-disable-line
import MkMod from "./data/MkMod.js";
import {_} from "./I18n.js";

const $ = e => Ui.$(e);

/**
    @param {string} tx
    @return {!Domo}
**/
function box (tx) {
  return $("table").add($("tr").add($("td")
    .add($("pre").klass("frame").text(tx))));
}

/**
    @param {string} id
    @param {string} doc
    @return {!Domo}
**/
function docBox (id, doc) {
  if (doc.startsWith(id + " ") || doc.startsWith(id + "\n")) {
    let ix = doc.indexOf("\n");
    if (ix === -1) ix = doc.length;
    const r = $("div")
      .add($("pre").style("font-style: italic").text(doc.substring(0, ix)));
    const right = doc.substring(ix).trim();
    return right === "" ? r : r.add(box(right));
  }
  return box(doc);
}

/**
    Index page.
**/
export default class Module {

  /**
      @param {!Client} client
      @param {string} lib
      @param {string} modName
      @param {string} tx
  **/
  constructor (client, lib, modName, tx) {
    this._client = client;
    this._lib = lib;
    this._modName = modName;
    this._tx = tx;

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
    const lib = this._lib;
    const modName = this._modName;
    const mod = MkMod.run(this._tx);

    function index () {
      const n = mod.entries.length;
      const nrows = (n % 4 === 0) ? (n / 4) | 0 : ((n / 4) | 0) + 1;
      const tds = [...It.range(nrows * 4)].map(() =>
        $("td").style("width:25%")
      );
      mod.entries.sort((e1, e2) => e1.id > e2.id ? 1 : -1);

      for (let i = 0; i < n; ++i) {
        const c = (i / nrows) | 0;
        const r = i % nrows;
        const id = mod.entries[i].id;
        const td = tds[r * 4 + c];
        if (mod.entries[i].line === -1)
          td.text(id);
        else
          td.add($("a").klass("link").att("href", "#" + id).text(id));
      }

      const trs = [];
      let tr;
      for (let i = 0; i < tds.length; ++i) {
        if (i % 4 === 0) {
          if (i !== 0) trs.push(tr);
          tr = $("tr");
        }
        tr.add(tds[i]);
      }
      if (n > 0) trs.push(tr);

      return $("div")
        .add($("div").klass("frame2").html("<b>" + modName + "</b>"))
        .add($("table").klass("main").adds(trs))
      ;
    }

    function overview () {
      function doc () {
        if (mod.overview.length === 0) return $("div");
        const d = mod.overview[0].doc;
        if (d === "") return $("div");
        return box(d);
      }

      return $("div")
        .add($("hr"))
        .add($("div").klass("frame").html("<b>" + _("Overview") + "</b>"))
        .add($("div").html("&nbsp;"))
        .add($("div").html("<b>" + _("File") + "</b>"))
        .add($("div")
          .add($("a").att("href", "?" + lib + "&" + modName + "&0")
            .text(modName)))
        .add($("div").html("&nbsp;"))
        .add(doc())
      ;
    }

    function definitions () {
      function divEntry (e) {
        return $("div")
          .add($("a").klass("link")
            .att("id", e.id)
            .att("href", "?" + lib + "&" + modName + "&" + String(e.line))
            .text(e.id))
          .add($("pre").style("color: #004080").text(e.code))
          .add(docBox(e.id, e.doc))
          .add($("hr"))
        ;
      }

      return $("div")
        .add($("hr"))
        .add($("div").klass("frame"))
        .add($("div").html("&nbsp;"))
        .adds(mod.entries.filter(e => e.line !== -1).map(e => divEntry(e)))
      ;
    }

    $("@title").text(this._modName);
    this._wg.removeAll()
      .add(index())
      .add(overview())
      .add(definitions())
      .add($("div").style("height:1200px"))
      .add(Ui.upTop("up"))
    ;
  }

  // Control -------------------------------------------------------------------

  /**
      @param {!Client} client
      @param {string} lib
      @param {string} modName
      @param {string} filePath
      @return {!Promise<!Maybe<!Module>>}
  **/
  static async mk (client, lib, modName, filePath) {
    const rp = await client.send({
      "source": "Code",
      "path": filePath
    });
    return Maybe.fromJs(rp["code"]).fmap(tx =>
      new Module(client, lib, modName, tx)
    );
  }

}
