// Copyright 28-Feb-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import Client from "./dmjs/Client.js"; // eslint-disable-line
import Ui from "./dmjs/Ui.js";
import Domo from "./dmjs/Domo.js"; // eslint-disable-line
import Maybe from "./dmjs/Maybe.js";
import STree from "./data/STree.js"; //eslint-disable-line
import {_} from "./I18n.js";

const $ = e => Ui.$(e);

/**
    @private
    @param {string} l
    @param {string} r
    @return {string}
**/
function fcat (l, r) {
  return l === "" ? r : r === "" ? "" : l + "/" + r;
}

/**
    @private
    @param {string} lib
    @param {string} prefix
    @param {string} path
    @param {!Array<!STree>} entries
    @return {!Array<!Domo>}
**/
function mkTrs (lib, prefix, path, entries) {
  let r = [];
  for (const e of entries) {
    if (e.isDir) {
      r.push(
        $("tr")
          .add($("td")
            .style("width:5px;white-space:nowrap")
            .html(prefix + "<b>" + e.name + "</b>"))
          .add($("td"))
      );
      r = r.concat(
        mkTrs(
          lib,
          prefix + "&nbsp;&nbsp;&nbsp;&nbsp;",
          fcat(path, e.name),
          /** @type {!Array<!STree>} */ (e.data)
        )
      );
    } else {
      r.push(
        $("tr")
          .add($("td").style("white-space:nowrap")
            .add($("a")
              .klass("link")
              .att("href", "?" + lib + "&" + fcat(path, e.name))
              .html(prefix + e.name)))
          .add($("td")
            .style("padding-left:8px")
            .text(/** @type {string} */ (e.data)))
      );
    }
  }
  return r;
}

/**
    Index page.
**/
export default class Index {

  /**
      @param {!Client} client
      @param {string} pathName
      @param {!STree} tree
  **/
  constructor (client, pathName, tree) {
    this._pathName = pathName;
    this._client = client;
    this._tree = tree;

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
    $("@title").text(this._pathName);
    const tree = this._tree;
    tree.sort();
    const ls = /** @type {!Array<!STree>} */ (this._tree.data);
    if (ls.length === 0) {
      this._wg.removeAll()
        .add($("table").att("align", "center")
          .add($("tr")
            .add($("td")
              .add($("div").klass("frame").text(_("Library is empty"))))));
    } else {
      this._wg.removeAll()
        .add($("table").klass("frame").adds(mkTrs(this._pathName, "", "", ls)));
    }
  }

  // Control -------------------------------------------------------------------

  /**
      @param {!Client} client
      @param {string} pathName
      @param {string} filePath
      @return {!Promise<!Maybe<!Index>>}
  **/
  static async mk (client, pathName, filePath) {
    const rp = await client.send({
      "source": "Index",
      "rq": "tree",
      "path": filePath
    });
    return Maybe.fromJs(
      rp["tree"], (js) => STree.fromJs(/** @type {!Array<?>} */ (js))
    ).fmap((t) => new Index(client, pathName, t));
  }

}
