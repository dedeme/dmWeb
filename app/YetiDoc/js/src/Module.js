// Copyright 02-Feb-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import Client from "./dmjs/Client.js"; // eslint-disable-line
import Ui from "./dmjs/Ui.js";
import Domo from "./dmjs/Domo.js"; // eslint-disable-line
import Maybe from "./dmjs/Maybe.js";
import {Mod} from "./data/Mod.js";

const $ = e => Ui.$(e);

/**
    Index page.
**/
export default class Module {

  /**
      @param {!Client} client
      @param {string} lib
      @param {string} modPath
      @param {!Array<?>} d
  **/
  constructor (client, lib, modPath, d) {
    this._client = client;
    this._lib = lib;
    this._modPath = modPath;
    this._d = Mod.fromJs(d);

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
    const d = this._d;

    $("@title").text(this._modPath);
    this._wg.removeAll()
      .add($("div").html(d.html1))
      .add(this.index(d.tree))
      .add(this.overview(d.link))
      .add($("div").html(d.html2))
      .add($("div").style("height:1200px"))
      .add(Ui.upTop("up"))
    ;
  }

  index (tree) {
    function head (tp) {
      return [
        $("tr").add($("td").att("colspan", 4).add($("hr"))),
        $("tr")
          .add($("td").att("colspan", 4)
            .html(tp === "~"
              ? "<b>Module signature</b>"
              : "<b>Type</b> " + tp
            ))
      ];
    }

    function rows (tp, elements) {
      elements.sort((e1, e2) => e1.toUpperCase() > e2.toUpperCase() ? 1 : 0);
      const len = elements.length;
      const cols = 4;
      const rows = ((len + cols - 1) / cols) | 0;
      const rs = [];
      for (let r = 0; r < rows; ++r) {
        const row = $("tr");
        for (let c = 0; c < cols; ++c) {
          const ix = c * rows + r;
          if (ix >= len) {
            row.add($("td"));
          } else {
            row.add($("td").html(
              "<a href=\"#" + tp + "." + elements[ix] + "\">" +
              elements[ix] + "</a>"
            ));
          }
        }
        rs.push(row);
      }
      return rs;
    }

    function groupTrs (group, tp, elements) {
      if (elements.length === 0) return [];
      const r = [
        $("tr").add($("td").att("colspan", 4).html("<i>" + group + "</i>"))
      ];
      for (const row of rows(tp, elements)) {
        r.push(row);
      }
      return r;
    }

    function index (e) {
      if (e.enums.length + e.ps.length + e.ms.length === 0) return [];

      return head(e.tp)
        .concat(groupTrs("Variants", e.tp, e.enums))
        .concat(groupTrs("Fields", e.tp, e.ps))
        .concat(groupTrs("Functions", e.tp, e.ms))
      ;
    }

    function tds () {
      const r = [];
      for (const e of tree) {
        for (const row of index(e)) {
          r.push(row);
        }
      }
      return r;
    }

    return $("table").klass("main")
      .adds(tds());
  }

  overview (link) {
    return $("div")
      .add($("hr"))
      .add($("p")
        .add($("span").html("<b>File</b>"))
        .add($("br"))
        .add($("a").att("href", link.link).html(link.name)))
      .add($("p").klass("frame").html("<b>Overview</b>"))
    ;
  }

  // Control -------------------------------------------------------------------

  /**
      @param {!Client} client
      @param {string} lib
      @param {string} libPath
      @param {string} modPath
      @return {!Promise<!Maybe<!Module>>}
  **/
  static async mk (client, lib, libPath, modPath) {
    const rp = await client.send({
      "source": "Module",
      "lib": lib,
      "libPath": libPath,
      "modPath": modPath
    });
    return Maybe.fromJs(rp["data"]).fmap(d =>
      new Module(client, lib, modPath, d)
    );
  }

}
