// Copyright 06-Sept-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/** Module page */

// eslint-disable-next-line
import Main from "./Main.js";
import Ui from "./dmjs/Ui.js";
import It from "./dmjs/It.js";

const $ = Ui.$;

const mkTd = () => {
  return $("td").style("width:25%");
};

const mkLink = (sel, mod, type, id) => {
  return mkTd().html(`<a href = "#${type}.${id}">${id}</a>`);
};

const mkTable = (cells) => {
  const len = cells.length;
  const table = $("table").klass("main");
  const nrows = Math.floor((len - 1) / 4) + 1;
  It.range(nrows).each(r => {
    const tr = $("tr");
    It.range(4).each(c => {
      const ix = c * nrows + r;
      if (ix < len) {
        tr.add(cells[ix]);
      } else {
        tr.add($("td"));
      }
    });
    table.add(tr);
  });
  return table;
};

const mkDocOverview = (tx) => {
  const parsePre = (tx) => {
    return tx.replace(/&/g, "&amp;").replace(/</g, "&lt");
  }
  let inPre = false;
  tx = tx.split("\n").map(l => {
    if (inPre) {
      if (l.startsWith("   ")) {
        return parsePre(l.substring(3));
      } else {
        inPre = false;
        return "</pre>\n" + l;
      }
    } else {
      if (l.startsWith("   ")) {
        inPre = true;
        return "<pre class='frame'>\n" + parsePre(l.substring(3));
      } else {
        return l;
      }
    }
  }).join("\n");
  const td = $("td").html(tx);

  return $("table").add($("tr").add(td));
};

const mkDoc = (tx) => {
  tx = tx.replace(/<pre>/g, "<pre class='frame'>");
  const td = $("td").html(tx);

  return $("table").add($("tr").add(td));
};

/** Module page. */
export default class Module {
  /**
   * @param {!Main} main Main page
   */
  constructor (main) {
    /**
     * @private
     * @type {!Main}
     */
    this._main = main;
  }

  // ____
  // View ------------------------------------------------------------
  // TTTT


  /** @private */
  addIx(div, rp, id) {
    const model = this._main.model;
    const sel = model.sel;
    const mod = model.module;
    const entries = rp[id];
    if (entries.length == 0) {
      return;
    }

    let title = "<i>" + id +"</i>";
    entries.sort((a, b) => a.name.localeCompare(b.name));
    const cells = entries.map(entry => mkLink(sel, mod, entry.type, entry.name));
    div.add($("div")
      .add($("hr"))
      .add($("div").html(title))
      .add(mkTable(cells)));
  }

  /** @private */
  mkHead (rp) {
    const model = this._main.model;
/*
    mjson_put(m, "types", json_warray(filter(entries, "skType")));
    mjson_put(m, "enums", json_warray(filter(entries, "skEnumField")));
    mjson_put(m, "params", json_warray(filter(entries, "skParam")));
    mjson_put(m, "methods", json_warray(filter(entries, "skMethod")));
    mjson_put(m, "macros", json_warray(filter(entries, "skMacro")));
    mjson_put(m, "templates", json_warray(filter(entries, "skTemplate")));
    mjson_put(m, "iterators", json_warray(filter(entries, "skIterator")));
    mjson_put(m, "procs", json_warray(filter(entries, "skProc")));
*/
    const r = $("div")
      .add($("div").klass("frame2").html("<b>" + model.module + "</b>"));

    this.addIx(r, rp, "types");
    this.addIx(r, rp, "procs");

    return r;
  }

  /** @private */
  mkOverview (tx) {
    const model = this._main.model;
    const mod = model.module;
    const sel = model.sel;
    const selMod = `${sel}@${mod}`;
    return $("div")
      .add($("hr"))
      .add($("div").klass("frame").html("<b>Overview</b>"))
      .add($("div").html("&nbsp;"))
      .add($("div").html(`<b>File</b><br><a href="?${selMod}&hp:">${mod}</a>`))
      .add(mkDocOverview(tx))
      .add($("hr"))
      .add($("div").klass("frame").html(""))
    ;
  }

  /** @private */
  mkBody (entries) {
    const model = this._main.model;
    const sel = model.sel;
    const mod = model.module;

    const body = $("div");

    for (const entry of entries) {
      const name = entry["name"];
      const type = entry["type"];
      const type2 = type.substring(2);
      const code = entry["code"];
      const line = entry["line"];
      const desc = entry["description"] || "";
      const link = `?${sel}@${mod}&${line}`
      body
        .add($("div").att("id", `${type}.${name}`).html("&nbsp;"))
        .add($("div").html(`<a href="${link}"><b>${type2}</b> ${name}</a>`))
        .add($("pre").klass("def").text(code))
        .add(mkDoc(desc))
        .add($("hr"));
    }

    return body;
  }

  /** @private */
  mkFoot () {
    return $("div")
      .adds([...It.range(35).map(() => $("div").html("&nbsp"))])
      .add($("div").style("position: fixed;bottom: 0px;right: 20px")
        .add(Ui.link(() => location.assign("#"))
          .add(Ui.img("up"))))
    ;
  }

  /** @private */
  show2 (rp) {
    const main = this._main;
    const pg = $("div")
      .add(this.mkHead(rp))
      .add(this.mkOverview(rp["overview"]))
      .add(this.mkBody(rp["all"]))
      .add(this.mkFoot())
    ;
    main.dom.show(pg);
    $("@title").text(main.model.module);
  }

  /**
   * @return {Promise}
   */
  async show () {
    const self = this;
    const main = self._main;
    const sel = main.model.sel;
    const module = main.model.module;
    const path = main.model.paths.find(p => p.id === sel);
    if (path === undefined || module === "") {
      main.go("@");
      return;
    }

    const rq = {
      "page": "module",
      "path": path.path + "/" + module + ".nim"
    };
    const rp = await main.client.send(rq);
    if (!rp["ok"]) {
      main.go("@");
      return;
    }
console.log(rp);
    self.show2(rp);
  }
}
