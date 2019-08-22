// Copyright 20-Aug-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import Domo from "./dmjs/Domo.js"; //eslint-disable-line
import Ui from "./dmjs/Ui.js"; //eslint-disable-line
import Main from "./Main.js"; //eslint-disable-line
import {_, _args} from "./I18n.js";
import {Doc, DocEntry} from "./data/Doc.js"; //eslint-disable-line

const $ = e => Ui.$(e);

/**
    @param {string} tx
    @return {string}
**/
function makeHelp (tx) {
  if (tx.trim() === "")
    return "";
  const tx1 = [];
  const tx2 = [];
  let txx = tx1;
  tx.split("\n").forEach(l => {
    if (l.startsWith("  ")) {
      const ix = l.indexOf(":");
      if (ix !== -1) {
        const word = l.substring(0, ix).trim();
        if (word.indexOf(" ") === -1) {
          txx = tx2;
        }
      }
    }
    txx.push(l);
  });
  let r = "<table><tr><td class='GrFrame' style='white-space: nowrap'><pre>" +
    "<span style='font-size: 14px;'>" +
    tx1.join("\n").replace("&", "&amp;").replace("<", "&lt;") +
    "</span></pre></td></tr></table>"
  ;
  if (tx2.length) {
    r += "<table><tr><td style='white-space:nowrap'>" +
      "<div class='frame' style='padding-right:20px'><pre>" +
      tx2.join("\n").replace("&", "&amp;").replace("<", "&lt;") +
      "</pre></div></td></tr></table>"
    ;
  }
  return r;
}

/**
    Module page.
**/
export default class Module {

  /**
      @param {!Main} main
  **/
  constructor (main) {
    this._main = main;
  }

  // View ----------------------------------------------------------------------

  /**
      @param {string} id
      @param {string} path
  **/
  writeFail (id, path) {
    this._main.view.removeAll()
      .add($("table").att("align", "center")
        .add($("tr").add($("td").add($("div").klass("frame").html(
          _args(_("File '%0.h' not found"), path) + "<br><br>" +
          _args(_("Click <a href='?%0'>here</a> to continue."), id)
        )))));
  }

  /**
      @param {string} id
      @param {string} path
      @param {!Doc} doc
  **/
  write (id, path, doc) {
    function fsort (e1, e2) {
      return e1.name > e2.name ? 1 : e2.name > e1.name ? -1 : 0;
    }
    doc.enums.sort(fsort);
    doc.structs.sort(fsort);
    doc.unions.sort(fsort);
    doc.typedefs.sort(fsort);
    doc.functions.sort(fsort);
    doc.vars.sort(fsort);
    doc.defines.sort(fsort);

    /**
        @param {string} bf
        @param {!Doc} mod
        @return {string}
     */
    function index (bf, mod) {
      const tab = "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;";
      /**
          @param {!Array<!Doc> | !Array<!DocEntry>} a
          @param {string} name
          @return {string}
       */
      function addBlock (a, name) {
        if (a.length === 0) {
          return bf;
        }
        bf += "<tr><td colspan='3'><i>" + name + "</i></td></tr>";
        const h = Math.floor((a.length - 1) / 3) + 1;
        for (let y = 0; y < h; ++y) {
          bf += "<tr>";
          for (let x = 0; x < 3; ++x) {
            const e = a[x * h + y];
            bf += !e
              ? "<td></td>"
              : "<td><a href='#hp:" + e.name + "'>" +
                tab + e.name + "</a></td>"
            ;
          }
          bf += "</tr>";
        }
        return bf;
      }

      bf += "<p class='frame2'><b>" + path + "</b></p>";

      bf += "<table border=0 width='100%'>";

      addBlock(mod.defines, "Defines");
      addBlock(mod.enums, "Enums");
      addBlock(mod.structs, "Structs");
      addBlock(mod.unions, "Unions");
      addBlock(mod.typedefs, "Typedefs");
      addBlock(mod.functions, "Functions");
      addBlock(mod.vars, "Variables");

      bf += "</table><hr>";
      return bf;
    }

    /**
        @param {string} bf
        @param {!Doc} mod
        @return {string}
     */
    function overview (bf, mod) {
      bf += "<p class='frame'><b>Overview</b></p>";
      bf += makeHelp(mod.doc);
      bf += "<p><b>File</b><br><a href='?" +
        id + "@" + path + "&hp:'>" +
        path + ".h" + "</a>"
      ;
      bf += " | <a href='?"
      + id + "@" + path + "&hp::'>"
      + path + ".c" + "</a>"
      ;
      bf += "</p><hr>";
      return bf;
    }

    /**
        @param {string} bf
        @param {!Doc} mod
        @return {string}
     */
    function body (bf, mod) {
      /**
          @param {string} link
          @param {string} name
       */
      function makeLink (link, name) {
        return "<a href='?" + id + "@" + path +
          "&hp:" + link + "'>" + name + "</a>";
      }
      /**
          @param {string} bf
          @param {!DocEntry} e
          @return {string}
       */
      function endEntry (bf, e) {
        let isNewLine = true;
        let bf2 = "";
        const code = e.code;
        for (let i = 0; i < code.length; ++i) {
          const ch = code[i];
          if (isNewLine) {
            if (/\s/.test(ch)) {
              bf2 += "&nbsp;";
            } else {
              bf2 += ch;
              isNewLine = false;
            }
          } else if (ch === "\n") {
            bf2 += "<br>";
            isNewLine = true;
          } else {
            bf2 += ch;
          }
        }

        bf += "<p><tt>" + bf2.toString() + "</tt></p>";
        bf += makeHelp(e.doc);
        bf += "<hr>";
        return bf;
      }
      /**
          @param {!Array<!DocEntry>} a
          @return {void}
       */
      function addBlock (a, name) {
        a.forEach(e => {
          bf += "<h3 id='hp:" + e.name + "'>" + name + " " +
            makeLink(e.link, e.name) + "</h3>";
          bf = endEntry(bf, e);
        });
      }

      addBlock(mod.defines, "define");
      addBlock(mod.enums, "enum");
      addBlock(mod.structs, "struct");
      addBlock(mod.unions, "union");
      addBlock(mod.typedefs, "typedef");

      mod.functions.forEach(e => {
        bf += "<h3 id='hp:" + e.name + "'>function " +
          makeLink(":" + e.link, e.name) + "</h3>";
        bf = endEntry(bf, e);
      });

      addBlock(mod.vars, "variable");

      return bf;
    }

    let bf = "";
    bf = index(bf, doc);
    bf = overview(bf, doc);
    bf += "<hr class='frame'>";
    bf = body(bf, doc);

    for(let i = 0; i < 22; ++i) bf += "<p>&nbsp;</p>";

    bf += "<div style='position: fixed;bottom: 0px;right: 20px'>" +
      "<a href='#'><img border='0' src='img/up.png'></a></div>";

    this._main.view.removeAll().html(bf);

    const iBar = path.lastIndexOf("/");
    $("@title").text(iBar === -1 ? path : path.substring(iBar + 1));

    const lc = location.href/**/;
    const ix = lc.indexOf("#");
    if (ix !== -1) {
      const tg = lc.substring(ix);
      if (tg !== "#") {
        const e = $(tg).e;
        if (e) {
          e.scrollIntoView(true);
        }
      }
    }
  }

  /**
      @param {string} id
      @param {string} path
  **/
  show (id, path) {
    this.update(id, path);
  }

  // Control -------------------------------------------------------------------

  /**
      @param {string} id
      @param {string} path
  **/
  async update (id, path) {
    const rq = {
      "page": "Module",
      "id": id,
      "path": path
    };
    const /** !Object<string, !Array<?>> */ rp = await Main.client.rq(rq);
    const doc = rp["doc"];
    if (doc === null) this.writeFail(id, path);
    else this.write(id, path, Doc.fromJs(doc));
  }

}
