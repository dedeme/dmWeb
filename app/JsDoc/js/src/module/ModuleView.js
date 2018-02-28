// Copyright 03-Jan-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

goog.provide("module_View");

goog.require("module_TxCode");

{
  function addTopLinks () {
    const headers = document.getElementsByTagName('h3');
    for (let i = 0; i < headers.length; ++i) {
      const span = document.createElement('span');
      span.className/**/ = 'navtop';
      const link = document.createElement("a");
      span.appendChild(link);
      link.href/**/ = "#top";
      link.className/**/ ="navtop";
      const textNode = document.createTextNode('[Top]');
      link.appendChild(textNode);
      headers[i].appendChild(link);
    }
  }

module_View = class {

  /** @param {!module_Control} control */
  constructor (control) {
    /** @const {!module_Control} */
    this._control = control;
  }

  /**
   * @param {!Array<!Path>} paths
   * @param {string} selected
   * @param {string} modPath
   * @param {string} text
   * @return {void}
   */
  show (paths, selected, modPath, text) {
    const methods = module_TxCode.mkModule(text);
    const size = methods.length;
    const nCol = size < 10
      ? 10
      : size < 20
        ? Math.ceil(size / 2)
        : size < 30
          ? Math.ceil(size / 3)
          : Math.ceil(size / 4);
    let c = 0;
    const nextC = () => {
      if (++c >= nCol) {
        c = 0;
        return "<td valign='top' style='width:5px;white-space:nowrap'>";
      }
      return "";
    }

    const index = "<table border=0 width='100%'><tr>" +
      "<td valign='top' style='width:5px;white-space:nowrap'>" +
      It.from(methods).sortf(module_TxMethod.sortf/**/).reduce("", (s, m) =>
        s += "<a href='#help:" + m.id() + "'>" + (
          m.id() === ""
            ? "()"
            : m.st()
              ? "<i>" + m.id() + "</i>"
              : m.id()
        ) + "</a><br>" + nextC()
      ) +
      "</tr></table>";

    let overview =  "<p class='frame'><b>Overview</b></p>" +
      "<table border='0'><tr><td style='width:0px;'><pre>" +
      module_TxCode.overview() +
      "</pre></td><td></td></tr></table>" +
      "<p><b>File</b><br><a href='?" +
      selected + "@" + modPath + "&hp:'>" +
      modPath + ".js" + "</a>" +
      "</p><hr>";

    let body = It.from(methods).sortf(module_TxMethod.sortf/**/)
      .reduce("", (s, m) =>
        s += "<h3 id='help:" + m.id() + "'>" +
          "<a href='?" + selected + "@" + modPath +
          "&help:" + m.id() + "'>" + (
            m.id() === ""
              ? "()"
              : m.st()
                ? "<i>" + m.id() + "</i>"
                : m.id()
          ) + "</a></h3>" +
          "<p><tt>" + m.head()  + "</tt></p>" +
          "<table border='0'><tr><td style='width:0px;'>" +
          "<pre>" + m.help() + "</pre>" +
          "</td><td></td></tr></table>" +
          "<hr>"
      );

    this._control.control().dom().show(paths, selected, $("div").html(
      "<p class='frame2'><b>" +  modPath + "</b></p>" +
      index +
      overview +
      "<hr class='frame'>" +
      body +
      It.range(22).reduce("", (seed, i) => {
        return seed + "<p>&nbsp;</p>";
      })
    ));
    addTopLinks();

    const ix = modPath.lastIndexOf("/");
    $$("title").next().text(
      "JsDoc : " +
      (ix == -1 ? modPath : modPath.substring(ix + 1))
    );

    if (navigator.vendor/**/.indexOf("Google") != -1) {
      const hash = location.hash/**/;
      location.hash/**/ = "";
      location.hash/**/ = hash;
    }
  }
}}

