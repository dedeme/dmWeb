// Copyright 03-Jan-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

goog.provide("code_View");

goog.require("Tx");

code_View = class {
  /** @param {!code_Control} control */
  constructor (control) {
    /** @const {!code_Control} */
    this._control = control;
  }

  /**
   * @param {!Array<!Path>} paths
   * @param {string} selected
   * @param {string} modPath
   * @param {string} anchor
   * @param {string} text
   * @return {void}
   */
  show (paths, selected, modPath, anchor, text) {
    const code = "<table id='hp:' border='0'><tr><td class='frame'>" +
      "<a href='?" +
      selected + "@" + modPath + "'>" +
      modPath + "</td></tr></table>" +
      Tx.mkCode(text) +
      It.range(22).reduce("", (seed, i) => seed + "<p>&nbsp;</p>");

    this._control.control().dom().show(paths, selected, $("div").html(code));

    const ix = modPath.lastIndexOf("/");
    $$("title").next().text(
      "JsDoc : " +
      (ix == -1 ? modPath : modPath.substring(ix + 1))
    );

    $("#" + anchor).e().scrollIntoView(true);
/*    if (navigator.vendor.indexOf("Google") != -1) {
      const hash = location.hash;
      location.hash = "";
      location.hash = hash;
    }
    */
  }
}
