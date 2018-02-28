// Copyright 03-Jan-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

goog.provide("index_View");

goog.require("index_Tree");

{
    const cmp = (s1, s2) => s1.toUpperCase() > s2.toUpperCase() ? 1 : -1;

    const sort = (e1, e2) =>
      e1._help === null
        ? e2._help === null
          ? cmp(e1._id, e2._id)
          : 1
        : e2._help === null
          ? -1
          : cmp(e1._id, e2._id);

index_View = class {

  /** @param {!index_Control} control */
  constructor (control) {
    /** @const {!index_Control} */
    this._control = control;
  }

  /**
   * @param {!Array<!Path>} paths
   * @param {string} selected
   * @param {!index_Tree} tree
   * @return {void}
   */
  show (paths, selected, tree) {
    const table = $("table").att("class", "frame").att("width", "100%");

    /**
     * @param {List<string>} prefix
     * @param {List<string>} path
     * @param {Array<!index_Tree>} ies
     * @return {void}
     */
    function addTrs(prefix, path, ies) {
      if (ies === null) {
        throw ("Array of IndexTree is null");
      }
      It.from(ies).sortf(sort).each(ie => {
        if (ie.help() === null) {
          table.add($("tr")
            .add($("td")
              .att("style", "text-align:left;width:5px")
              .html(prefix.head() + "<b>" + ie.id() + "</b>"))
            .add($("td"))
            .add($("td"))
          );
          addTrs(
            prefix.cons(prefix.head() + "&nbsp;&nbsp;&nbsp;&nbsp;"),
            prefix.cons(path.head() + (path.head() == "" ? "" : "/") + ie.id()),
            ie.entries()
          );
        } else {
          const modName = ie.id().substring(0, ie.id().length - 3)
          table.add($("tr")
            .add($("td")
              .att("style", "text-align:left;font-weight:bold;width:5px")
              .html("<a href='?" +
                selected + "@" + path.head() +
                (path.head() == "" ? "" : "/") +
                modName +
                "'>" +
                prefix.head() + modName +
                "</a>"))
            .add($("td").att("style", "width:5px").text("  "))
            .add($("td").html(ie.help()))
          );
        }
      });
    }

    addTrs(new List().cons(""), new List().cons(""), tree.entries());
    this._control.control().dom().show(paths, selected, table);
    $$("title").next().text("JsDoc : " + selected);
  }
}}
