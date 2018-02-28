// Copyright 03-Jan-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

goog.provide("index_Control");

goog.require("index_View");

index_Control = class {
  /**
   * @param {!Main} control
   */
  constructor (control) {
    /** @private */
    this._control = control;

    /**
     * @private
     * @type {Array<!Path>}
     */
    this._paths = null;

  }

  /** @return {!Main} */
  control () {
    return this._control;
  }

  /** @return {!Array<!Path>} */
  paths () {
    if (!this._paths) {
      throw ("this._patsh is null");
    }
    return this._paths;
  }

  run () {
    const self = this;
    const control = this._control;
    const client = control.client();
    const conf = control.conf();

    const data = {"page": "Paths", "rq": "get"};
    client.send0(data, rp => {
      const pathsJArray = rp["paths"]
      self._paths = It.from(pathsJArray).map(row => Path.restore(row)).to();
      const selected = Ui.url()["0"];
      const p = It.from(self._paths).findFirst(p => p.id() === selected);
      if (p === undefined) {
        alert(_args(_("Library %0 not found"), selected));
        location.assign("?@")
        return;
      }
      const data = {
        "page" : "Index",
        "rq": "path",
        "path" : p.path()
      }
      client.send0(data, rp => {
        const tree = index_Tree.restore(rp["tree"]);
        if (tree.help() === null && tree.entries() === null) {
          alert(_args(_("Error reading %0"), selected))
          location.assign("?@")
          return;
        }
        new index_View(self).show(self.paths(), selected, tree);
      });
    });
  }

}

