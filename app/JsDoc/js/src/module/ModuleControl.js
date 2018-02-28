// Copyright 03-Jan-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

goog.provide("module_Control");

goog.require("module_View");

module_Control = class {
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
      const parts = Ui.url()["0"].split("@");
      const selected = parts[0];
      const relative = parts[1];
      const p = It.from(self._paths).findFirst(p => p.id() === selected);
      if (p === undefined) {
        alert(_args(_("Library %0 not found"), selected));
        location.assign("?@")
        return;
      }
      const path = p.path() + "/" + relative + ".js";
      const data = {
        "page" : "Module",
        "rq": "code",
        "path" : path
      }
      client.send0(data, rp => {
        new module_View(self).show(
          self.paths(), selected, relative, rp["text"]
        );
      });
    });
  }

}

