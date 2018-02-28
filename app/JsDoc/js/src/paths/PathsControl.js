// Copyright 03-Jan-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

goog.provide("paths_Control");

goog.require("paths_View");
goog.require("Path");

paths_Control = class {
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
    client.send(data, rp => {
      const pathsJArray = rp["paths"]
      self._paths = It.from(pathsJArray).map(row => {
        const p = Path.restore(row)
        if (!p.valid()) {
          p.setShow(false);
        }
        return p;
      }).to();
      new paths_View(self).show();
    });
  }

  /**
   * @param {string} id
   * @param {string} path
   * @return {void}
   */
  newPath (id, path) {
    const self = this;
    id = id.trim();
    path = path.trim();
    while (path.endsWith("/")) {
      path = path.substring(0, path.length - 1)
    }

    const vId = self.validateId(id);
    if (vId !== "") {
      alert(vId);
      return;
    }

    const vPath = self.validatePath(path);
    if (vPath !== "") {
      alert(vPath);
      return;
    }

    self.paths().push(new Path(id, path, true, true));
    let data = {
      "page": "Paths",
      "rq": "setPaths",
      "paths": self.pathsToJson()
    };
    self._control.client().send(data, rp => {
      self.run();
    });
  }

  /**
   * @param {string} id
   * @return {void}
   */
  modifyBegin (id) {
    const view = new paths_View(this);
    view.show();
    view.modify(id);
  }

  modifyPath (id, newId, path, newPath) {
    const self = this;
    newId = newId.trim();
    newPath = newPath.trim();
    let vname = self.validateId(newId);
    let vpath = self.validatePath(newPath);

    if (newId !== id && vname !== "") {
      alert(vname);
    } else if (vpath !== "") {
      alert(vpath);
    } else {
      if (id === newId && path === newPath) {
        self.run();
      } else {
        let p = It.from(self.paths()).findFirst(p => p.id() === id);
        if (p) {
          p.setId(newId);
          p.setPath(newPath);
          let data = {
            "page": "Paths",
            "rq": "setPaths",
            "paths": self.pathsToJson()
          };
          this._control.client().send(data, rp => {
            self.run();
          });
          return;
        }
        throw ("Path " + id  + " does not exists")
      }
    }
  }

  /**
   * @param {string} id
   * @return {void}
   */
  deletePath (id) {
    const self = this;
    if (!confirm(_args(_("Delete %0?"), id))) {
      return;
    }
    const paths = self.paths();
    const ix = It.from(paths).indexf(p => p.id() === id);
    paths.splice(ix, 1);
    let data = {
      "page": "Paths",
      "rq": "setPaths",
      "paths": self.pathsToJson()
    };
    this._control.client().send(data, rp => {
      self.run();
    });
  }

  /**
   * @param {string} id
   * @param {boolean} sel
   * @param {boolean} err
   * @return {void}
   */
  selPath (id, sel, err) {
    const self = this;
    if (err) {
      alert(_("This source can not be selected, because it does not exist"));
      return;
    }
    let p = It.from(self.paths()).findFirst(p => p.id() === id);
    if (p) {
      p.setShow(sel);
      let data = {
        "page": "Paths",
        "rq": "setPaths",
        "paths": self.pathsToJson()
      };
      self._control.client().send(data, rp => {
        self.run();
      });
      return;
    }
    throw ("Path " + id  + " does not exists")

  }

  /**
   * @return {void}
   */
  changeShowAll () {
    const self = this;
    const conf = this._control.conf();
    conf.setShowAll(conf.showAll() ? false : true);
    let data = {"page": "Paths", "rq": "setConf", "conf": conf.serialize()};
    self._control.client().send(data, rp => {
      self.run();
    });
  }

  /**
   * @return {void}
   */
  changeLang () {
    const self = this;
    const conf = self._control.conf();
    conf.setLang(conf.lang() === "es" ? "en" : "es");
    let data = {"page": "Paths", "rq": "setConf", "conf": conf.serialize()};
    self._control.client().send(data, rp => {
      location.assign("");
    });
  }

  /**
   * @return {void}
   */
  changePassPage () {
    new user_Chpass(this).show();
  }

  /**
   * @param {string} pass
   * @param {string} newPass
   * @param {function(boolean):void} f Function to manage captcha counter.
   * @return {void}
   */
  changePass (pass, newPass, f) {
    const self = this;
    const data = {
      "page": "Paths",
      "rq": "chpass",
      "user": "admin",
      "pass": Client.crypPass(pass),
      "newPass": Client.crypPass(newPass)
    };
    self._control.client().send(data, rp => {
      const ok = rp["ok"];
      f(ok);
      if (ok) {
        alert(_("Password successfully changed"));
        location.assign("");
      } else {
        self.changePassPage();
      }
    });
  }

  /**
   * @private
   * @param {string} id
   * @return {string}
   */
  validateId (id) {
    return id == ""
      ? _("Name is missing")
      : id.indexOf("=") !== -1
        ? _args(_("Name '%0' contains '%1'"), id, "=")
        : id.indexOf("@") !== -1
          ? _args(_("Name '%0' contains '%1'"), id, "@")
          : id.indexOf("/") !== -1
            ? _args(_("Name '%0' contains '%1'"), id, "/")
            : id.indexOf(" ") !== -1
              ? _args(_("Name '%0' contains blanks"), id)
              : It.from(this.paths()).containsf(p => p.id() === id)
                ? _args(_("Name '%0' is repeated"), id)
                : "";
  };

  /**
   * @private
   * @param {string} path
   * @return {string}
   */
  validatePath (path) {
    return path == ""
      ? _("Path is missing")
      : "";
  };

  /**
   * @return {Array<?>}
   */
  pathsToJson () {
    const self = this;
    return It.from(self.paths()).map(p => p.serialize()).to();
  }

}

