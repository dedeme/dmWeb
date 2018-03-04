// Copyright 12-Nov-2017 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/**
 * Instalation requires pdfPrinter installed in /home/deme/bin
 */
goog.provide("Main");

goog.require("github_dedeme");
goog.require("I18n");
goog.require("Dom");
goog.require("Conf");
goog.require("Quote");
goog.require("Trace");
goog.require("user_Expired");
goog.require("user_Auth");
goog.require("user_Chpass");
goog.require("view_Backups");
goog.require("view_Settings");
goog.require("view_Bye");
goog.require("view_Run");
goog.require("view_Bests");
goog.require("view_Statistics");
goog.require("view_Trace");

Main = class {
  constructor () {
    /** @private */
    this._client = new Client(
      Main.app(),
      () => { new user_Expired(this).show(); }
    );

    /** @private */
    this._dom = new Dom(this);
    /**
     * @private
     * @type {!Array<string>}
     */
    this._backups = [];
    /**
     * @private
     * @type {!Array<string>}
     */
    this._trash = [];
    /**
     * @private
     * @type {Object<string,!Array<Flea>>}
     */
    this._bests = null;

    /**
     * @private
     * @type {number}
     */
    this._bestsLastUpdate = 0;


    /** @private */
    this._view = null
    /** @private */
    this._conf = null;
  }

  /** @return {string} */
  static app () {
    return "FleasData";
  }

  /** @return {string} */
  static version () {
    return "201802";
  }

  /** @return {number} */
  static maxQuotes () {
    return 550;
  }

  /** @return {string} */
  static langStore () {
    return Main.app() + "__lang";
  }

  /** @return {string} */
  static captchaAuthStore () {
    return Main.app() + "__captcha";
  }

  /** @return {string} */
  static captchaChpassStore () {
    return Main.app() + "__captchaCh";
  }

  /** @return {!Client} */
  client () {
    return this._client;
  }

  /** @return {!Dom} */
  dom () {
    return this._dom;
  }

  /** @return {!Conf} */
  conf () {
    if (this._conf) {
      return this._conf;
    }
    throw ("Conf has not been created");
  }

  /** @return {!Array<string>} */
  backups () {
    return this._backups;
  }

  /** @return {!Array<string>} */
  trash () {
    return this._trash;
  }

  /** @return {!Object<string,!Array<Flea>>} */
  bests () {
    if (this._bests) {
      return this._bests;
    }
    throw ("Bests have not been created");
  }

  /** @return {number} */
  bestsLastUpdate () {
    return this._bestsLastUpdate;
  }

  run2 () {
    const self = this;

    let data = {"rq": "getConf"};
    self._client.send(data, rp => {
      self._conf = Conf.restore(
        /** @type {!Array<?>} */ (JSON.parse(rp["conf"]))
      );
      self._conf.lang() === "es" ? I18n.es() : I18n.en();
//self._conf.setDbase("all");
      switch (self._conf.page()) {
        case "run":
          self._view = new view_Run(self);
          self._view.show();
          break;
        case "bests":
          data = {"rq": "bestsTime", "dbase": self._conf.dbase()};
          self._client.send(data, rq => {
            self._bestsLastUpdate = rq["time"];
            let more = true;
            let bests = [];
            It.range(1, 9).sync(
              (i, f) => {
                if (more) {
                  data = {
                    "rq": "readBests",
                    "dbase": self._conf.dbase(),
                    "ix": "" + i
                  };
                  self._client.send(data, rp => {
                    const moreBests =
                      /** @type {!Array<?>} */ (JSON.parse(rp["bests"]));
                    if (moreBests.length === 0) {
                      more = false;
                    } else {
                      bests = bests.concat(moreBests);
                    }
                    f();
                  });
                } else {
                  f();
                }
              }, () => {
                self._view = new view_Bests(self, bests);
                self._view.show();
              }
            );
          });
          break;
        case "statistics":
          data = {"rq": "bestsTime", "dbase": self._conf.dbase()};
          self._client.send(data, rq => {
            self._bestsLastUpdate = rq["time"];
            self._view = new view_Statistics(self);
            self._view.show();
          });
          break;
        case "trace":
          data = {"rq": "readTraces", "dbase": self._conf.dbase()}
          self._client.send(data, rp => {
            const traces = rp["traces"];
            if (traces) {
              const flea0 = Flea.restore(traces[0]);
              if (flea0 === null) {
                throw ("flea ought not to be null");
              }
              /** @type {!Flea} */
              const flea = flea0;
              const ts = It.from(traces[1]).map(t => Trace.restore(t)).to();
              const nicks = {};
              It.from(ts).each(t => { nicks[t.nick()] = "" });
              const quotes = {};
              It.keys(nicks).sync(
                function (n, f) {
                  data = {"rq": "readQuotes", "nick": n};
                  self._client.send(data, rq => {
                    quotes[n] = Quote.fromString(rq["quotes"]);
                    f();
                  });
                },
                function () {
                  self._view = new view_Trace(self, flea, ts, quotes);
                  self._view.show();
                }
              );
            } else {
              view_Trace.emptyPage(this);
            }
          });
          break;
        case "backups":
          data = {"rq": "readBackLists"};
          self._client.send(data, rp => {
            self._backups = rp["backups"];
            self._trash = rp["trash"];
            self._view = new view_Backups(self);
            self._view.show();
          });
          break;
        case "settings":
          self._view = new view_Settings(self);
          self._view.show();
          break;
        default:
          throw("Page '" + self._conf.page() + "' is unknown");
      }
    });
  }

  run () {
    const self = this;
    self._client.connect(ok => {
      if (ok) {
        self.run2();
      } else {
        new user_Auth(self, self._client).show();
      }
    });
  }

  /**
   * @param {function():void} f
   * @return {void}
   */
  sendConf (f) {
    const self = this;
    const data = {
      "rq": "setConf",
      "conf": JSON.stringify(self.conf().serialize())
    };
    self._client.send(data, rp => {
      f();
    });
  }

  /**
   * @param {string} target
   * @return {void}
   */
  go (target) {
    const self = this;
    self.conf().setSubpage("");
    self.conf().setPage(target);
    self.sendConf(() => { self.run2(); });
  }

  /**
   * @return {void}
   */
  bye () {
    const self = this;
    const data = {"rq" : "logout"};
    self._client.send(data, rp => { new view_Bye(self).show(); });
  }

// Settings ----------------------------------------------------------

  /** @return {void} */
  changeLanguage () {
    const self = this;
    self.conf().setLang(self.conf().lang() === "en" ? "es" : "en");
    self.sendConf(() => { self.run2(); });
  }

  /**
   * @param {string} dbase
   * @return {void}
   */
  setDbase(dbase) {
    const self = this;
    self.conf().setDbase(dbase);
    self.sendConf(() => { self.run2(); });
  }

  /** @return {void} */
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
      "rq": "chpass",
      "user": "admin",
      "pass": Client.crypPass(pass),
      "newPass": Client.crypPass(newPass)
    };
    self._client.send(data, rp => {
      const ok = rp["ok"];
      f(ok);
      if (ok) {
        alert(_("Password successfully changed"));
        self.run2();
      } else {
        self.changePassPage();
      }
    });
  }

// Backups -----------------------------------------------------------

  /**
   * Downloads a backup
   * @param {function(string):void} action This callback passes the name of
   *  backup file.
   * @return {void}
   */
  backupDownload (action) {
    const data = {"rq": "backup"};
    this._client.send(data, rp => { action(rp["name"]); });
  }

  /**
   * Restores a backup
   * @param {*} file
   * @param {function(number):void} progress
   */
  backupRestore (file, progress) {
    const self = this;
    const step = 25000;
    let start = 0;

    const reader = new FileReader();
    reader.onerror/**/ = evt => {
      alert(_args(_("'%0' can not be read"), file.name/**/));
      const data = {"rq": "restoreAbort"};
      self._client.send(data, () => {
        new view_Backups(self).show();
      });
    }
    reader.onloadend/**/ = evt => {
      if (evt.target/**/.readyState/**/ === FileReader.DONE/**/) { // DONE == 2
        const bindata = new Uint8Array(evt.target/**/.result/**/);
        progress(start);
        if (bindata.length > 0) {
          const data = {
            "rq": "restoreAppend",
            "data": B64.encodeBytes(bindata)
          };
          self._client.send(data, rp => {
            start += step;
            var blob = file.slice(start, start + step);
            reader.readAsArrayBuffer(blob);
          });
        } else {
          progress(file.size/**/);
          const data = {"rq": "restoreEnd"};
          self._client.send(data, (rp) => {
            const fail = rp["fail"];
            if (fail === "restore:unzip") {
              alert(_("Fail unzipping backup"));
            } else if (fail === "restore:version") {
              alert(_("File is not a Selectividad backup"));
            }
            self.run();
          });
        }
      }
    };

    function append() {
      var blob = file.slice(start, start + step);
      reader.readAsArrayBuffer(blob);
    }

    const data = {"rq": "restoreStart"};
    self._client.send(data, () => {
      append();
    });
  }

  /** @return {void} */
  clearTrash () {
    const self = this;
    const data = {"rq": "clearTrash"};
    self._client.send(data, () => {
      self.run();
    });
  }

  /**
   * @param {string} f
   * @return {void}
   */
  autorestore (f) {
    const self = this;
    const data = {"rq": "autorestore", "file": f};
    self._client.send(data, () => {
      self.run();
    });
  }

  /**
   * @param {string} f
   * @return {void}
   */
  restoreTrash (f) {
    const self = this;
    const data = {"rq": "restoreTrash", "file": f};
    this._client.send(data, () => {
      self.run();
    });
  }

// Bests -------------------------------------------------------------

  /** @param {function():void} f */
  updateBests (f) {
    const self = this;
    const data = {"rq": "bestsTime", "dbase": self._conf.dbase()};
    self._client.send(data, rq => {
      const time = rq["time"];
      if (time > self._bestsLastUpdate) {
        self._bestsLastUpdate = time;
        f();
      }
    });
  }

  /**
   * Changes the bests id
   * @param {string} id
   * @return {void}
   */
  setBestsId (id) {
    const self = this;
    self.conf().setSubpage(id);
    self.sendConf(() => { self.run2(); });
  }

// Statistics --------------------------------------------------------

  /** @param {function():void} f */
  updateStatistics (f) {
    const self = this;
    const data = {"rq": "bestsTime", "dbase": self._conf.dbase()};
    self._client.send(data, rq => {
      const time = rq["time"];
      if (time > self._bestsLastUpdate) {
        self._bestsLastUpdate = time;
        f();
      }
    });
  }

  /**
   * Changes the selectefd faly
   * @param {string} sel
   * @return {void}
   */
  setStatisticsSelection (sel) {
    const self = this;
    self.conf().setSubpage(sel);
    self.sendConf(() => { self.run2(); });
  }
}
new Main().run();

