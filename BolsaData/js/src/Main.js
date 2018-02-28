// Copyright 12-Nov-2017 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/**
 * Instalation requires pdfPrinter installed in /home/deme/bin
 */
goog.provide("Main");

goog.require("github_dedeme");
goog.require("I18n");
goog.require("Dom");
goog.require("Db");
goog.require("Reader");
goog.require("user_Expired");
goog.require("user_Auth");
goog.require("user_Chpass");
goog.require("view_Update");
goog.require("view_Create");
goog.require("view_Backups");
goog.require("view_Settings");
goog.require("view_Bye");

Main = class {
  constructor () {
    /** @private */
    this._client = new Client(
      Main.app(),
      () => { new user_Expired(this).show(); }
    );

    /**
     * @private
     * @type {!Domo}
     */
    this._waitDiv = $("span");
    /** @private */
    this._dom = new Dom(this);
    /** @private */
    this._db = null;
    /**
     * @private
     * @type {!Array<string>}
     */
    this._trash = [];
    /**
     * @private
     * @type {!Reader}
     */
    this._reader = new Reader(this);

  }

  /** @return {string} */
  static app () {
    return "BolsaData";
  }

  /** @return {string} */
  static version () {
    return "201801";
  }

  static invertia () {
    return "INVERTIA";
  }

  static infomercados () {
    return "INFOMERCADOS";
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

  /** @return {!Domo} */
  waitDiv () {
    return this._waitDiv;
  }

  /** @return {!Dom} */
  dom () {
    return this._dom;
  }

  /** @return {!Db} */
  db () {
    if (this._db) {
      return this._db;
    }
    throw ("Data base has not been created");
  }

  /** @return {!Array<string>} */
  trash () {
    return this._trash;
  }

  /** @return {!Reader} */
  reader () {
    return this._reader;
  }


  run () {
    const self = this;
    const client = self._client;

    function getNick(nick, f) {
      f(nick);
    };

    self._waitDiv.removeAll()
      .add($("img").style("width:12px;height:12px").att("src", "img/wait.gif"))
      .add($("span").html(" · "));
    client.connect(ok => {
      this._client.setPageId();
      if (ok) {
        let data = {"rq": "getDb"};
        client.send(data, rp => {
          self._trash = rp["trash"];
          self._db = Db.restore(
            /** @type {!Object<string, ?>} */ (JSON.parse(rp["db"])
          ));
          const db = self._db;

          db.language() === "es" ? I18n.es() : I18n.en();
          It.keys(db.invertiaId()).sync(
            (nick, f) => {
              let data = {"rq": "getQuotes", "nick": nick};
              client.send(data, rp => {
                let quotes = rp["quotes"];
                if (!quotes) {
                  quotes = "";
                }
                db.quotes()[nick] = It.from(rp["quotes"].split("\n"))
                  .map(q => Quote.from(q))
                  .to();
                f();
              })
            },
            () => {
              db.verify();
              self._waitDiv.removeAll();
              switch (db.page()) {
                case "update":
                  new view_Update(self).show();
                  break;
                case "create":
                  new view_Create(self).show();
                  break;
                case "backups":
                  new view_Backups(self).show();
                  break;
                case "settings":
                  new view_Settings(self).show();
                  break;
                default:
                  throw("Page '" + db.page() + "' is unknown");
              }
            }
          );
        });
      } else {
        new user_Auth(self, self._client).show();
      }
    });
  }

  // server ----------------------------

  /**
   * @param {function():void} f
   * @return {void}
   */
  sendDb (f) {
    const self = this;
    const db = self.db();
    self._waitDiv.removeAll()
      .add($("img").style("width:12px;height:12px").att("src", "img/wait.gif"))
      .add($("span").html(" · "));
    db.verify();
    const data = {"rq": "setDb", "db": JSON.stringify(db.serialize())};
    self._client.send(data, rp => {
      It.keys(db.invertiaId()).sync(
        (nick, fsync) => {
          const data = {
            "rq": "setQuotes",
            "nick": nick,
            "quotes": db.quotes()[nick].join("\n")
          };
          self._client.send(data, rp => { fsync(); });
        },
        () => {
            self._waitDiv.removeAll();
            f();
          }
      );
    });
  }

  /**
   * @param {string} target
   * @return {void}
   */
  go (target) {
    const self = this;
    self.db().setPage(target);
    self.sendDb(() => { self.run(); });
  }

  /**
   * @return {void}
   */
  bye () {
    const self = this;
    const data = {"rq" : "logout"};
    self._client.send(data, rp => { new view_Bye(self).show(); });
  }

  /** @return {void} */
  changeLanguage () {
    const self = this;
    const db = self._db;
    db.setLanguage(db.language() === "en" ? "es" : "en");
    self.sendDb(() => { self.run(); });
  }

  /** @return {void} */
  changeSource () {
    const self = this;
    const db = self._db;
    db.setSource(db.source() === Main.invertia()
      ? Main.infomercados()
      : Main.invertia()
    );
    self.sendDb(() => { self.run(); });
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
        self.run();
      } else {
        self.changePassPage();
      }
    });
  }

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
      this._client.send(data, () => {
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
          this._client.send(data, rp => {
            start += step;
            var blob = file.slice(start, start + step);
            reader.readAsArrayBuffer(blob);
          });
        } else {
          progress(file.size/**/);
          const data = {"rq": "restoreEnd"};
          this._client.send(data, (rp) => {
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
    this._client.send(data, () => {
      append();
    });
  }

  /** @return {void} */
  clearTrash () {
    const self = this;
    const data = {"rq": "clearTrash"};
    this._client.send(data, () => {
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

  /**
   * Only work with Main.invertia()
   * @param {string} nick
   * @param {string} invertiaKey
   * @param {string} infomercadosKey
   * @param {!Domo} counter A $("span")
   * @return {void}
   */
  create (nick, invertiaKey, infomercadosKey, counter) {
    const self = this;
    const db = self._db;

    function read(nPage) {
      counter.html("" + nPage);
      const data = {
        "rq" : "getSource",
        "url" : Reader.sourceUrl(Main.invertia(), invertiaKey, nPage)
      }
      self._client.send(data, rp => {
        if (rp["fail"] === "") {
          const err = self._reader.readCreate(nick, rp["page"]);
          if (err !== "") {
            alert(_args(_("Fail in page %0:\n%1"), "" + nPage, err));
            self.go("update");
          }
        } else {
          alert(_args(_("Fail in page %0:\n%1"), "" + nPage, rp["fail"]));
          self.go("update");
        }
        if (db.quotes()[nick].length >= Main.maxQuotes() + 50) {
          self.go("update");
        } else {
          read(nPage + 1);
        }
      });
    }

    if (nick === "") {
      alert(_("Nick is missing"));
      return;
    }
    if (invertiaKey === "") {
      alert(_("Invertia key is missing"));
      return;
    }
    if (infomercadosKey === "") {
      alert(_("Infomercados key is missing"));
      return;
    }
    if (It.keys(db.quotes()).contains(nick)) {
      if (!confirm(
        _args(_("Nick '%0' already exists.\nContinue?"), nick)
      )) {
        return;
      }
    }

    db.invertiaId()[nick] = invertiaKey;
    db.infomercadosId()[nick] = infomercadosKey;
    db.status()[nick] = "?";
    db.quotes()[nick] = [];

    read (1);
  }

  /**
   * Nick must exist in Main.invertia or in Main.infomercados.
   * @param {string} nick
   * @param {string} invertiaKey Can be ""
   * @param {string} infomercadosKey Can be ""
   * @return {void}
   */
  setKey (nick, invertiaKey, infomercadosKey) {
    const self = this;
    const db = self._db;

    if (nick === "") {
      alert(_("Nick is missing"));
      return;
    }
    if (invertiaKey === "") {
      if (infomercadosKey === "") {
        alert(_("Both keys are missing"));
        return;
      }
      if (db.invertiaId()[nick] === undefined) {
        alert(_args(
          _("Nick '%0' is not defined in %1"), nick, Main.infomercados()
        ));
        return;
      }
      db.infomercadosId()[nick] = infomercadosKey;
      const conf = confirm(_args(
        _("%0 wil be set:\n%1: %2\n%3: %4"),
        nick,
        Main.invertia(), db.invertiaId()[nick],
        Main.infomercados(), db.infomercadosId()[nick]
      ));
      if (!conf) {
        return;
      }
      self.go("update");
      return;
    }

    if (infomercadosKey !== "") {
      alert(_("One of key to set must be blank"));
      return;
    }
    if (db.infomercadosId()[nick] === undefined) {
      alert(_args(
        _("Nick '%0' is not defined in %1"), nick, Main.invertia()
      ));
      return;
    }
    db.invertiaId()[nick] = invertiaKey;
    const conf = confirm(_args(
      _("%0 wil be set:\n%1: %2\n%3: %4"),
      nick,
      Main.invertia(), db.invertiaId()[nick],
      Main.infomercados(), db.infomercadosId()[nick]
    ));
    if (!conf) {
      return;
    }
    self.go("update");
  }

  /**
   * @param {string} nick
   * @param {number} pages Number of pages to read
   * @param {!Domo} counter
   * @return {void}
   */
  update (nick, pages, counter) {
    if (nick === "") {
      alert(_("Nick is missing"));
      return;
    }
    const self = this;
    const db = self._db;
    const key = db.source() === Main.invertia()
      ? db.invertiaId()[nick]
      : db.infomercadosId()[nick]
    ;
    if (key === undefined) {
      alert(_("Key is missing"));
      return;
    }
    let newQs = [];

    function read(nPage) {
      counter.html(nick + ": " + nPage);
      const data = {
        "rq" : "getSource",
        "url" : Reader.sourceUrl(db.source(), key, nPage)
      }
      self._client.send(data, rp => {
        if (rp["fail"] === "") {
          const tp = Reader.readPage(db.source(), rp["page"]);
          const err = tp.e1();
          if (err !== "") {
            alert(_args(_("Fail in page %0:\n%1"), "" + nPage, err));
            self.go("update");
          } else {
            newQs = It.from(newQs).addIt(It.from(tp.e2())).to();
            if (nPage >= pages) {
              const oldQs = db.quotes()[nick] || [];
              const firstQ = oldQs.length > 0 ? oldQs[0].date() : "20000101";
              db.quotes()[nick] = It.from(newQs)
                .takeWhile(q => q.date() > firstQ)
                .addIt(It.from(oldQs))
                .take(Main.maxQuotes())
                .to();
              self.go("update");
            } else {
              read(nPage + 1);
            }
          }
        } else {
          alert(_args(_("Fail in page %0:\n%1"), "" + nPage, rp["fail"]));
          self.go("update");
        }
      });
    }

    db.status()[nick] = "?";

    read (1);
  }

  /**
   * @param {number} pages Number of pages to read
   * @param {!Domo} counter
   * @return {void}
   */
  updateAll (pages, counter) {
    const self = this;
    const db = self._db;

    const keysMap = db.source() === Main.invertia()
      ? db.invertiaId()
      : db.infomercadosId()
    ;
    It.keys(keysMap).sort().sync(
      (nick, f) => {
          const key = keysMap[nick];
          let newQs = [];
          function read(nPage) {
            counter.html(nick + ": " + nPage);
            const data = {
              "rq" : "getSource",
              "url" : Reader.sourceUrl(db.source(), key, nPage)
            }
            self._client.send(data, rp => {
              if (rp["fail"] === "") {
                const tp = Reader.readPage(db.source(), rp["page"]);
                const err = tp.e1();
                if (err !== "") {
                  alert(_args(_("Fail in page %0:\n%1"), "" + nPage, err));
                  self.go("update");
                } else {
                  newQs = It.from(newQs).addIt(It.from(tp.e2())).to();
                  if (nPage >= pages) {
                    const oldQs = db.quotes()[nick] || [];
                    const firstQ = oldQs.length > 0
                      ? oldQs[0].date()
                      : "20000101";
                    db.quotes()[nick] = It.from(newQs)
                      .takeWhile(q => q.date() > firstQ)
                      .addIt(It.from(oldQs))
                      .take(Main.maxQuotes())
                      .to();
                    f();
                  } else {
                    read(nPage + 1);
                  }
                }
              } else {
                alert(_args(_("Fail in page %0:\n%1"), "" + nPage, rp["fail"]));
                self.go("update");
              }
            });
          }
        db.status()[nick] = "?";
        read (1);
      },
      () => { self.go("update"); }
    );
  }

  /**
   * @param {boolean} value
   * @return {void}
   */
  quoteTranslator (value) {
    this._db.setQuoteTranslator(value);
    this.go("update");
  }

  /**
   * @param {string} nick
   * @return {void}
   */
  setModel (nick) {
    this._db.setModel(nick);
    this.go("update");
  }

  /**
   * @param {string} nick
   * @param {boolean} value 'true' adds nick to ibex, 'false' removes it.
   * @return {void}
   */
  ibex (nick, value) {
    this._db.ibex()[nick] = value;
    this.go("update");
  }

  /**
   * @param {string} nick
   * @return {void}
   */
  del (nick) {
    delete this._db.invertiaId()[nick];
    delete this._db.ibex()[nick];
    delete this._db.status()[nick];
    this.go("update");
  }

  /**
   * @param {string} nick
   * @param {string} text
   * @return {void}
   */
  modify (nick, text) {
    this._db.quotes()[nick] = It.from(text.split("\n"))
      .map(q => Quote.from(q))
      .to();
    this.go("update");
  }

}
new Main().run();
