// Copyright 2-Jan-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

goog.provide("Main");

goog.require("github_dedeme");
goog.require("I18n");
goog.require("Dom");
goog.require("user_Expired");
goog.require("user_Auth");
goog.require("user_Chpass");
goog.require("Conf");
goog.require("Db");
goog.require("Bye");
goog.require("paths_Control");
goog.require("index_Control");
goog.require("module_Control");
goog.require("code_Control");

Main = class {
  constructor () {
    /**
     * @private
     * @type {Conf}
     */
    this._conf = null;
    /** @private */
    this._client = new Client(
      Main.app(),
      () => { new user_Expired(this).show(); }
    );
    /** @private */
    this._dom = new Dom(this);
  }

  /** @return {string} */
  static app () {
    return "CDoc";
  }

  /** @return {string} */
  static version () {
    return "201803";
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

  /** @return {!Conf} */
  conf () {
    if (this._conf === null) {
      throw ("conf is null");
    }
    return this._conf;
  }

  /** @return {!Client} */
  client () {
    return this._client;
  }

  /** @return {!Dom} */
  dom () {
    return this._dom;
  }

  run () {
    const self = this;
    const client = self._client;
    client.connect(ok => {
      if (ok) {
        const data = {"page": "Main", "rq": "get"};
        client.send0(data, rp => {
          self._conf = Conf.restore(
            /** @type {!Array<?>} */ (JSON.parse(rp["conf"]))
          );
          if (self._conf.lang() === "es") I18n.es(); else I18n.en();

          const url = Ui.url();
          let path = url["0"];
          if (path === undefined) {
            path = self._conf.path();
            location.assign("?" + self._conf.path());
          } else {
            const data = {
              "page": "Main",
              "rq": "set",
              "conf": self._conf.serialize()
            };
            client.send0(data, rp => {
              if (path === "@") {
                this._client.setPageId();
                new paths_Control(self).run();
              } else if (path.indexOf("@") === -1) {
                new index_Control(self).run();
              } else {
                if (!url["1"]) {
                  new module_Control(self).run();
                } else {
                  new code_Control(self).run();
                }
              }
            });
          }
        });
      } else {
        new user_Auth(self, self._client).show();
      }
    });
  }

  // menu ------------------------------

  /**
   * @return {void}
   */
  bye () {
    const self = this;
    const data = {"rq": "logout"};
    self._client.send0(data, rp => { new Bye(self).show(); });
  }

  /**
   * @param {string} page
   * @return {void}
   */
  go (page) {
    const self = this;
//    self.conf().setPage(page);
//    self.sendConf(() => { self.run(); });
  }


  // settings --------------------------


}
new Main().run();

