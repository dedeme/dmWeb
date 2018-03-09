// Copyright 06-Mar-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/**
 * Instalation requires pdfPrinter installed in /home/deme/bin
 */
goog.provide("Main");

goog.require("github_dedeme");
goog.require("Dom");
goog.require("user_Expired");
goog.require("user_Auth");
goog.require("I18n");
goog.require("Bye");
goog.require("portfolio_View");

Main = class {
  constructor () {
    /** @private */
    this._client = new Client(
      Main.app(),
      () => { new user_Expired(this).show(); }
    );

    /** @private */
    this._dom = new Dom(this);

    /** @private */
    this._conf = null;
  }

  /** @return {string} */
  static app () {
    return "Mconta";
  }

  /** @return {string} */
  static version () {
    return "201803";
  }

  /** @return {!Dom} */
  dom () {
    return this._dom;
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

  /**
   * @return {void}
   */
  run () {
    const self = this;

    self._client.connect(ok => {
      if (ok) {
        const data = {"pg": "Main", "rq": "conf"};
        self._client.send(data, rp => {
          rp["lang"] === "es" ? I18n.es() : I18n.en();
          switch (rp["pg"]) {
            case "portfolio":
            case "accounting":
              switch (rp["pg2"]) {
                case "view":
                case "balance":
                  new portfolio_View(self).run();
                  break;
                default : throw(_args(
                    _("Subpage '%0' is unknown in %1"), rp["pg2"], rp["pg"]
                  ));
              }
              break;
            default : throw(_args(_("Page '%0' is unknown"), rp["pg"]));
          }

        });
      } else {
        new user_Auth(self, self._client).show();
      }
    });
  }

  /**
   * @param {string} page
   * @return {void}
   */
  go (page) {
    const self = this;
    if (page == "bye") {
      new Bye(self).run();
    } else {
      const subpage = page === "accounting" ? "balance"
        : page === "portfolio" ? "view"
        : page === "market" ? "" // "orders"
        : page === "backup" ? "" // "-"
        : page === "settings" ? "" // "settings"
        : "";
      if (subpage === "") {
        throw(_args(_("Pagex '%0' is unknown"), page));
      }
      const data = {
        "pg" : "Main",
        "rq" : "go",
        "page" : page,
        "subpage" : subpage
      };
      self._client.send(data, rp => {
        self.run();
      });
    }
  }
}
new Main().run();

