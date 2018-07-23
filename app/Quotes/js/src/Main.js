// Copyright 10-07-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

goog.provide("Main");

goog.require("github_dedeme");
goog.require("I18n");
goog.require("user_Expired");
goog.require("user_Auth");
goog.require("Bye");
goog.require("Settings");
goog.require("Backups");

goog.require("Nicks");

Main = class {
  /** @return {!Main} */
  constructor () {

    /** @private */
    this._client = new Client(
      Main.app(),
      () => { new user_Expired(this).show(); }
    );

  }

  /** @return {!Client} */
  client () {
    return this._client;
  }

  /** @return {string} */
  static app () {
    return "Quotes";
  }

  /** @return {string} */
  static version () {
    return "201807";
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

  /**
   * @return {void}
   */
  run () {
    const self = this;
    const client = self._client;
    client.connect(ok => {
      if (ok) {
        const data = {"page": "main", "rq": "idata"};
        client.send(data, rp => {
          rp["lang"] === "es" ? I18n.es() : I18n.en();
          switch (rp["menu"]) {
          case "settings" :
            self.show(rp["menu"], new Settings(self, rp["lang"]).mk());
            break;
          case "backups" :
            const bk = new Backups(self, rp["menu"]);
            bk.backups(backups => {
              bk.trash(trash => {
                self.show(rp["menu"], bk.mk(backups, trash));
              });
            });
            break;
          case "nicks" :
            const pg = new Nicks(self);
            pg.idata((d) => {
              self.show(rp["menu"], pg.mk(d))
              pg.initialFocus();
            });
            break;
          default:
            console.trace();
            throw("Page '" + rp["menu"] + "' is unknown");
          }
        });
      } else {
        new user_Auth(self).show();
      }
    });
  }

  /**
   * @private
   * @param {string} target
   * @return {void}
   */
  go (target) {
    const self = this;
    const client = self._client;
    const data = {"page": "main", "rq": "setMenu", "option": target};
    client.send(data, rp => {
      self.run();
    });
  }

  /**
   * @private
   * @return {void}
   */
  bye () {
    const self = this;
    const client = self._client;
    const data = {"page": "main", "rq": "bye"};
    client.send(data, rp => {
      self.showRoot(new Bye().mk());
    });
  }

/* View --------------------------------------------------------- */

  /**
   * @param {!Domo} o
   * @return {void}
   */
  showRoot (o) {
    $$("body").next().removeAll().add(
      $("div")
        .add(o)
        .add($("p").html("&nbsp;"))
        .add($("hr"))
        .add($("table").klass("main")
          .add($("tr")
            .add($("td")
              .add($("a")
                .att("href", "doc/about.html")
                .att("target", "blank")
                .html("<small>Help & Credits</small>")))
            .add($("td")
              .style("text-align: right;font-size: 10px;" +
                "color:#808080;font-size:x-small;")
              .html("- © ºDeme. " + Main.app() + " (" +
                Main.version() + ") -"))))
    );
  }

  /**
   * @param {string} page
   * @param {!Domo} o
   * @return {void}
   */
  show (page, o) {
    const self = this;
    function entry(id, target) {
      return Ui.link(ev => { self.go(target) })
        .klass(target === page ? "frame" : "link").html(id);
    }
    function separator() {
      return $("span").html(" · ");
    }

    const menu = $("table").klass("main").add($("tr")
      .add($("td")
        .add(entry(_("Nicks"), "nicks"))
        .add(separator())
        .add(entry(_("Reader"), "reader")))
      .add($("td").style("text-align:right")
        .add(entry(_("Backs"), "backups"))
        .add(separator())
        .add(entry(_("Settings"), "settings"))
        .add(separator())
        .add(Ui.link(ev => { self.bye(); })
          .add(Ui.img("cross").style("vertical-align:bottom")))))
    ;

    this.showRoot(
      $("div")
        .add(menu)
        .add($("hr"))
        .add(o)
    );
  }

}
new Main().run();

