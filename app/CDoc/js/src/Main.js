// Copyright 18-Aug-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import Domo from "./dmjs/Domo.js"; //eslint-disable-line
import Client from "./dmjs/Client.js";
import Ui from "./dmjs/Ui.js";
import It from "./dmjs/It.js";
import Menu from "./dmjs/Menu.js";
import Expired from "./core/Expired.js";
import Auth from "./core/Auth.js";
import {I18n, _, _args} from "./I18n.js";
import Bye from "./core/Bye.js";

import Conf from "./data/Conf.js";
import Dpath from "./data/Dpath.js";
import Paths from "./Paths.js";
import Index from "./Index.js";
import Module from "./Module.js";
import Code from "./Code.js";

const app = "CDoc";
const version = "201909";
const langStore = `${app}__lang`;
const captchaAuthStore = `${app}__captcha`;
const captchaChpassStore = `${app}__captchaCh`;

const $ = e => Ui.$(e);

let client = null;

/**
    Main page.
**/
export default class Main {

  constructor () {
    client = new Client(true, app, () => {
      new Expired(this).show();
    });
    /**
        @type {!Conf}
    **/
    this._conf = new Conf("@", "es", true);

    this._menu = $("div");
    this._credits = $("a")
      .att("href", "doc/about.html")
      .att("target", "blank");
    this._view = $("div");
  }

  /**
      @return {!Conf}
  **/
  get conf () {
    return this._conf;
  }

  /**
      @return {!Domo}
  **/
  get view () {
    return this._view;
  }

  // View ----------------------------------------------------------------------

  /**
      @private
      @return {!Domo}
  **/
  get wg () {
    return $("div")
      .add(this._menu)
      .add(this._view)
      .add($("p").html("&nbsp;"))
      .add($("hr"))
      .add($("table").klass("main")
        .add($("tr")
          .add($("td").add(this._credits))
          .add($("td")
            .style("text-align: right;font-size: 10px;" +
              "color:#808080;font-size:x-small;")
            .html(`- © ºDeme. ${Main.app} (${Main.version}) -`))))
    ;
  }

  /**
      @return {void}
  **/
  show () {
    $("@body").removeAll().add(this.wg);
    this.update();
  }

  // Control -------------------------------------------------------------------

  bye () {
    if (!confirm(_("Application exit?"))) {
      return;
    }
    const rq = {
      "page": "Main",
      "rq": "setPath",
      "option": Ui.url()["0"] || "@"
    };
    Main.client.rq(rq);
    const rq2 = {
      "page": "logout"
    };
    Main.client.rq(rq2);

    new Bye(this).show();
  }

  /**
      @return {!Promise}
  **/
  async update () {
    const self = this;

    async function go () {
      const rq = {
        "page": "Main",
        "rq": "idata"
      };

      const /** !Object<string, !Array<?>> */ rp = await Main.client.rq(rq);
      self._conf = Conf.fromJs(rp["conf"]);
      const /** !Array<Dpath> */ dpaths = rp["paths"].map(Dpath.fromJs);

      if (self.conf.lang === "en") I18n.en();
      else I18n.es();

      self._credits.html("<small>" + _("Help & Credits") + "</small>");

      const url = Ui.url();
      const /** string */ page = url["0"] || self.conf.path;

      const menu = new Menu();
      menu.addLeft(
        $("a").att("href", Main.urlBase + "?@").att("id", "menu_@")
          .add(Ui.img("asterisk").style("vertical-align:middle"))
      );
      It.from(dpaths).filter(p =>
        p.valid && p.show
      ).sort((p1, p2) =>
        p1.id.toUpperCase() > p2.id.toUpperCase() ? 1 : -1
      ).each(p => {
        menu.addLeft(Menu.separator());
        menu.addLeft(Menu.mkLink(p.id, p.id));
      });
      menu.addRight(Menu.mkClose(() => self.bye()));
      self._menu.removeAll().add(menu.wg);

      if (page === "@") {
        menu.setSelected(page);
        new Paths(self).show(dpaths);
      } else {
        const ix = page.indexOf("@");
        const mp = ix === -1 ? [page, null] : page.split("@");
        if (mp.length > 2) {
          alert(_args(_("'%0': url with 2 or more '@'"), page));
          location.assign(Main.urlBase);
        }
        const [module, path] = [mp[0], mp[1]];
        menu.setSelected(module);
        if (url["1"]) {
          new Code(self).show(module, path, url["1"]);
        } else if (path === null) {
          new Index(self).show(module);
        } else {
          new Module(self).show(module, path);
        }
      }
    }

    const url = Ui.url();
    const /** string */ page = url["0"] || "@";

    if (page === "@") {
      const /** boolean */ ok = await Main.client.connect();
      if (ok) go();
      else new Auth(this).show();
    } else {
      go();
    }
  }

  // Static --------------------------------------------------------------------

  /**
      @return {string }
  **/
  static get urlBase () {
    let path = window.location.href;
    const ix = path.indexOf("?");
    path = ix === -1 ? path : path.substring(0, ix);
    return path;
  }

  /**
      @return {string} Application name
  **/
  static get app () {
    return app;
  }

  /**
      @return {string} Application version
  **/
  static get version () {
    return version;
  }

  /**
      @return {string} Key for language data store
  **/
  static get langStore () {
    return langStore;
  }

  /**
      @return {string} Key for authentication captcha data store
  **/
  static get captchaAuthStore () {
    return captchaAuthStore;
  }

  /**
      @return {string} Key for change pass captcha data store
  **/
  static get captchaChpassStore () {
    return captchaChpassStore;
  }

  /**
      @return {!Client}
  **/
  static get client () {
    if (client === null) {
      throw new Error("Client is not initialized");
    }
    return client;
  }

}
