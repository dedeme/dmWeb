// Copyright 30-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import Domo from "./dmjs/Domo.js"; //eslint-disable-line
import Client from "./dmjs/Client.js";
import Ui from "./dmjs/Ui.js";
import {Menu, MenuEntry} from "./dmjs/Menu.js"; //eslint-disable-line
import Maybe from "./dmjs/Maybe.js";
import Path from "./dmjs/Path.js";
import Msg from "./Msg.js";
import Authentication from "./Authentication.js";
import Settings from "./Settings.js";
import Doc from "./Doc.js";
import PLibRow from "./data/PLibRow.js"; //eslint-disable-line
import Cts from "./Cts.js";
import {I18n, _} from "./I18n.js";

const $ = e => Ui.$(e);

/**
    @private
**/
async function conf (client) {
  await client.send({
    "source": "Main",
    "rq": "setLcPath",
    "path": "@"
  });
  location.assign("?@");
}

/**
    @private
    @param {boolean} isPersonal
    @param {string} path
    @return {!MenuEntry}
**/
function mkLink (isPersonal, path) {
  const link = isPersonal ? path : "*" + path;
  return Menu.tlink(link, Path.base(path));
}

/**
    Main page.
**/
export default class Main {

  /**
      @private
      @param {!Client} client
      @param {!Maybe<!Domo>=} wgChild
      @param {!Maybe<!Domo>=} wgFocus
  **/
  constructor (client, wgChild = Maybe.nothing, wgFocus = Maybe.nothing) {
    this._client = client;
    this._wgChild = wgChild;
    this._wgFocus = wgFocus;

    if (wgChild.isNothing()) {
      const auth = new Authentication(Cts.app, client);
      this._wgChild = Maybe.just(auth.wg);
      this._wgFocus = Maybe.just(auth.pass);
    }

    this._wg = $("div");
    this.view();
  }

  /**
      @return {!Domo}
  **/
  get wg () {
    return this._wg;
  }

  /**
      @return {!Maybe<!Domo>}
  **/
  get wgFocus () {
    return this._wgFocus;
  }

  // View ----------------------------------------------------------------------

  /**
      @private
      @return {void}
  **/
  view () {
    this._wg.removeAll()
      .add(this._wgChild.fromJust());
  }

  // Static --------------------------------------------------------------------

  /**
      @return {!Promise<!Main>}
  **/
  static async mk () {
    const client = new Client(true, Cts.app, () => {
      const exp = new Msg(Cts.app, _("Session is expired."), true);
      $("@body").removeAll().add(exp.wg);
    });
    const /** boolean */ ok = await client.connect();
    if (ok) {
      const url = Object.values(Ui.url());
      const ilib = (url.length === 0) ? "" : url[0];

      const /** !Object<string, ?> */ rp = await client.send({
        "source": "Main",
        "rq": "idata",
        "lib": ilib
      });
      const /** string */ lib = rp["lib"];
      const /** string */ lang = rp["lang"];
      const /** !Array<!PLibRow> */ plibs = rp["plibs"]
        .map(e => PLibRow.fromJs(e));
      const pmlibs = plibs.filter(e => e.order > 0);
      const /** !Array<!PLibRow> */ slibs = rp["slibs"]
        .map(e => PLibRow.fromJs(e));
      const smlibs = slibs.filter(e => e.order > 0);

      if (lang === "en") I18n.en(); else I18n.es();

      const lopts = [
        Menu.ioption("@", "menu", () => { conf(client) }),
        Menu.separator2()
      ];

      let ls = pmlibs
        .sort((p1, p2) =>
          Path.base(p1.path).toUpperCase() > Path.base(p2.path).toUpperCase()
            ? 1
            : -1
        );
      let isFirst = true;
      ls.forEach(p => {
        if (isFirst) {
          isFirst = false;
        } else {
          lopts.push(Menu.separator());
        }
        lopts.push(mkLink(true, p.path));
      });

      const ropts = [];

      ls = smlibs
        .sort((p1, p2) =>
          Path.base(p1.path).toUpperCase() > Path.base(p2.path).toUpperCase()
            ? 1
            : -1
        );
      isFirst = true;
      ls.forEach(p => {
        if (isFirst) {
          isFirst = false;
        } else {
          ropts.push(Menu.separator());
        }
        ropts.push(mkLink(false, p.path));
      });

      ropts.push(Menu.separator2());
      ropts.push(Menu.ioption("@", "menu", () => { conf(client) }));

      const menu = new Menu(lopts, ropts, lib);

      let pg;
      if (lib === "@")
        pg = new Settings(client, lang, plibs, slibs);
      else
        pg = new Doc(lib);

      return new Main(
        client,
        Maybe.just($("div").add(menu.wg).add(pg.wg)),
        Maybe.nothing
      );
    }

    return new Main(client);
  }

  /**
      @return {!Promise<void>}
  **/
  static async show () {
    const main = await Main.mk();
    $("@body").removeAll().add(main.wg);
    main.wgFocus.ifElse(
      wg => { wg.e.focus() },
      () => {}
    );
  }

}
