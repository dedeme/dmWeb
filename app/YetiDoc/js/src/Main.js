// Copyright 27-Feb-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import Domo from "./dmjs/Domo.js"; //eslint-disable-line
import Client from "./dmjs/Client.js";
import Ui from "./dmjs/Ui.js";
import Maybe from "./dmjs/Maybe.js";
import {Menu} from "./dmjs/Menu.js";
import Msg from "./Msg.js";
import Authentication from "./Authentication.js";
import {I18n, _, _args} from "./I18n.js";
import PathEntry from "./data/PathEntry.js";

import Cts from "./Cts.js";
import Paths from "./Paths.js";
import Index from "./Index.js";
import Module from "./Module.js";
import Code from "./Code.js";

const $ = e => Ui.$(e);

/**
    @private
    @param {string} target
**/
function go (target) {
  location.assign("?" + target);
}

/**
    @private
    @param {!Client} client
**/
function bye (client) {
  if (!confirm(_("Application exit?"))) {
    return;
  }
  client.send({
    "source": "Main",
    "rq": "bye",
    "sessionId": client.sessionId()
  });

  const pg = new Msg(Cts.app, _args(_("Logout-message"), Cts.app));
  $("@body").removeAll().add(pg.wg);
}

/**
    Main page.
**/
export default class Main {

  /**
      @private
      @param {!Client} client
      @param {!Maybe<!Domo>=} wgChild
      @param {!Maybe<function():void>=} afterShow
  **/
  constructor (client, wgChild = Maybe.nothing, afterShow = Maybe.nothing) {
    this._client = client;
    this._wgChild = wgChild;
    this._afterShow = afterShow;

    if (wgChild.isNothing()) {
      const auth = new Authentication(Cts.app, client);
      this._wgChild = Maybe.just(auth.wg);
      this._afterShow = Maybe.just(() => auth.pass.e.focus());
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

  // View ----------------------------------------------------------------------

  /**
      @private
      @return {void}
  **/
  view () {
    this._wg.removeAll()
      .add(this._wgChild.fromJust())
      .add($("p").html("&nbsp;"))
      .add($("hr"))
      .add($("table").klass("main")
        .add($("tr")
          .add($("td")
            .add($("a")
              .att("href", "doc/about.html")
              .att("target", "blank")
              .html("<small>" + _("Help & Credits") + "</small>")))
          .add($("td")
            .style("text-align: right;font-size: 10px;" +
              "color:#808080;font-size:x-small;")
            .html(`- © ºDeme. ${Cts.app} (${Cts.version}) -`))));
    this.afterShow();
  }

  // Control -------------------------------------------------------------------

  /**
      Set focus
  **/
  afterShow () {
    if (this._afterShow.isJust()) this._afterShow.fromJust()();
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
      const /** !Object<string, ?> */ rp = await client.send({
        "source": "Main",
        "rq": "idata"
      });
      const /** !Array<string> */ lcPath = rp["lcPath"];
      const /** string */ lang = rp["lang"];
      const /** boolean */ showAll = rp["showAll"];
      const /** !Array<!PathEntry> */ paths =
        rp["paths"].map(e => PathEntry.fromJs(e));

      if (lang === "en") I18n.en(); else I18n.es();

      let url = Object.values(Ui.url());
      if (url.length === 0) url = lcPath;
      await client.send({
        "source": "Main",
        "rq": "setLcPath",
        "path": url
      });

      const lib = url[0];

      const lopts = [
        Menu.ioption("@", "asterisk", () => { go("@") })
      ];

      const ls = paths
        .filter(p => p.selected && p.exists)
        .sort((p1, p2) =>
          p1.name.toUpperCase() > p2.name.toUpperCase() ? 1 : -1
        );
      ls.forEach(p => {
        lopts.push(Menu.separator());
        lopts.push(Menu.tlink(p.name, p.name));
      });

      const ropts = [
        Menu.close(() => { bye(client) })
      ];
      const menu = new Menu(lopts, ropts, lib);

      const defaultMain = () => {
        const pg = new Paths(Cts.app, client, lang, showAll, paths);
        return new Main(
          client,
          Maybe.just($("div").add(menu.wg).add(pg.wg)),
          Maybe.just(() => { pg.wgNewName.e.focus() })
        );
      };

      if (lib === "@" || lib === "") return defaultMain();

      const path = paths.find(p => p.name === lib);
      if (path === undefined) return defaultMain();

      if (url.length === 1) {
        const pg = await Index.mk(client, lib, path.path);
        if (pg.isNothing()) return defaultMain();

        return new Main(
          client,
          Maybe.just($("div").add(menu.wg).add(pg.fromJust().wg)),
          Maybe.nothing
        );
      }

      if (url.length === 2) {
        const mod = url[1];
        if (mod === "") return defaultMain();
        const pg = await Module.mk(
          client, lib, path.path, mod
        );
        if (pg.isNothing()) return defaultMain();

        return new Main(
          client,
          Maybe.just($("div").add(menu.wg).add(pg.fromJust().wg)),
          Maybe.nothing
        );
      }

      if (url.length === 3) {
        const mod = url[1];
        if (mod === "") return defaultMain();
        const link = url[2];
        const pg = await Code.mk(
          client, lib, mod, link, path.path + "/" + mod + ".yeti"
        );
        if (pg.isNothing()) return defaultMain();

        return new Main(
          client,
          Maybe.just($("div").add(menu.wg).add(pg.fromJust().wg)),
          Maybe.just(() => pg.fromJust().jump())
        );
      }

      return defaultMain();
    }

    return new Main(client);
  }

  /**
      @return {!Promise<void>}
  **/
  static async show () {
    const main = await Main.mk();
    $("@body").removeAll().add(main.wg);
    main.afterShow();
  }

}
