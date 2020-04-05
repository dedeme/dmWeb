// Copyright 27-Feb-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import Domo from "./dmjs/Domo.js"; //eslint-disable-line
import Client from "./dmjs/Client.js";
import Ui from "./dmjs/Ui.js";
import Maybe from "./dmjs/Maybe.js";
import Defs from "./Defs.js";
import Msg from "./Msg.js";
import Authentication from "./Authentication.js";
import Path from "./data/Path.js";
import MainMain from "./pgs/main/MainMain.js";
import {_} from "./I18n.js";

const $ = e => Ui.$(e);

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
      const auth = new Authentication(Defs.app, client);
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
            .html(`- © ºDeme. ${Defs.app} (${Defs.version}) -`))))
      .add(Ui.upTop("up"));
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
    const client = new Client(true, Defs.app, () => {
      const exp = new Msg(Defs.app, _("Session is expired."), true);
      $("@body").removeAll().add(exp.wg);
    });
    const /** boolean */ ok = await client.connect();
    if (ok) {
      const pg = await MainMain.mk(client, Path.mkRoot());
      return new Main(client, Maybe.just(pg.wg), pg.afterShow);
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
