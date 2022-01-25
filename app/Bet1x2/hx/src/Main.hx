// Copyright 12-Dic-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Js;
import dm.Opt;
import dm.Store;
import dm.Menu;
import data.Result;
import data.Bet;
import pgs.MsgPg;
import pgs.Authentication;
import pgs.Home;
import pgs.Standings;
import pgs.PointsPg;
import pgs.BetsPg;
import pgs.DocPg;
import pgs.Settings;
import I18n._;
import I18n._args;

/// Applicatoin entry.
class Main {
  static final HOME = "home";
  static final STANDINGS = "standings";
  static final POINTS = "points";
  static final BETS = "bets";
  static final DOC = "doc";
  static final SETTINGS = "settings";

  static var lang: String;

  final wg: Domo;
  final body = Q("div");

  var menuSel = HOME;


  function new (wg: Domo) {
    this.wg = wg;
  }

  // View ----------------------------------------------------------------------

  function show () {
    final menu = new Menu(
      [ Menu.toption(HOME, _("Home"), home),
        Menu.separator2(),
        Menu.toption(STANDINGS, _("Standings"), standings),
        Menu.separator(),
        Menu.toption(POINTS, _("Points"), pointsPg),
        Menu.separator(),
        Menu.toption(BETS, _("Bets"), betsPg),
        Menu.separator2(),
        Menu.toption(DOC, _("Documentation"), docPg)
      ],
      [ Menu.toption(SETTINGS, _("Settings"), settings),
        Menu.separator(),
        Menu.close(close)
      ],
      menuSel
    );

    wg
      .removeAll()
      .add(menu.wg)
      .add(body)
    ;

    switch (menuSel) {
      case HOME: Home.mk(body);
      case STANDINGS: Standings.mk(body);
      case POINTS: PointsPg.mk(body);
      case BETS: BetsPg.mk(body);
      case DOC: DocPg.mk(body);
      case SETTINGS: new Settings(body, lang).show();
      default: Home.mk(body);
    }
  }

  // Control -------------------------------------------------------------------

  function home (): Void {
    menuSel = HOME;
    show();
  }

  function standings (): Void {
    menuSel = STANDINGS;
    show();
  }

  function pointsPg (): Void {
    menuSel = POINTS;
    show();
  }

  function betsPg (): Void {
    menuSel = BETS;
    show();
  }

  function docPg (): Void {
    menuSel = DOC;
    show();
  }

  function settings (): Void {
    menuSel = SETTINGS;
    show();
  }

  function close (): Void {
    if (!Ui.confirm(_("Application exit?"))) {
      return;
    }
    Cts.client.send([
      "source" => Js.ws("Main"),
      "rq" => Js.ws("close"),
      "sessionId" => Js.ws(Cts.client.sessionId())
    ], rp -> {
      new MsgPg(wg , _args(_("Logout-message"), [Cts.appName]), false).show();
    });
  }

  // Static --------------------------------------------------------------------

  static function mk (wg: Domo, fn: Void -> Void) {
    Cts.client.connect(ok -> {
      if (ok) {
        new Main(wg).show();
        fn();
      } else {
        new Authentication(wg, Cts.appName, () -> mk(wg, fn));
        fn();
      }
    });
  }

  /// Application entry.
  static public function main (): Void {
    lang = switch(Store.get(Cts.langKey)) {
        case Some(l): l;
        case None: "es";
      };
    if (lang == "es") I18n.es();
    else I18n.en();

    var wg = Q("div");
    mk(wg, () -> {
      Q("@body")
        .removeAll()
        .add(wg)
        .add(Cts.foot)
        .add(Ui.upTop("up"))
      ;

      var fc = Q("#autofocus");
      if (fc.e != null) fc.e.focus();
    });
  }
}
