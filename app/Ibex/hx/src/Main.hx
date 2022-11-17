// Copyright 21-Aug-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Js;
import dm.Opt;
import dm.Store;
import dm.Menu;
import data.Market;
import pgs.MsgPg;
import pgs.Authentication;
import pgs.Home;
import pgs.ResultsPg;
import pgs.SavingsPg;
import pgs.OrdersPg;
import pgs.RefsPg;
import pgs.Settings;
import I18n._;
import I18n._args;

/// Applicatoin entry.
class Main {
  static final HOME = "home";
  static final RESULTS = "results";
  static final SAVINGS = "savings";
  static final ORDERS = "orders";
  static final REFS = "refs";
  static final SETTINGS = "settings";

  static var lang: String;

  final wg: Domo;
  final market: Market;
  final body = Q("div");

  var menuSel = HOME;

  function new (wg: Domo, market: Market) {
    this.wg = wg;
    this.market = market;
  }

  // View ----------------------------------------------------------------------

  function show () {
    final menu = new Menu(
      [ Menu.toption(HOME, _("Home"), home),
        Menu.separator(),
        Menu.toption(RESULTS, _("Results"), results),
        Menu.separator(),
        Menu.toption(SAVINGS, _("Savings"), savings),
        Menu.separator(),
        Menu.toption(ORDERS, _("Orders"), orders),
        Menu.separator(),
        Menu.toption(REFS, _("Refs."), refs),
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
      case RESULTS: new ResultsPg(body, market).show();
      case SAVINGS: new SavingsPg(body, market).show();
      case ORDERS: new OrdersPg(body, market).show();
      case REFS: new RefsPg(body, market).show();
      case SETTINGS: new Settings(body, lang).show();
      default: Home.mk(body);
    }
  }

  // Control -------------------------------------------------------------------

  function home (): Void {
    menuSel = HOME;
    show();
  }

  function results (): Void {
    menuSel = RESULTS;
    show();
  }

  function savings (): Void {
    menuSel = SAVINGS;
    show();
  }

  function orders (): Void {
    menuSel = ORDERS;
    show();
  }

  function refs (): Void {
    menuSel = REFS;
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
        Cts.client.send([
          "source" => Js.ws("Main"),
          "rq" => Js.ws("idata")
        ], rp -> {
          final market = Market.fromJs(rp["market"]);
          new Main(wg, market).show();
          fn();
        });
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
