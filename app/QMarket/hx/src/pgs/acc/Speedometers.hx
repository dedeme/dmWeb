// Copyright 14-Nov-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pgs.acc;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Js;
import dm.It;
import dm.Str;
import dm.Dec;
import dm.Menu;
import dm.Speedometer;
import data.ProfitsEntry;
import data.Cts;
import I18n._;
import pgs.acc.wgs.ProfitsWg;

/// Accounting speedometers.
class Speedometers {
  static final NO_ORDER = 0;
  static final ASC = 1;
  static final DESC= -1;

  var wg: Domo;
  var data: Data;
  final mSel: Int;

  // Order 0: Nick, 1: Losses, 2: Risk
  final ciasOrder = [ASC, NO_ORDER, NO_ORDER];

  function new (wg: Domo, mSel: Int, data: Data) {
    this.wg = wg;
    this.data = data;
    this.mSel = mSel;

    view();
  }

  // View ----------------------------------------------------------------------

  function view () {
    final wg = Q("div");
    final data = data;
    final sel = mSel == -1 ? "All" : _("Inv-") + Std.string(mSel);

    final lopts = [
      Menu.toption("All", _("All"), () -> setMenu(-1))
    ];
    for (i in 0...Cts.qlevels) {
      final op = _("Inv-") + Std.string(i);
      lopts.push(i == 0 ? Menu.separator2() : Menu.separator());
      lopts.push(Menu.toption(op, op, () -> setMenu(i)));
    }
    lopts.push(Menu.separator2());
    lopts.push(Menu.toption("Cias", _("Companies"), () -> setMenu(-2)));
    final ropts = [];
    final menu = new Menu(lopts, ropts, sel);

    var d = [];
    if (mSel == -2) {
      addStocks(wg);
    } else {
      addInvestor(wg);
    }

    this.wg
      .removeAll()
      .add(menu.wg)
      .add(wg)
    ;
  }

  function addInvestor(wg: Domo) {
    final e = mSel == -1
      ? InvestorEntry.sum(data.getInvestors())
      : data.getInvestors()[mSel]
    ;

    wg
      .add(Q("div")
        .klass("head")
        .text(_("Global")))
      .add(Q("table")
        .att("align", "center")
        .add(Q("tr")
          .add(Q("td")
            .klass("header")
            .text(_("Losses")))
          .add(Q("td")
            .klass("header")
            .text(_("Risk"))))
        .add(Q("tr")
          .add(Q("td")
            .klass("frame")
            .add(new Speedometer(
                acumulatorAvg(e.currentProfits, e.active),
                0.6
              ).wg))
          .add(Q("td")
            .klass("frame")
            .add(new Speedometer(
                acumulatorAvg(e.riskProfits, e.active),
                0.6
              ).wg))))
      .add(Q("div")
        .klass("head")
        .text(_("Stocks")))
      .add(Q("table")
        .att("align", "center")
        .add(Q("tr")
          .add(Q("td")
            .klass("header")
            .text(_("Losses")))
          .add(Q("td")
            .klass("header")
            .text(_("Risk"))))
        .add(Q("tr")
          .add(Q("td")
            .klass("frame")
            .add(new Speedometer(
                acumulatorAvg(e.stocksCurrentProfits, e.stocksAccountValue),
                0.6
              ).wg))
          .add(Q("td")
            .klass("frame")
            .add(new Speedometer(
                acumulatorAvg(e.stocksRiskProfits, e.stocksAccountValue),
                0.6
              ).wg))))
    ;

  }

  function addStocks (wg: Domo) {
    final cias = data.getCompanies();
    if (ciasOrder[0] == ASC) {
      CompanyEntry.sort(cias, "nick", true);
    } else if (ciasOrder[0] == DESC) {
      CompanyEntry.sort(cias, "nick", false);
    } else if (ciasOrder[1] == ASC) {
      CompanyEntry.sort(cias, "losses", true);
    } else if (ciasOrder[1] == DESC) {
      CompanyEntry.sort(cias, "losses", false);
    } else if (ciasOrder[2] == ASC) {
      CompanyEntry.sort(cias, "risk", true);
    } else if (ciasOrder[2] == DESC) {
      CompanyEntry.sort(cias, "risk", false);
    } else {
      throw new haxe.Exception("'ciasOrder' has a not valid value");
    }
    final sm = CompanyEntry.sum(cias);
    wg
      .add(Q("div")
        .klass("head")
        .text(_("Summary")))
      .add(Q("table")
        .att("align", "center")
        .add(Q("tr")
          .add(Q("td")
            .klass("header")
            .text(_("Value")))
          .add(Q("td")
            .klass("header")
            .text(_("Losses")))
          .add(Q("td")
            .klass("header")
            .text(_("Risk"))))
        .add(Q("tr")
          .add(Q("td")
            .klass("number")
            .text(Dec.toIso(sm.stocksAccountValue, 2)))
          .add(Q("td")
            .klass("frame")
            .add(new Speedometer(
                acumulatorAvg(sm.stocksCurrentProfits, sm.stocksAccountValue),
                0.4
              ).wg))
          .add(Q("td")
            .klass("frame")
            .add(new Speedometer(
                acumulatorAvg(sm.stocksRiskProfits, sm.stocksAccountValue),
                0.4
              ).wg))))
      .add(Q("div")
        .klass("head")
        .text(_("Companies")))
      .add(Q("table")
        .att("align", "center")
        .add(Q("tr")
          .add(Q("td")
            .klass("header")
            .add(Ui.link(ev -> setOrder("nick"))
              .klass("link")
              .text("Nick")))
          .add(Q("td")
            .klass("header")
            .text(_("Value")))
          .add(Q("td")
            .klass("header")
            .add(Ui.link(ev -> setOrder("losses"))
              .klass("link")
              .text(_("Losses"))))
          .add(Q("td")
            .klass("header")
            .add(Ui.link(ev -> setOrder("risk"))
              .klass("link")
              .text(_("Risk")))))
        .adds(It.from(cias)
          .map(e ->
            Q("tr")
              .add(Q("td")
                .klass("border")
                .text(e.nick))
              .add(Q("td")
                .klass("number")
                .text(Dec.toIso(e.stocksAccountValue, 2)))
              .add(Q("td")
                .klass("frame")
                .add(new Speedometer(
                    acumulatorAvg(e.stocksCurrentProfits, e.stocksAccountValue),
                    0.25
                  ).wg))
              .add(Q("td")
                .klass("frame")
                .add(new Speedometer(
                    acumulatorAvg(e.stocksRiskProfits, e.stocksAccountValue),
                    0.25
                  ).wg))
          ).to()
        ))
    ;
  }

  // Control -------------------------------------------------------------------

  function setMenu (managerSel: Int) {
    mk(wg, managerSel);
  }

  function setOrder (field: String) {
    if (field == "nick") {
      if (ciasOrder[0] == ASC) ciasOrder[0] = DESC;
      else ciasOrder[0] = ASC;
      ciasOrder[1] = NO_ORDER;
      ciasOrder[2] = NO_ORDER;
    } else if (field == "losses") {
      if (ciasOrder[1] == ASC) ciasOrder[1] = DESC;
      else ciasOrder[1] = ASC;
      ciasOrder[0] = NO_ORDER;
      ciasOrder[2] = NO_ORDER;
    } else if (field == "risk") {
      if (ciasOrder[2] == ASC) ciasOrder[2] = DESC;
      else ciasOrder[2] = ASC;
      ciasOrder[0] = NO_ORDER;
      ciasOrder[1] = NO_ORDER;
    } else {
      throw new haxe.Exception("'field' (" + field + ") has a not valid value");
    }

    view();
  }

  // Static --------------------------------------------------------------------

  /// Constructor.
  ///   wg: Container.
  ///   mSel: Manager (investor) selected.
  ///         Especial values:
  ///           -1: All of the investors.
  ///           -2: Companies.
  ///         Default '-1'.
  public static function mk (wg: Domo, mSel = -1) {
    if (mSel == -2) {
      Cts.client.send([
        "module" => Js.ws("acc"),
        "source" => Js.ws("speedometers"),
        "rq" => Js.ws("companiesData")
      ], rp -> {
        final data = Data.fromJs(rp["data"]);
        new Speedometers(wg, mSel, data);
      });
    } else {
      Cts.client.send([
        "module" => Js.ws("acc"),
        "source" => Js.ws("speedometers"),
        "rq" => Js.ws("investorsData")
      ], rp -> {
        final data = Data.fromJs(rp["data"]);
        new Speedometers(wg, mSel, data);
      });
    }
  }

  /// Calculates a value for speedometer of profits / (profits + invest)
  public static function acumulatorAvg (profits: Float, invest: Float): Float {
    var r = 0.5 - profits / (2 * profits + 4 * invest);
    if (r > 0.5) {
      r = 0.5 + (r - 0.5) * 4;
      if (r > 1) r = 1;
    } else if (r < 0.5) {
      r = 0.5 - (0.5 - r) * 4;
      if (r < 0) r = 0;
    }
    return r;
  }

  static function investorProfits (
    currentProfits: Float, active: Float
  ): Float {
    return acumulatorAvg(currentProfits, active);
  }

  static function investorRisk (
    riskProfits: Float, active: Float
  ): Float {
    return acumulatorAvg(riskProfits, active);
  }
}

class Data {
  public final isInvestor: Bool;
  public final data: Js;

  function new (isInvestor: Bool, data: Js) {
    this.isInvestor = isInvestor;
    this.data = data;
  }

  public function getInvestors (): Array<InvestorEntry> {
    if (!isInvestor) {
      throw new haxe.Exception("Data is not from an investor");
    }

    return data.ra().map(e -> InvestorEntry.fromJs(e));
  }

  public function getCompanies (): Array<CompanyEntry> {
    if (isInvestor) {
      throw new haxe.Exception("Data is not from a company");
    }

    return data.ra().map(e -> CompanyEntry.fromJs(e));
  }

  static public function fromJs (js:  Js): Data {
    final a = js.ra();
    return new Data(
      a[0].rb(),
      a[1]
    );
  }
}

class InvestorEntry {
  public final currentProfits: Float;
  public final riskProfits: Float;
  public final active: Float;
  public final stocksCurrentProfits: Float;
  public final stocksRiskProfits: Float;
  public final stocksAccountValue: Float;

  function new (
    currentProfits: Float, riskProfits: Float, active: Float,
    stocksCurrentProfits: Float, stocksRiskProfits: Float,
    stocksAccountValue: Float
  ) {
    this.currentProfits = currentProfits;
    this.riskProfits = riskProfits;
    this.active = active;
    this.stocksCurrentProfits = stocksCurrentProfits;
    this.stocksRiskProfits = stocksRiskProfits;
    this.stocksAccountValue = stocksAccountValue;
  }

  public static function fromJs (js: Js) {
    final a = js.ra();
    return new InvestorEntry(
      a[0].rf(),
      a[1].rf(),
      a[2].rf(),
      a[3].rf(),
      a[4].rf(),
      a[5].rf()
    );
  }

  public static function sum (entries: Array<InvestorEntry>): InvestorEntry {
    var currentProfits = 0.0;
    var riskProfits = 0.0;
    var active = 0.0;
    var stocksCurrentProfits = 0.0;
    var stocksRiskProfits = 0.0;
    var stocksAccountValue = 0.0;
    for (e in entries) {
      currentProfits += e.currentProfits;
      riskProfits += e.riskProfits;
      active += e.active;
      stocksCurrentProfits += e.stocksCurrentProfits;
      stocksRiskProfits += e.stocksRiskProfits;
      stocksAccountValue += e.stocksAccountValue;
    }
    return new InvestorEntry(
      currentProfits, riskProfits, active,
      stocksCurrentProfits, stocksRiskProfits, stocksAccountValue
    );
  }
}

class CompanyEntry {
  public final nick: String;
  public final stocks: Int;
  public final currentPrice: Float;
  public final accountPrice: Float;
  public final stopPrice: Float;
  public var stocksCurrentProfits(default, null): Float;
  public var stocksRiskProfits(default, null): Float;
  public var stocksAccountValue(default, null): Float;

  function new (
    nick: String, stocks: Int,
    accountPrice: Float, currentPrice: Float, stopPrice: Float
  ) {
    this.nick = nick;
    this.stocks = stocks;
    this.currentPrice = currentPrice;
    this.accountPrice = accountPrice;
    this.stopPrice = stopPrice;
    stocksAccountValue = stocks * accountPrice;
    stocksCurrentProfits = stocks * currentPrice - stocksAccountValue;
    stocksRiskProfits = stocks * stopPrice - stocksAccountValue;
  }

  public static function fromJs (js: Js) {
    final a = js.ra();
    return new CompanyEntry(
      a[0].rs(),
      a[1].ri(),
      a[2].rf(),
      a[3].rf(),
      a[4].rf()
    );
  }

  /// Only fields 'stocksCurrentProfits', 'stocksRiskProfits' and
  /// 'stocksAccountValue' have valid values.
  public static function sum (entries: Array<CompanyEntry>): CompanyEntry {
    var stocksCurrentProfits = 0.0;
    var stocksRiskProfits = 0.0;
    var stocksAccountValue = 0.0;
    for (e in entries) {
      stocksCurrentProfits += e.stocksCurrentProfits;
      stocksRiskProfits += e.stocksRiskProfits;
      stocksAccountValue += e.stocksAccountValue;
    }

    final r = new CompanyEntry("", 0, 0.0, 0.0, 0.0);
    r.stocksCurrentProfits = stocksCurrentProfits;
    r.stocksRiskProfits = stocksRiskProfits;
    r.stocksAccountValue = stocksAccountValue;
    return r;
  }

  /// Sorts 'entries' in place.
  ///   entries: Entries to sort.
  ///   field  : Main field to sort. Can be "nick", "losses" or "risk".
  ///   isAscendant: 'true' if order is ascendant.
  public static function sort (
    entries: Array<CompanyEntry>, field: String, isAscendant: Bool
  ): Void {
    final acAvg = Speedometers.acumulatorAvg;

    if (field == "nick") {
      if (isAscendant) {
        entries.sort((e1, e2) ->
          e1.nick < e2.nick ? -1: e1.nick > e2.nick ? 1: 0
        );
      } else {
        entries.sort((e1, e2) ->
          e1.nick > e2.nick ? -1: e1.nick < e2.nick ? 1: 0
        );
      }
    } else if (field == "losses") {
      if (isAscendant) {
        entries.sort((e1, e2) -> {
          final v1 =
            acAvg(e1.stocksCurrentProfits, e1.stocksAccountValue) -
            acAvg(e2.stocksCurrentProfits, e2.stocksAccountValue);
          final v2 =
            acAvg(e1.stocksRiskProfits, e1.stocksAccountValue) -
            acAvg(e2.stocksRiskProfits, e2.stocksAccountValue);
          final r = v1 == 0.0 ? v2  : v1;
          return r > 0 ? 1 : -1;
        });
      } else {
        entries.sort((e1, e2) -> {
          final v1 =
            acAvg(e2.stocksCurrentProfits, e2.stocksAccountValue) -
            acAvg(e1.stocksCurrentProfits, e1.stocksAccountValue);
          final v2 =
            acAvg(e2.stocksRiskProfits, e2.stocksAccountValue) -
            acAvg(e1.stocksRiskProfits, e1.stocksAccountValue);
          final r = v1 == 0.0 ? v2  : v1;
          return r > 0 ? 1 : -1;
        });
      }
    } else {
      if (isAscendant) {
        entries.sort((e1, e2) -> {
          final v1 =
            acAvg(e1.stocksRiskProfits, e1.stocksAccountValue) -
            acAvg(e2.stocksRiskProfits, e2.stocksAccountValue);
          final v2 =
            acAvg(e1.stocksCurrentProfits, e1.stocksAccountValue) -
            acAvg(e2.stocksCurrentProfits, e2.stocksAccountValue);
          final r = v1 == 0.0 ? v2  : v1;
          return r > 0 ? 1 : -1;
        });
      } else {
        entries.sort((e1, e2) -> {
          final v1 =
            acAvg(e2.stocksRiskProfits, e2.stocksAccountValue) -
            acAvg(e1.stocksRiskProfits, e1.stocksAccountValue);
          final v2 =
            acAvg(e2.stocksCurrentProfits, e2.stocksAccountValue) -
            acAvg(e1.stocksCurrentProfits, e1.stocksAccountValue);
          final r = v1 == 0.0 ? v2  : v1;
          return r > 0 ? 1 : -1;
        });
      }
    }
  }
}
