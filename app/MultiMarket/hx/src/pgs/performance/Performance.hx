// Copyright 17-Jan-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pgs.performance;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Js;
import dm.Dec;
import dm.Dt;
import dm.It;
import dm.Opt;
import data.Cts;
import I18n._;

/// Execution orders performance.
class Performance {
  var wg: Domo;
  var results: Array<Result>;
  var diff: Float;

  // Operations are unsorted.
  function new (wg: Domo, operations: Array<Operation>, diff: Float) {
    this.wg = wg;
    this.diff = diff;

    operations.sort((o1, o2) ->
      return o1.date > o2.date
        ? 1
        : o1.date < o2.date
          ? -1
          : o1.isSell ? 1 : -1
    );

    var nBuys = 0;
    var amountBuys = 0.0;
    var profitsBuys = 0.0;
    var nSells = 0;
    var amountSells = 0.0;
    var profitsSells = 0.0;
    results = operations.map(o -> {
      final amount = o.stocks * o.price;
      var profits: Float;
      if (o.isSell) {
        ++nSells;
        amountSells += amount;
        profits = o.stocks * (o.price - o.open);
        profitsSells += profits;
      } else {
        ++nBuys;
        amountBuys += amount;
        profits = o.stocks * (o.open - o.price);
        profitsBuys += profits;
      }

      return new Result(
        o.nick, o.date, o.isSell, amount, profits,
        nBuys, amountBuys, profitsBuys,
        nSells, amountSells, profitsSells
      );
    });

    view();
  }

  // View ----------------------------------------------------------------------

  function view () {
    final rs = results.copy();
    rs.reverse();

    final trs = rs.length == 0
      ? [
          Q("tr")
            .add(Q("td")
              .klass("borderWhite")
              .html(_("Without operations")))
        ]
      : rs.map(e -> {
          Q("tr")
            .add(ciaTd()
              .text(Dt.toIso(Opt.eget(Dt.from(e.date)))))
            .add(ciaTd()
              .text(e.nick))
            .add(ciaTd()
              .add(Ui.led(e.isSell ? "#0060a0" : "#a04000")))
            .add(ciaTd())
            .add(nmTd("NORMAL", Some(e.amount)))
            .add(nmTd("NORMAL", Some(e.profits)))
            .add(nmTd("NORMAL", Some(e.percentage), true))
            .add(ciaTd())
            .add(nmTd("BUY", e.isSell ? None : Some(e.totalBuys)))
            .add(nmTd("BUY", e.isSell ? None : Some(e.avgBuys)))
            .add(nmTd("BUY", e.isSell ? None : Some(e.percBuys), true))
            .add(ciaTd())
            .add(nmTd("SELL", e.isSell ? Some(e.totalSells) : None))
            .add(nmTd("SELL", e.isSell ? Some(e.avgSells) : None))
            .add(nmTd("SELL", e.isSell ? Some(e.percSells) : None, true))
            .add(ciaTd())
            .add(nmTd("NORMAL", Some(e.totalTotal)))
            .add(nmTd("NORMAL", Some(e.avgTotal)))
            .add(nmTd("NORMAL", Some(e.percTotal), true))
        ;})
    ;

    wg
      .removeAll()
      .add(Q("table")
        .att("align", "center")
        .add(Q("tr")
          .add(Q("td")
            .att("colspan", "4")
            .att("rowspan", "2")
            .style("text-align:center")
            .add(Q("span")
              .klass("frame")
              .text(_("Diff/op") + " = " + Dec.toIso(diff, 2))))
          .add(Q("td")
            .klass("header")
            .att("colspan", "3")
            .att("rowspan", "2")
            .text(_("Operations")))
          .add(Q("td")
            .klass("header")
            .att("rowspan", "2")
            .text(""))
          .add(Q("td")
            .klass("header")
            .att("colspan", "11")
            .text(_("Profits"))))
        .add(Q("tr")
          .add(Q("td")
            .klass("header")
            .att("colspan", "3")
            .text(_("Buys")))
          .add(Q("td")
            .klass("header")
            .text(""))
          .add(Q("td")
            .klass("header")
            .att("colspan", "3")
            .text(_("Sales")))
          .add(Q("td")
            .klass("header")
            .text(""))
          .add(Q("td")
            .klass("header")
            .att("colspan", "3")
            .text(_("Total"))))
        .add(Q("tr")
          .add(Q("td")
            .klass("header")
            .text(_("Date")))
          .add(Q("td")
            .klass("header")
            .text(_("Nick")))
          .add(Q("td")
            .klass("header")
            .text(_("B/S")))
          .add(Q("td")
            .klass("header"))
          .add(Q("td")
            .klass("header")
            .text(_("Amount")))
          .add(Q("td")
            .klass("header")
            .text(_("Prof.")))
          .add(Q("td")
            .klass("header")
            .text("%"))
          .add(Q("td")
            .klass("header"))
          .add(Q("td")
            .klass("header")
            .text(_("Total")))
          .add(Q("td")
            .klass("header")
            .text(_("Avg.")))
          .add(Q("td")
            .klass("header")
            .text(_("%")))
          .add(Q("td")
            .klass("header"))
          .add(Q("td")
            .klass("header")
            .text(_("Total")))
          .add(Q("td")
            .klass("header")
            .text(_("Avg.")))
          .add(Q("td")
            .klass("header")
            .text(_("%")))
          .add(Q("td")
            .klass("header"))
          .add(Q("td")
            .klass("header")
            .text(_("Total")))
          .add(Q("td")
            .klass("header")
            .text(_("Avg.")))
          .add(Q("td")
            .klass("header")
            .text(_("%"))))


        .adds(trs))
    ;
  }

  // Static --------------------------------------------------------------------

  /// Constructor:
  ///   wg: Container.
  public static function mk (wg: Domo) {
    Cts.client.send([
      "module" => Js.ws("performance"),
      "rq" => Js.ws("idata")
    ], rp -> {
      final operations = rp["operations"].ra().map(e -> Operation.fromJs(e));
      final diff = rp["diff"].rf() / operations.length;

      new Performance(wg, operations, diff);
    });
  }

  function ciaTd () {
    return Q("td")
      .klass("borderWhite")
      .style("text-align:left")
    ;
  }

  function nmTd (type: String, value: Option<Float>, isPerc = false) {
    final back = switch (type) {
      case "SELL": "background-color: rgb(200, 230, 250);";
      case "BUY": "background-color: rgb(250, 210, 200);";
      default: "background-color: rgb(250, 250, 250);";
    }
    final width = isPerc ? "width: 50px;" : "width: 80px;";
    return Q("td")
      .style("text-align:right; border: 1px solid rgb(110,130,150);" +
             back + width)
      .text(switch(value) {
          case Some(v): Dec.toIso(v, 2);
          case None: "";
        })
    ;
  }

}

// Trading operation
// NOTE: If an investor should buy toBuy is true and if it should sell
//       toSell has a number > 0. An operation can have sells and buys
//       at the same time.
private class Operation {
  public var nick(default, null): String;
  public var date(default, null): String;
  public var isSell(default, null): Bool;
  public var stocks(default, null): Int;
  public var price(default, null): Float;
  public var open(default, null): Float;

  function new () {}

  public static function fromJs(js: Js): Operation {
    final a = js.ra();
    final r = new Operation();
    r.nick = a[0].rs();
    r.date = a[1].rs();
    r.isSell = a[2].rb();
    r.stocks = a[3].ri();
    r.price = a[4].rf();
    r.open = a[5].rf();
    return r;
  }
}

private class Result {
  public var nick(default, null): String;
  public var date(default, null): String;
  public var isSell(default, null): Bool;
  public var amount(default, null): Float; // Expected total operation.
  public var profits(default, null): Float;
  public var percentage(default, null): Float; //%

  public var totalBuys(default, null): Float;
  public var avgBuys(default, null): Float;
  public var percBuys(default, null): Float; //%

  public var totalSells(default, null): Float;
  public var avgSells(default, null): Float;
  public var percSells(default, null): Float; //%

  public var totalTotal(default, null): Float;
  public var avgTotal(default, null): Float;
  public var percTotal(default, null): Float;  //%

  public function new (
    nick: String, date: String, isSell: Bool,
    amount: Float, profits: Float,
    nBuys: Int, amountBuys: Float, profitsBuys: Float,
    nSells: Int, amountSells: Float, profitsSells: Float
  ) {
    this.nick = nick;
    this.date = date;
    this.isSell = isSell;
    this.amount = amount;
    this.profits = profits;

    final nTotal = nBuys + nSells;
    final amountTotal = amountBuys + amountSells;

    percentage = amount > 0 ? profits * 100 / amount : 0;

    totalBuys = profitsBuys;
    avgBuys = nBuys > 0 ? profitsBuys / nBuys : 0;
    percBuys = amountBuys > 0 ? profitsBuys * 100 / amountBuys : 0;

    totalSells = profitsSells;
    avgSells = nSells > 0 ? profitsSells / nSells: 0;
    percSells = amountSells > 0 ? profitsSells * 100 / amountSells : 0;

    totalTotal = profitsBuys + profitsSells;
    avgTotal = nTotal > 0 ? totalTotal / nTotal : 0;
    percTotal = amountTotal > 0 ? totalTotal * 100 / amountTotal : 0;
  }
}
