// Copyright 19-Sep-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pgs.acc;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Js;
import dm.Dec;
import dm.Opt;
import data.Cts;
import data.BrokerA;
import I18n._;

/// Accounting profits.
class Trading {
  var wg: Domo;
  var bet: Float;
  var operations: Array<Operation>;

  var bentry: Domo;
  var pentry: Domo;
  var result: Domo;

  function new (wg: Domo, bet: Float, operations: Array<Operation>) {
    this.wg = wg;
    this.bet = bet;
    this.operations = operations;

    bentry = Ui.field("pentry").value(Dec.toIso(bet, 0));
    Ui.changePoint(bentry);
    pentry = Ui.field("calcBt").att("id", "pentry");
    Ui.changePoint(pentry);
    result = Q("div").klass("frame").style("text-align:right");

    view();
  }

  // View ----------------------------------------------------------------------

  function view () {
    final os = operations;
    final buys = os.filter(o -> o.toBuy);
    final buyTrs = buys.length == 0
      ? [
        Q("tr")
          .add(Q("td")
            .att("colspan", 2)
            .klass("borderWhite")
            .html(_("Without operations")))
      ]
      : buys.map(o ->
        Q("tr")
          .add(invTd(o.managers))
          .add(ciaTd(o.nick))
      )
    ;
    final sells = os.filter(o -> o.toSell > 0);
    final sellTrs = sells.length == 0
      ? [
        Q("tr")
          .add(Q("td")
            .att("colspan", 3)
            .klass("borderWhite")
            .html(_("Without operations")))
      ]
      : sells.map(o ->
        Q("tr")
          .add(invTd(o.managers))
          .add(ciaTd(o.nick))
          .add(stocksTd(o.toSell))
      )
    ;

    wg
      .removeAll()
      .add(Q("table")
        .klass("main")
        .add(Q("tr")
          .add(Q("td")
            .style("vertical-align:top;width:5px")
            .add(Q("table")
              .klass("frame4")
              .add(Q("tr")
                .add(Q("td")
                  .add(Q("div")
                    .klass("head")
                    .text(_("Buy")))))
              .add(Q("tr")
                .add(Q("td")
                  .add(Q("hr"))))
              .add(Q("tr")
                .add(Q("td")
                  .style("text-align:center")
                  .add(Q("div")
                    .html("<b>" + _("Invest") + "</b>"))))
              .add(Q("tr")
                .add(Q("td")
                  .add(bentry)))
              .add(Q("tr")
                .add(Q("td")
                  .style("text-align:center")
                  .add(Q("div")
                    .html("<b>" + _("Price") + "</b>"))))
              .add(Q("tr")
                .add(Q("td")
                  .add(pentry)))
              .add(Q("tr")
                .add(Q("td")
                  .style("text-align:center")
                  .add(Q("button")
                    .text(_("Calculate"))
                    .on(CLICK, e -> calculate()))))
              .add(Q("tr")
                .add(Q("td")
                  .add(Q("hr"))))
              .add(Q("tr")
                .add(Q("td")
                  .style("text-align:center")
                  .add(Q("div")
                    .html("<b>" + _("Stocks") + "</b>"))))
              .add(Q("tr")
                .add(Q("td")
                  .add(result)))))
          .add(Q("td")
            .style("text-align:center")
            .add(Q("div")
              .klass("head")
              .html(_("Buys")))
            .add(Q("table")
              .att("align", "center")
              .klass("buys")
              .add(Q("tr")
                .add(Q("td").klass("head").html(_("Inv")))
                .add(Q("td").klass("head").html(_("Co."))))
              .adds(buyTrs))
            .add(Q("div")
              .klass("head")
              .html(_("Sells")))
            .add(Q("table")
              .att("align", "center")
              .klass("sells")
              .add(Q("tr")
                .add(Q("td").klass("head").html(_("Inv")))
                .add(Q("td").klass("head").html(_("Co.")))
                .add(Q("td").klass("head").html(_("Stocks"))))
              .adds(sellTrs)))))
    ;
  }

  // Control -------------------------------------------------------------------

  function calculate () {
    final bs = cast(bentry.getValue(), String);
    final ps = cast(pentry.getValue(), String);
    final b = Opt.get(Dec.fromIso(bs));
    if (b == null) {
      Ui.alert('${bs} is not a valid number.');
      return;
    }
    final p = Opt.get(Dec.fromIso(ps));
    if (p == null) {
      Ui.alert('${ps} is not a valid number.');
      return;
    }
    if (p == 0) {
      Ui.alert("Price is 0");
      return;
    }
    var rs = Std.int(b / p);
    while (rs > 0 && BrokerA.buy(rs, p) > b) --rs;

    result.text(Dec.toIso(rs, 0));
  }

  // Static --------------------------------------------------------------------

  /// Constructor:
  ///   wg: Container.
  public static function mk (wg: Domo) {
    wg
      .removeAll()
      .add(Q("table")
        .klass("main")
        .add(Q("tr")
          .add(Q("td")
            .style("text-align:center")
            .add(Ui.Q("img")
              .att("src", "img/wait2.gif")
              .klass("frame")))))
    ;
    Cts.client.send([
      "module" => Js.ws("acc"),
      "source" => Js.ws("trading"),
      "rq" => Js.ws("idata")
    ], rp -> {
      final bet = rp["bet"].rf();
      final operations = rp["operations"].ra().map(e -> Operation.fromJs(e));

      new Trading(wg, bet, operations);
    });
  }

  function invTd (invs) {
    return Q("td")
      .klass("borderWhite")
      .text(invs.map(i -> Std.string(i)).join(" - "))
    ;
  }

  function ciaTd (nick) {
    return Q("td")
      .klass("borderWhite")
      .style("text-align:left")
      .text(nick)
    ;
  }

  function stocksTd (stock) {
    return Q("td")
      .klass("borderWhite")
      .style("text-align:right")
      .text(Dec.toIso(stock, 0))
    ;
  }

}

// Trading operation
// NOTE: If an investor should buy toBuy is true and if it should sell
//       toSell has a number > 0. An operation can have sells and buys
//       at the same time.
private class Operation {
  public var toBuy(default, null): Bool;
  public var toSell(default, null): Int;
  public var managers(default, null): Array<Int>;
  public var nick(default, null): String;

  function new () {}

  public static function fromJs(js: Js): Operation {
    final a = js.ra();
    final r = new Operation();
    r.toBuy = a[0].rb();
    r.toSell = a[1].ri();
    r.managers = a[2].ra().map(e -> e.ri());
    r.nick = a[3].rs();
    return r;
  }
}
