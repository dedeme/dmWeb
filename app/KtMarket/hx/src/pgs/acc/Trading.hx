// Copyright 14-Nov-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pgs.acc;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Js;
import dm.Dec;
import dm.Opt;
import data.Broker;
import data.InvOperation;
import data.Nick; // NickNameStr
import I18n._;

/// Accounting profits.
class Trading {
  final wg: Domo;
  final bet: Float;
  final rebuys: Array<NickNameStr>;
  final operations: Array<InvOperation>;

  var bentry: Domo;
  var pentry: Domo;
  var result: Domo;
  var spreadWg: Domo;
  var blankTd = Q("td");
  var stPoss: Array<Domo> = [];
  var prPoss: Array<Domo> = [];
  var posTrs: Array<Domo> = [];

  function new (
    wg: Domo, bet: Float, rebuys: Array<NickNameStr>,
    operations: Array<InvOperation>
  ) {
    this.wg = wg;
    this.bet = bet;
    this.rebuys = rebuys;
    this.operations = operations;

    bentry = Ui.field("pentry")
      .style("width:80px")
      .value(Dec.toIso(bet, 0));
    Ui.changePoint(bentry);
    pentry = Ui.field("calcBt")
      .style("width:80px")
      .att("id", "pentry");
    Ui.changePoint(pentry);
    result = Q("div").klass("frame").style("text-align:right");
    spreadWg = Q("div").klass("frame").style("text-align:right");

    prPoss.push(Q("div")
      .klass("frame")
      .style("text-align:right")
    );
    posTrs.push(Q("tr")
      .add(blankTd)
      .add(Q("td")
        .att("rowspan", "3")
        .add(prPoss[0]))
    );
    for (i in 0...11) {
      var bold = i == 5 ? ";font-weight:bold" : "";
      var stPos = Q("div").style("text-align:right" + bold);
      stPoss.push(stPos);

      if (i == 4) bold = ";font-weight:bold";
      var prPos = Q("div").klass("frame").style("text-align:right" + bold);
      prPoss.push(prPos);

      posTrs.push(Q("tr").add(Q("td")));
      posTrs.push(Q("tr")
        .add(Q("td")
          .att("rowspan", "2")
          .add(stPos))
      );
      posTrs.push(Q("tr")
        .add(Q("td")
          .att("rowspan", "3")
          .add(prPos))
      );
    }
    posTrs.push(Q("tr").add(Q("td")));
    posTrs.push(Q("tr").add(Q("td")));

    view();
  }

  // View ----------------------------------------------------------------------

  function view () {
    final rebuyTrs = rebuys.length == 0
      ? [
        Q("tr")
          .add(Q("td")
            .att("colspan", 2)
            .klass("borderWhite")
            .html(_("Without operations")))
      ]
      : rebuys.map(r ->
        Q("tr")
          .add(ciaTd(r.nick))
          .add(ciaTd(r.value))
      )
    ;
    final os = operations;
    final buys = os.filter(o -> o.stocks <= 0);
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
          .add(invTd(o.manager))
          .add(ciaTd(o.nick).style(
              o.stocks == 0 ? "" : "text-decoration:line-through"
            ))
      )
    ;

    final sells = os.filter(o -> o.stocks > 0);
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
          .add(invTd(o.manager))
          .add(ciaTd(o.nick))
          .add(stocksTd(o.stocks))
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
                  .att("colspan", "2")
                  .add(Q("div")
                    .klass("head")
                    .text(_("Buy")))))
              .add(Q("tr")
                .add(Q("td")
                  .att("colspan", "2")
                  .add(Q("hr"))))
              .add(Q("tr")
                .add(Q("td")
                  .style("text-align:right")
                  .add(Q("div")
                    .html("<b>" + _("Invest") + ":</b>")))
                .add(Q("td")
                  .add(bentry)))
              .add(Q("tr")
                .add(Q("td")
                  .style("text-align:right")
                  .add(Q("div")
                    .html("<b>" + _("Price") + ":</b>")))
                .add(Q("td")
                  .add(pentry)))
              .add(Q("tr")
                .add(Q("td")
                  .att("colspan", "2")
                  .style("text-align:center")
                  .add(Q("button")
                    .text(_("Calculate"))
                    .on(CLICK, e -> calculate()))))
              .add(Q("tr")
                .add(Q("td")
                  .att("colspan", "2")
                  .add(Q("hr"))))
              .add(Q("tr")
                .add(Q("td")
                  .style("text-align:right")
                  .add(Q("div")
                    .html("<b>" + _("Stocks") + ":</b>")))
                .add(Q("td")
                  .add(result)))
              .add(Q("tr")
                .add(Q("td")
                  .style("text-align:right")
                  .add(Q("div")
                    .html("<b>" + _("Spread") + ":</b>")))
                .add(Q("td")
                  .add(spreadWg)))
              .add(Q("tr")
                .add(Q("td")
                  .att("colspan", "2")
                  .add(Q("hr"))))
              .add(Q("tr")
                .add(Q("td")
                  .style("text-align:right")
                  .add(Q("div")
                    .html("<b>" + _("Stocks") + "</b>")))
                .add(Q("td")
                  .style("text-align:right")
                  .add(Q("div")
                    .html("<b>" + _("Price") + "</b>"))))
              .adds(posTrs)))
          .add(Q("td")
            .style("text-align:center")
            .add(Q("div")
              .klass("head")
              .html(_("Rebuys")))
            .add(Q("table")
              .att("align", "center")
              .add(Q("tr")
                .add(Q("td").klass("head").html(_("Co.")))
                .add(Q("td").klass("head").html(_("Date"))))
              .adds(rebuyTrs))
            .add(Q("hr"))
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
    final b2 = b + b - Broker.buy(1, b);

    final p = Opt.get(Dec.fromIso(ps));
    if (p == null) {
      Ui.alert('${ps} is not a valid number.');
      return;
    }
    if (p == 0) {
      Ui.alert("Price is 0");
      return;
    }

    var rs = Std.int(b2 / p);

    result.text(Dec.toIso(rs, 0));
    spreadWg.text(Dec.toIso(spreads(p), 4));

    var b2 = b + b - Broker.buy(1, b);
    for (i in 0...12) {
      var st = rs + i - 5;
      var pr = Std.int(b2 * 10000 / st) / 10000;
      if (i < 11) stPoss[i].text(Dec.toIso(st, 0));
      prPoss[i].text(Dec.toIso(pr, 4));
    }

    blankTd.html("&nbsp;");
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
      final rebuys = rp["rebuys"].ra().map(e -> NickNameStr.fromJs(e));
      final operations = rp["operations"].ra().map(e -> InvOperation.fromJs(e));

      new Trading(wg, bet, rebuys, operations);
    });
  }

  function invTd (inv: Int): Domo {
    return Q("td")
      .klass("borderWhite")
      .text("" + inv)
    ;
  }

  function ciaTd (nick:String): Domo {
    return Q("td")
      .klass("borderWhite")
      .style("text-align:left")
      .text(nick)
    ;
  }

  function stocksTd (stocks: Int): Domo {
    return Q("td")
      .klass("borderWhite")
      .style("text-align:right")
      .text(Dec.toIso(stocks, 0))
    ;
  }

  function spreads (price: Float): Float {
    return price < 1 ? 0.0001
      : price < 2 ? 0.0002
      : price < 5 ? 0.0005
      : price < 10 ? 0.001
      : price < 20 ? 0.002
      : price < 50 ? 0.005
      : price < 100 ? 0.01
      : price < 200 ? 0.02
      : price < 500 ? 0.05
      : 0.1
    ;
  }

}

