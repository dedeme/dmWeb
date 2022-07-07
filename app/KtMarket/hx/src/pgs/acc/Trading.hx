// Copyright 14-Nov-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pgs.acc;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Js;
import dm.Dec;
import dm.Opt;
import dm.Dt;
import dm.It;
import wgs.Msg;
import data.Broker;
import data.InvOperation;
import data.Nick; // NickNameStr
import data.acc.PfEntry;
import I18n._;

/// Accounting profits.
class Trading {
  final wg: Domo;
  final bet: Float;
  final rebuys: Array<NickNameStr>;
  final operations: Array<InvOperation>;
  final portfolios: Array<Array<PfEntry>>;

  var bentry: Domo;
  var pentry: Domo;
  var price2: Domo;
  var result: Domo;
  var result2: Domo;
  var sentry: Domo;
  var sprice2: Domo;
  var blankTd = Q("td");

  function new (
    wg: Domo, bet: Float, rebuys: Array<NickNameStr>,
    operations: Array<InvOperation>, portfolios: Array<Array<PfEntry>>
  ) {
    this.wg = wg;
    this.bet = bet;
    this.rebuys = rebuys;
    this.operations = operations;
    this.portfolios = portfolios;

    bentry = Ui.field("pentry")
      .style("width:80px")
      .value(Dec.toIso(bet, 0));
    Ui.changePoint(bentry);
    pentry = Ui.field("calcBuyBt")
      .style("width:80px")
      .att("id", "pentry");
    Ui.changePoint(pentry);
    price2 = Q("div").klass("frame").style("text-align:right");
    result = Q("div").klass("frame").style("text-align:right");
    result2 = Q("div").klass("frame").style("text-align:right");

    sentry = Ui.field("calcSellBt")
      .style("width:80px")
      .att("id", "sentry");
    Ui.changePoint(sentry);
    sprice2 = Q("div").klass("frame").style("text-align:right");

    view();
  }

  // View ----------------------------------------------------------------------

  function buyWg () {
    return Q("table")
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
            .att("id", "calcBuyBt")
            .text(_("Calculate"))
            .on(CLICK, e -> calculateBuy()))))
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
          .att("colspan", "2")
          .add(Q("hr"))))
      .add(Q("tr")
        .add(Q("td")
          .style("text-align:right;white-space:nowrap")
          .add(Q("div")
            .html("<b>" + _("Price") + " + :</b>")))
        .add(Q("td")
          .add(price2)))
      .add(Q("tr")
        .add(Q("td")
          .style("text-align:right;white-space:nowrap")
          .add(Q("div")
            .html("<b>" + _("Stocks") + " + :</b>")))
        .add(Q("td")
          .add(result2)))
    ;
  }

  function sellWg () {
    return Q("table")
      .klass("frame3")
      .add(Q("tr")
        .add(Q("td")
          .att("colspan", "2")
          .add(Q("div")
            .klass("head")
            .text(_("Sell")))))
      .add(Q("tr")
        .add(Q("td")
          .att("colspan", "2")
          .add(Q("hr"))))
      .add(Q("tr")
        .add(Q("td")
          .style("text-align:right")
          .add(Q("div")
            .html("<b>" + _("Price") + ":</b>")))
        .add(Q("td")
          .add(sentry)))
      .add(Q("tr")
        .add(Q("td")
          .att("colspan", "2")
          .style("text-align:center")
          .add(Q("button")
            .att("id", "calcSellBt")
            .text(_("Calculate"))
            .on(CLICK, e -> calculateSell()))))
      .add(Q("tr")
        .add(Q("td")
          .att("colspan", "2")
          .add(Q("hr"))))
      .add(Q("tr")
        .add(Q("td")
          .style("text-align:right;white-space:nowrap")
          .add(Q("div")
            .html("<b>" + _("Price") + " + :</b>")))
        .add(Q("td")
          .add(sprice2)))
    ;
  }

  function view () {
    rebuys.sort((e1, e2) ->
      e1.value < e2.value
        ? 1
        : e1.value > e2.value
          ? -1
          : e1.nick > e2.nick ? 1 : e1.nick < e2.nick ? -1 : 0
    );
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
          .add(ciaTd(
            I18n.lang == "es"
              ? Dt.toIso(Opt.get(Dt.from(r.value)))
              : Dt.toEn(Opt.get(Dt.from(r.value)))
          ))
      )
    ;
    final os = operations;
    os.sort((e1, e2) ->
      e1.investor > e2.investor
        ? 1
        : e1.investor < e2.investor
          ? -1
          : e1.nick > e2.nick ? 1 : e1.nick < e2.nick ? -1 : 0
    );
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
          .add(invTd(o.investor))
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
      : sells.map(o -> {
          var gol = 0.0;
          var q = 0.0;
          var dif = 0.0;
          var ref = 0.0;
          switch (It.from(portfolios[o.investor]).find(e -> e.nick == o.nick)) {
            case Some(e): {
              gol = e.price * Cts.noLostMultiplicator;
              q = e.quote;
              dif = (gol - q) * 100 / q;
              ref = e.ref;
            };
            default:{}
          }
          final color = dif <= 5 ? "#d0d9f0" : dif <= 15 ? "#f0f0d9" : "#ffffff";
          return Q("tr")
            .add(invTd(o.investor).setStyle("background", color))
            .add(ciaTd(o.nick).setStyle("background", color))
            .add(stocksTd(o.stocks).setStyle("background", color))
            .add(quoteTd(gol).setStyle("background", color))
            .add(quoteTd(q).setStyle("background", color))
            .add(quoteTd(dif).setStyle("background", color))
          ;
        }
      )
    ;

    wg
      .removeAll()
      .add(Q("table")
        .klass("main")
        .add(Q("tr")
          .add(Q("td")
            .style("vertical-align:top;width:5px")
            .add(buyWg()))
          .add(Q("td")
            .style("text-align:center")
              .add(Q("table")
                .klass("frame")
                .att("align", "center")
                .add(Q("tr")
                  .add(Q("td")
                    .add(Q("div")
                      .klass("head")
                      .html(_("Rebuys")))
                    .add(Q("table")
                      .klass("frame2")
                      .add(Q("tr")
                        .add(Q("td").klass("head").html(_("Co.")))
                        .add(Q("td").klass("head").html(_("Date"))))
                      .adds(rebuyTrs)))))
            .add(Q("div")
              .klass("head")
              .html("&nbsp;"))
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
                .add(Q("td").klass("head").html(_("Stocks")))
                .add(Q("td").klass("head").html(_("Gol")))
                .add(Q("td").klass("head").html(_("Quote")))
                .add(Q("td").klass("head").html(_("Dif.")))
                )
              .adds(sellTrs)))
          .add(Q("td")
            .style("vertical-align:top;width:5px")
            .add(sellWg()))))
    ;
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

  function quoteTd (q: Float): Domo {
    return Q("td")
      .klass("borderWhite")
      .style("text-align:right")
      .text(Dec.toIso(q, 4))
    ;
  }

  // Control -------------------------------------------------------------------

  function calculateBuy () {
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

    final rs = Std.int(b2 / p);
    final p2 = p * 0.99;
    final rs2 = Std.int(b2 / p2);

    price2.text(Dec.toIso(p2, 2));
    result.text(Dec.toIso(rs, 0));
    result2.text(Dec.toIso(rs2, 0));


    blankTd.html("&nbsp;");
  }

  function calculateSell () {
    final ps = cast(sentry.getValue(), String);
    final p = Opt.get(Dec.fromIso(ps));
    if (p == null) {
      Ui.alert('${ps} is not a valid number.');
      return;
    }
    sprice2.text(Dec.toIso(p * 1.01, 2));
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
      final portfolios = rp["portfolios"].ra().map(pf ->
        pf.ra().map(PfEntry.fromJs)
      );

      new Trading(wg, bet, rebuys, operations, portfolios);
    });
  }

}

