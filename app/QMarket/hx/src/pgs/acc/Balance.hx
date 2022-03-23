// Copyright 12-Nov-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pgs.acc;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Js;
import dm.It;
import dm.Dec;
import dm.Menu;
import data.Cts;
import data.acc.Ledger;
import data.acc.PfEntry;
import I18n._;

/// Accounting balance.
class Balance {
  static final NICK_ORDER = 0;
  static final VALUE_ORDER = 1;
  static final PROFITS_ORDER = 2;
  static final RISK_ORDER = 3;
  static final BET_ORDER = 4;
  static final REF_ORDER = 5;

  var wg: Domo;
  var ledgers: Array<Ledger>; // One for each manager.
  var portfolios: Array<Array<PfEntry>>; // One portfolio for each manager.
  var mSel: Int;
  var order: Int;

  function new (
    wg: Domo, ledgers: Array<Ledger>, portfolios: Array<Array<PfEntry>>
  ) {
    this.wg = wg;
    this.ledgers = ledgers;
    this.portfolios = portfolios;

    mSel = -1;
    order = REF_ORDER;

    view();
  }

  // View ----------------------------------------------------------------------

  /**
      @private
      @return function(number, number):number
  **/
  function sortf (): (PfEntry, PfEntry) -> Int {
    return order == NICK_ORDER
      ? (row1, row2) -> row1.nick > row2.nick ? 1 : -1
      : order == VALUE_ORDER
        ? (row1, row2) ->
          row1.quote * row1.stocks > row2.quote * row2.stocks ? -1 : 1
        : order == PROFITS_ORDER
          ? (row1, row2) ->
            (row1.quote - row1.price) * row1.stocks >
            (row2.quote - row2.price) * row2.stocks
              ? -1 : 1
          : order == RISK_ORDER
            ? (row1, row2) ->
              (row1.ref - row1.price) * row1.stocks >
              (row2.ref - row2.price) * row2.stocks
                ? -1 : 1
            : order == BET_ORDER
              ? (row1, row2) ->
                (row1.quote - row1.ref) * row1.stocks >
                (row2.quote - row2.ref) * row2.stocks
                  ? -1 : 1
              : (row1, row2) ->
                fref(row1.quote, row1.ref) <
                fref(row2.quote, row2.ref)
                  ? -1 : 1
    ;
  }

  function body (wg: Domo, ld: Ledger, pf: Array<PfEntry>) {
    pf.sort(this.sortf());
    final stocks = ld.stocks;
    final cash = ld.cash;
    final capital = -ld.capital;
    final sells = -ld.sells;
    final fees = -ld.fees;
    final incomes = -ld.profits;
    final differences = -ld.differences;

    final accountProfits = sells + fees + incomes + differences;
    var currentProfits = accountProfits;
    var riskProfits = accountProfits;
    var bet = 0.0;

    final portfolio = pf.map(e -> {
      if (e.quote <= 0) {
        currentProfits = null;
        return new PfEntry(e.nick, e.stocks, e.price, null, e.ref);
      }
      if (currentProfits != null) {
        currentProfits += (e.quote - e.price) * e.stocks;
        riskProfits += (e.ref - e.price) * e.stocks;
        bet += (e.quote - e.ref) * e.stocks;
      }
      return e;
    });

    var withdraw = 0.0;
    if (currentProfits != null) {
      final assets = currentProfits + capital;
      if (assets > Cts.initialCapital + Cts.bet + Cts.bet) {
        final dif = assets - Cts.initialCapital - Cts.bet;
        if (cash > dif + 1000) {
          withdraw = dif;
        } else if (cash > Cts.bet + 1000) {
          withdraw = Std.int((cash - 1000) / Cts.bet) * Cts.bet;
        }
      }
    }

    var sumValues = 0.0;
    var sumProfits = 0.0;
    var sumRisks = 0.0;
    var sumBets = 0.0;
    if (currentProfits == null) {
      sumValues = null;
      sumProfits = null;
      sumRisks = null;
      sumBets = null;
    } else {
      for (e in portfolio) {
        sumValues += e.quote * e.stocks;
        sumProfits += fprofits(e.stocks, e.price, e.quote);
        sumRisks += frisk(e.stocks, e.price, e.ref);
        sumBets += fbet(e.stocks, e.quote, e.ref);
      }
    }

    wg
      .removeAll()
      .add(Q("div")
        .klass("head")
        .html(_("Profits")))
      .add(Q("table")
        .att("align", "center")
        .klass("home")
        .add(Q("tr")
          .add(Q("td")
            .klass("rlabel")
            .add(Q("span")
              .html(_("Current profits") + ":")))
          .add(Q("td")
            .klass("number")
            .add(Q("span")
              .html(
                "<font color='0041aa'>" +
                (currentProfits == null ? "[?]"
                  : formatN(currentProfits, 2)) +
                "</font>"))))
        .add(Q("tr")
          .add(Q("td")
            .klass("rlabel")
            .add(Q("span")
              .html(_("Accounting profits") + ":")))
          .add(Q("td")
            .klass("number")
            .add(Q("span")
              .html(formatN(accountProfits, 2)))))
        .add(Q("tr")
          .add(Q("td")
            .klass("rlabel")
            .add(Q("span")
              .html(_("Risk profits") + ":")))
          .add(Q("td")
            .klass("number")
            .add(Q("span")
              .html(
                "<font color='aa2800'>" +
                formatN(riskProfits, 2) +
                "</font>"))))
        .add(Q("tr")
          .add(Q("td")
            .klass("rlabel")
            .add(Q("span")
              .html(_("Total bet") + ":")))
          .add(Q("td")
            .klass("number")
            .add(Q("span")
              .html(
                "<font color='00aa41'>" +
                (formatN(bet, 2)) +
                "</font>"))))
        .add(Q("tr")
          .add(Q("td")
            .klass("rlabel")
            .add(Q("span")
              .html(_("To withdraw") + ":")))
          .add(Q("td")
            .klass("number")
            .add(Q("span")
              .html(mSel == -1 ? "- - -" : formatN(withdraw, 2)))))
        .add(Q("tr")
          .add(Q("td")
            .klass("rlabel")
            .add(Q("span")
              .html(_("Reference (‰)") + ":")))
          .add(Q("td")
            .klass("number")
            .add(Q("span")
              .html(
                '<font color="${fcolor(riskProfits)}">' +
                formatN(
                  bet * 1000 / (stocks + currentProfits - accountProfits),
                  0
                ) +
                "</font>")))))
      .add(Q("div")
        .klass("head")
        .html(_("Balance")))
      .add(Q("table")
        .att("align", "center")
        .klass("home")
        .add(Q("tr")
          .add(Q("td")
            .att("colspan", 2)
            .klass("head")
            .add(Q("span")
              .html(_("Assets"))))
          .add(balanceSeparator())
          .add(Q("td")
            .att("colspan", 2)
            .klass("head")
            .add(Q("span")
              .html(_("Equity")))))
        .add(Q("tr")
          .add(Q("td")
            .klass("rlabel")
            .add(Q("span")
              .html(_("Stocks") + ":")))
          .add(Q("td")
            .klass("number")
            .add(Q("span")
              .html(formatN(stocks, 2))))
          .add(balanceSeparator())
          .add(Q("td")
            .klass("rlabel")
            .add(Q("span")
              .html(_("Capital") + ":")))
          .add(Q("td")
            .klass("number")
            .add(Q("span")
              .html(formatN(capital, 2)))))
        .add(Q("tr")
          .add(Q("td")
            .klass("rlabel")
            .add(Q("span")
              .html(_("Cash") + ":")))
          .add(Q("td")
            .klass("number")
            .add(Q("span")
              .html(formatN(cash, 2))))
          .add(balanceSeparator())
          .add(Q("td")
            .klass("rlabel")
            .add(Q("span")
              .html(_("Sells") + ":")))
          .add(Q("td")
            .klass("number")
            .add(Q("span")
              .html(formatN(sells, 2)))))
        .add(Q("tr")
          .add(Q("td"))
          .add(Q("td"))
          .add(balanceSeparator())
          .add(Q("td")
            .klass("rlabel")
            .add(Q("span")
              .html(_("Fees") + ":")))
          .add(Q("td")
            .klass("number")
            .add(Q("span")
              .html(formatN(fees, 2)))))
        .add(Q("tr")
          .add(Q("td"))
          .add(Q("td"))
          .add(balanceSeparator())
          .add(Q("td")
            .klass("rlabel")
            .add(Q("span")
              .html(_("Profits") + ":")))
          .add(Q("td")
            .klass("number")
            .add(Q("span")
              .html(formatN(incomes, 2)))))
        .add(Q("tr")
          .add(Q("td"))
          .add(Q("td"))
          .add(balanceSeparator())
          .add(Q("td")
            .klass("rlabel")
            .add(Q("span")
              .html(_("Differences") + ":")))
          .add(Q("td")
            .klass("number")
            .add(Q("span")
              .html(formatN(differences, 2))))))
      .add(Q("div")
        .klass("head")
        .html(_("Stocks")))
      .add(Q("table")
        .att("align", "center")
        .klass("home")
        .add(Q("tr")
          .add(Q("td")
            .klass("head")
            .add(Ui.link(e -> {
              order = NICK_ORDER;
              this.body(wg, ld, pf);
            }).klass("linkBold").html(_("Co."))))
          .add(Q("td").klass("head")
            .add(Q("span").html("Nm.")))
          .add(Q("td").klass("head")
            .add(Q("span").html(_("Buy"))))
          .add(Q("td").klass("head")
            .add(Q("span").html(_("Sell"))))
          .add(Q("td").klass("head")
            .add(
              (currentProfits == null
                ? Q("span")
                : Ui.link(e -> {
                  order = VALUE_ORDER;
                  this.body(wg, ld, pf);
                }).klass("linkBold")
              ).html(_("Value"))))
          .add(Q("td")
            .klass("head")
            .add(
              (currentProfits == null
                ? Q("span")
                : Ui.link(e -> {
                  order = PROFITS_ORDER;
                  this.body(wg, ld, pf);
                }).klass("linkBold")
              ).html(_("Profits"))))
          .add(Q("td")
            .klass("head")
            .add(
              (currentProfits == null
                ? Q("span")
                : Ui.link(e -> {
                  order = RISK_ORDER;
                  this.body(wg, ld, pf);
                }).klass("linkBold")
              ).html(_("Risk"))))
          .add(Q("td")
            .klass("head")
            .add(
              (currentProfits == null
                ? Q("span")
                : Ui.link(e -> {
                  order = BET_ORDER;
                  this.body(wg, ld, pf);
                }).klass("linkBold")
              ).html(_("Bet"))))
          .add(Q("td")
            .klass("head")
            .add(
              (currentProfits == null
                ? Q("span")
                : Ui.link(e -> {
                  order = REF_ORDER;
                  this.body(wg, ld, pf);
                }).klass("linkBold")
              ).html(_("Rf. (‰)")))))
        .adds(portfolio.map(e -> Q("tr")
          .add(Q("td")
            .klass("nick")
            .add(Q("span")
              .html(e.nick)))
          .add(Q("td")
            .klass("number2")
            .add(Q("span")
              .html(formatN(e.stocks, 0))))
          .add(Q("td")
            .klass("number")
            .add(Q("span")
              .html(formatN(e.price, 4))))
          .add(Q("td")
            .klass("number")
            .add(Q("span")
              .html(e.quote == null ? "[?]" : formatN(e.quote, 4))))
          .add(Q("td")
            .klass("number")
            .add(Q("span")
              .html(e.quote == null ? "[?]" : formatN(e.quote * e.stocks, 2))))
          .add(Q("td") // Profits
            .klass("number")
            .add(Q("span")
              .html(e.quote == null
                ? "[?]"
                : formatN(fprofits(e.stocks, e.price, e.quote), 2))))
          .add(Q("td") // Risk
            .klass("number")
            .add(Q("span")
              .html(e.quote == null
                ? "[?]"
                : formatN(frisk(e.stocks, e.price, e.ref), 2))))
          .add(Q("td") // Bet
            .klass("number")
            .add(Q("span")
              .html(e.quote == null
                ? "[?]"
                : formatN(fbet(e.stocks, e.quote, e.ref), 2))))
          .add(Q("td")
            .klass("number") // Rf
            .add(Q("span")
              .html(e.quote == null
                ? "[?]"
                : formatN(fref(e.quote, e.ref), 0))))))
        .add(Q("tr")
          .add(Q("td"))
          .add(Q("td"))
          .add(Q("td"))
          .add(Q("td"))
          .add(Q("td")
            .klass("numberSum")
            .add(Q("span")
              .html(sumValues == null
                ? "[?]"
                : formatN(sumValues, 2))))
          .add(Q("td")
            .klass("numberSum")
            .add(Q("span")
              .html(sumProfits == null
                ? "[?]"
                : formatN(sumProfits, 2))))
          .add(Q("td")
            .klass("numberSum")
            .add(Q("span")
              .html(sumRisks == null
                ? "[?]"
                : formatN(sumRisks, 2))))
          .add(Q("td")
            .klass("numberSum")
            .add(Q("span")
              .html(sumBets == null
                ? "[?]"
                : formatN(sumBets, 2))))
          .add(Q("td"))
        ))
    ;

  }

  function view () {
    final wg = Q("div");
    final sel = mSel == -1 ? "All" : _("Inv-") + Std.string(mSel);

    final lopts = [
      Menu.toption("All", _("All"), () -> setMenu(-1))
    ];
    for (i in 0...ledgers.length) {
      final op = _("Inv-") + Std.string(i);
      lopts.push(Menu.separator());
      lopts.push(Menu.toption(op, op, () -> setMenu(i)));
    }
    final ropts = [];
    final menu = new Menu(lopts, ropts, sel);

    var ledger = new Ledger(0, 0, 0, 0, 0, 0, 0);
    var portfolio: Array<PfEntry> = [];
    if (mSel == -1) {
      for (man in 0...ledgers.length) {
        final l = ledgers[man];
        ledger = new Ledger(
          ledger.stocks + l.stocks, ledger.cash + l.cash,
          ledger.capital + l.capital, ledger.sells + l.sells,
          ledger.fees + l.fees, ledger.profits + l.profits,
          ledger.differences + l.differences
        );
        final pf = portfolios[man];
        for (i in 0...pf.length) {
          final e = pf[i];
          final nick = e.nick;
          final i2 = It.from(portfolio).indexf(e2 -> e2.nick == nick);
          if (i2 == -1) {
            portfolio.push(e);
          } else {
            final e2 = portfolio[i2];
            final stocks = e.stocks + e2.stocks;
            portfolio[i2] = new PfEntry(
              nick,
              stocks,
              (e.stocks * e.price + e2.stocks * e2.price) / stocks,
              e2.quote,
              (e.stocks * e.ref + e2.stocks * e2.ref) / stocks
            );
          }
        }
      }
    } else {
      ledger = ledgers[mSel];
      portfolio = portfolios[mSel];
    }

    body(wg, ledger, portfolio);

    this.wg
      .removeAll()
      .add(menu.wg)
      .add(wg)
    ;
  }

  // Control -------------------------------------------------------------------

  function setMenu (manager: Int) {
    mSel = manager;
    view();
  }

  // Static --------------------------------------------------------------------

  /// Constructor.
  ///   wg: Container.
  public static function mk (wg:Domo) {
    Cts.client.send([
      "module" => Js.ws("acc"),
      "source" => Js.ws("balance"),
      "rq" => Js.ws("idata")
    ], rp -> {
      final ledgers = rp["ledgers"].ra().map(e -> Ledger.fromJs(e));
      final portfolios = rp["portfolios"].ra()
        .map(p -> p.ra().map(e -> PfEntry.fromJs(e)))
      ;

      new Balance(wg, ledgers, portfolios);
    });
  }


  function balanceSeparator (): Domo {
    return Q("td").klass("separator");
  }

  function fprofits (stocks: Int, buy: Float, current: Float): Float {
    return (current - buy) * stocks;
  }

  function frisk (stocks: Int, buy: Float, ref: Float): Float {
    return (ref - buy) * stocks;
  }

  function fbet (stocks: Int, current: Float, ref: Float): Float {
    return (current - ref) * stocks;
  }

  function fref (current: Float, ref: Float): Float {
    return (current - ref) * 1000 / ref;
  }

  function fcolor (value: Float): String {
    if (value < 0) return "aa2800";
    return "0041aa";
  }

  function formatN (n: Float, dec: Int): String {
    return n == Math.POSITIVE_INFINITY ? "∞"
      : n == Math.NEGATIVE_INFINITY ? "-∞"
        : n == Math.NaN ? "[?]" : Dec.toIso(n, dec);
  }

}

