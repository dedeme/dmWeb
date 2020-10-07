// Copyright 22-Sep-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

using StringTools;
import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Opt;
import dm.It;
import dm.Dec;
import dm.Dt;
import dm.DatePicker;
import dm.Menu;
import data.All;
import data.Year;
import data.Ann;
import data.Report;
import I18n._;
import I18n._args;

class Annotations {
  static var wg: Domo;
  static var allData: All;
  static var update: Void -> Void;
  static var data: Year;

  /// Constructor
  ///   wg; Container.
  public static function mk (wg: Domo) {
    Annotations.wg = wg;
    All.request(all -> {
      allData = all;
      update = () -> all.send(() -> mk(wg));
      data = all.lastYear();
      show();
    });
  }

  static function entries (td: Domo): Array<Domo> {
    function entry (ann: Ann): Domo {
      return Q("tr")
        .add(Q("td")
          .add(Ui.link(e -> delete(ann))
            .add(Ui.img("delete"))))
        .add(Q("td")
          .add(Ui.link(e -> mkAnns(td, Some(ann)))
            .add(Ui.img("edit"))))
        .add(Q("td")
          .add(ann.isSell ? Ui.led("#4080ff") : Ui.led("#ff8040")))
        .add(Q("td").klass("frame").text(Dt.toIso(ann.date)))
        .add(Q("td").klass("frame").text(Std.string(ann.inv)))
        .add(Q("td").klass("frame").text(ann.nick))
        .add(Q("td").klass("number2").text(Dec.toIso(ann.stocks, 0)))
        .add(Q("td").klass("number2").text(Dec.toIso(ann.price, 4)))
        .add(Q("td").klass("number2").text(Dec.toIso(ann.cash, 2)))
      ;
    }
    return It.from(data.anns).reverse().map(e -> entry(e)).to();
  }

  static function sumaryTrs(sum: Array<ReportSummary>): Array<Domo> {
    function mkRow(e: ReportSummary) {
      return Q("tr")
        .add(Q("td")
          .text(e.nick))
        .add(Q("td")
          .klass("number2")
          .text(Dec.toIso(e.stocks, 0)))
        .add(Q("td")
          .klass("number")
          .text(Dec.toIso(e.price, 4)))
        .add(Q("td")
          .klass("number")
          .text(Dec.toIso(e.total, 2)))
      ;
    }
    return sum.map(e -> mkRow(e));
  }

  static function annsTrs(anns: Array<ReportAnn>): Array<Domo> {
    function mkRow(ann: ReportAnn) {
      return Q("tr")
        .add(Q("td")
          .add(ann.profits == None ? Ui.led("#ff8040") : Ui.led("#4080ff") ))
        .add(Q("td").klass("frame").text(Dt.toIso(ann.date)))
        .add(Q("td").klass("frame").text(ann.nick))
        .add(Q("td").klass("number2").text(Dec.toIso(ann.stocks, 0)))
        .add(Q("td").klass("number").text(Dec.toIso(ann.price, 4)))
        .add(Q("td").klass("number2").text(Dec.toIso(ann.total, 2)))
        .add(Q("td")
          .klass(ann.profits == None ? "header" : "number")
          .text(
            switch (ann.profits) {
              case Some(v): Dec.toIso(v, 2);
              case None: "";
            }
          ))
        .add(Q("td")
          .klass(ann.fees == None ? "header" : "number")
          .text(
            switch (ann.fees) {
              case Some(v): Dec.toIso(v, 2);
              case None: "";
            }
          ))
      ;
    }

    if (anns.length == 0) {
      return [
        Q("tr")
          .add(Q("td")
            .klass("frame")
            .text(_("Without Annotations")))
      ];
    }
    return It.from(anns).reverse().map(e -> mkRow(e)).to();
  }

  // Annotations ---------------------------------------------------------------
  static function mkAnns (td: Domo, ann: Option<Ann>) {
    function th () {
      return Q("td")
        .style("text-align: center;padding-left: 4px; padding-right: 4px")
      ;
    }

    final state = Ui.led(data.error == "" ? "#80C0E0" : "#E0C080")
      .att("title", data.error == "" ? _("Data is ok") : _("An error is found"))
    ;
    if (data.error != "") {
      state.setStyle("cursor", "pointer");
      state.on(CLICK, e -> Ui.alert(data.error));
    }

    final dateWg = Q("input").style("width: 100px;text-align:center");
    final datePk = new DatePicker();
    datePk.action = s -> {}

    final buyBt = Q("input")
      .att("type", "radio")
      .att("name", "operation")
      .att("checked", "true")
    ;

    final sellBt = Q("input")
      .att("type", "radio")
      .att("name", "operation")
    ;

    final invBts: Array<Domo> = [];
    for (i in 0...Cts.investors) {
      final ix = i;
      invBts.push(Q("input")
        .att("type", "radio")
        .att("name", "investor")
      );
    }
    invBts[0].att("checked", "true");
    final invsWg = Q("div")
      .adds(It.range(invBts.length).map(i ->
          Q("span")
            .add(invBts[i])
            .add(Q("span")
              .html("<big> " + Std.string(i) + "</big>&nbsp;&nbsp;"))
        ).to())
    ;

    final nickWg = Ui.field("stocks")
      .style("width: 50px")
    ;

    final stocksWg = Ui.field("price")
      .att("id", "stocks")
      .style("width: 50px")
    ;

    final priceWg = Ui.field("cash")
      .att("id", "price")
      .style("width: 100px")
    ;
    Ui.changePoint(priceWg);

    final cashWg = Ui.field("accept")
      .att("id", "cash")
      .style("width: 100px")
    ;
    Ui.changePoint(cashWg);


    final cancelBt = Q("button")
      .text(_("Cancel"))
      .on(CLICK, e -> cancel())
    ;

    final acceptBt = Q("button")
      .att("id", "accept")
      .text(_("Accept"))
      .on(CLICK, e -> accept(
          ann, buyBt, sellBt, dateWg, invBts, nickWg, stocksWg, priceWg, cashWg
        ))
    ;

    final a = Opt.get(ann);
    if (a != null) {
      if (a.isSell) {
        buyBt.checked(false);
        sellBt.checked(true);
      }
      datePk.date = a.date;
      for (i in 0...invBts.length) {
        invBts[i].checked(i == a.inv);
      }
      nickWg.value(a.nick);
      stocksWg.value(Dec.toIso(a.stocks, 0));
      priceWg.value(Dec.toIso(a.price, 4));
      cashWg.value(Dec.toIso(a.cash, 2));
    }

    final datePicker = datePk.mkText(dateWg);

    final editor = Q("table")
      .style(
        "background: " +
        (ann == None ? "rgb(240, 245, 250)" : "rgb(250, 250, 230)")
      )
      .add(Q("tr")
        .add(Q("td")
          .att("colspan", "2")
            .add(state))
        .add(Q("td"))
        .add(Q("td")))
      .add(Q("tr")
        .add(th()
          .text(_("Buy")))
        .add(th()
          .text(_("Sell")))
        .add(th()
          .text(_("Date")))
        .add(th()
          .text(_("Inv."))))
      .add(Q("tr")
        .add(th()
          .add(buyBt))
        .add(th()
          .add(sellBt))
        .add(Q("td")
          .add(datePicker))
        .add(Q("td")
          .klass("border")
          .style("white-space: nowrap")
          .add(invsWg)))
      .add(Q("tr")
        .add(th()
          .text(_("Nick")))
        .add(th()
          .text(_("Stocks")))
        .add(th()
          .text(_("Price")))
        .add(th()
          .text(_("Cash"))))
      .add(Q("tr")
        .add(th()
          .add(nickWg))
        .add(th()
          .add(stocksWg))
        .add(Q("td")
          .add(priceWg))
        .add(Q("td")
          .add(cashWg)))
      .add(Q("tr")
        .add(Q("td")
          .att("colspan", 4)
          .style("text-align: right")
          .add(cancelBt)
          .add(Q("span")
            .html("&nbsp;&nbsp;"))
          .add(acceptBt)))
    ;


    var list = Q("table")
        .klass("frame")
        .att("align", "center")
    ;
    if (data.anns.length == 0) {
      list
        .add(Q("tr")
          .add(Q("td")
            .text(_("Without Data"))))
      ;
    } else {
      list.adds(entries(td));
    }

    td
      .removeAll()
      .add(editor)
      .add(Q("hr"))
      .add(list)
    ;
  }

  // Reports -------------------------------------------------------------------

  // If isel == Report.ALL, data is from 'all'.
  // If isel == Report.WITH_FEES, data is from 'with'.
  // Otherwise is "Inv-" + isel
  static function mkReports (td: Domo, isel: Int): Void {
    final sel = isel == Report.ALL
      ? "all"
      : isel == Report.WITH_FEES
        ? "with"
        : "Inv-" + Std.string(isel)
    ;

    var lopts = [Menu.toption("all", _("All"), () -> mkReports(td, -1))];
    for (i in 0...Cts.investors) {
      final ix = i;
      final name = "Inv-" + Std.string(i);
      lopts.push(Menu.separator());
      lopts.push(Menu.toption(name, name, () -> mkReports(td, ix)));
    }
    var ropts = [
      Menu.toption("with", _("With Fees"), () -> mkReports(td, -2))
    ];
    final menu = new Menu(lopts, ropts, sel);

    final body = Q("div");
    final report = data.report(isel);

    body
      .add(Q("table")
        .klass("border")
        .att("align", "center")
        .add(Q("tr")
          .add(Q("td")
            .style("text-align: right; width: 80px")
            .text(_("Cost" + ":")))
          .add(Q("td")
            .klass("number")
            .text(Dec.toIso(report.cost, 2))))
        .add(Q("tr")
          .add(Q("td")
            .style("text-align: right; width: 80px")
            .text(_("Profits") + ":"))
          .add(Q("td")
            .klass("number")
            .text(Dec.toIso(report.profits, 2))))
        .add(Q("tr")
          .add(Q("td")
            .style("text-align: right; width: 80px")
            .text(_("Fees") + ":"))
          .add(Q("td")
            .klass(report.fees == None ? "header" : "number")
            .text(switch(report.fees) {
                case Some(v): Dec.toIso(v, 2);
                case None: "";
              }))))
      .add(Q("div")
        .klass("head")
        .text(_("Stocks")))
      .add(Q("table")
        .klass("border")
        .att("align", "center")
        .add(Q("tr")
          .add(Q("td")
            .klass("header")
            .text(_("Nick")))
          .add(Q("td")
            .klass("header")
            .text(_("Stocks")))
          .add(Q("td")
            .klass("header")
            .text(_("Price")))
          .add(Q("td")
            .klass("header")
            .text(_("Total"))))
        .adds(sumaryTrs(report.summary)))
      .add(Q("div")
        .klass("head")
        .text(_("Annotations")))
      .add(Q("table")
        .klass("border")
        .att("align", "center")
        .add(Q("tr")
          .add(Q("td")
            .klass("header")
            .text(""))
          .add(Q("td")
            .klass("header")
            .text(_("Date")))
          .add(Q("td")
            .klass("header")
            .text(_("Nick")))
          .add(Q("td")
            .klass("header")
            .text(_("Stocks")))
          .add(Q("td")
            .klass("header")
            .text(_("Price")))
          .add(Q("td")
            .klass("header")
            .text(_("Total")))
          .add(Q("td")
            .klass("header")
            .text(_("Profits")))
          .add(Q("td")
            .klass("header")
            .text(_("Fees"))))
        .adds(annsTrs(report.anns)))
    ;

    td
      .removeAll()
      .add(menu.wg)
      .add(body)
    ;
  }

  // Main ----------------------------------------------------------------------
  static function show () {
    final annsTd = Q("td")
      .klass("border")
      .style("width: 5px;vertical-align:top")
    ;
    mkAnns(annsTd, None);

    final reportsTd = Q("div")
      .klass("border")
      .style("vertical-align:top; padding-top: 3px; padding-left: 2px")
    ;
    mkReports(reportsTd, Report.ALL);

    wg
      .removeAll()
      .add(Q("table")
        .klass("main")
        .add(Q("tr")
          .add(annsTd)
          .add(Q("td")
            .klass("border"))
          .add(reportsTd)))
    ;
  }

  // Control -------------------------------------------------------------------

  static function cancel (): Void {
    mk(wg);
  }

  static function accept (
    ann: Option<Ann>,
    buyBt: Domo, sellBt: Domo, dateWg: Domo, invBts: Array<Domo>,
    nickWg: Domo, stocksWg: Domo, priceWg: Domo, cashWg: Domo
  ): Void {
    final isSell = sellBt.getChecked();
    var inv = 0;
    for (i in 0...invBts.length) {
      if (invBts[i].getChecked())
        inv = i;
    }
    var date: Date;
    var nick: String;
    var stocks: Int;
    var price: Float;
    var cash: Float;
    function validate (): String {
      final dateTx = cast(dateWg.getValue(), String).trim();
      if (dateTx == "")
        return _("Date is missing");
      date = Opt.get(Dt.fromIso(dateTx));
      if (date == null)
        return _("Date is not valid");
      final year = Dt.year(Date.now());
      if (year != Dt.year(date))
        return _("Date year is not the current year");

      nick = cast(nickWg.getValue(), String).trim();
      if (nick == "")
        return _("Nick is missing");

      stocks = Opt.get(Cts.int(stocksWg));
      if (stocks == null)
        return _("Stocks is not a valid integer");
      if (stocks <= 0)
        return _("Stocks number <= 0");

      price = Opt.get(Cts.float(priceWg, 4));
      if (price == null)
        return _("Price is not a valid number");
      if (price < 0)
        return _("Price < 0");

      cash = Opt.get(Cts.float(cashWg, 2));
      if (cash == null)
        return _("Cash is not a valid number");
      if (cash < 0)
        return _("Cash < 0");
      if (isSell && cash > stocks * price)
        return _("'Cash > Stocks * Price' in a sell");
      if (!isSell && cash < stocks * price)
        return _("'Cash < Stocks * Price' in a buy");

      return "";
    }

    final error = validate();
    if (error != "") {
      Ui.alert(error);
      return;
    }

    if (!allData.duplicateNick(nick)) {
      if (!Ui.confirm(_("A new nick is to be added.\nContinue?")))
        return;
    }

    data.add(ann, new Ann(isSell, date, inv, nick, stocks, price, cash));
    update();

    mk(wg);
  }

  static function delete (ann: Ann): Void {
    if (Ui.confirm(_args(_("%0\nDelete annotation?"), [ann.toString()]))) {
      data.delete(ann.id);
      update();

      mk(wg);
    }
  }
}
