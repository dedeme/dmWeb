// Copyright 16-Jun-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pgs.models.tests;

using StringTools;
import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Js;
import dm.It;
import dm.Dec;
import data.Model;
import wgs.Params;
import wgs.Msg;
import I18n._;
import I18n._args;
import pgs.models.wgs.HistoricChart;
import pgs.models.wgs.Operations;

/// References test.
class References {
  var wg: Domo;
  var model: Model;
  var nickList: Array<String>;
  var nickSel: Int;
  var wgParams: Params;

  final chartDiv = Q("div");
  final ordersDiv = Q("div");
  final resultDiv = Q("div");

  function new (wg: Domo, model: Model, nickList: Array<String>) {
    this.wg = wg;
    this.model = model;
    nickList.sort((e1, e2) -> e1 > e2 ? 1 : -1);
    this.nickList = nickList;
    nickSel = 0;
    wgParams = new Params(
      model.paramNames, model.paramMins, model.paramMaxs, "par", "showBt"
    );

    view();
  }

  // View ----------------------------------------------------------------------

  function view () {
    final nkRows = [];
    var row = Q("tr");
    It.from(nickList).eachIx((nk, i) -> {
      if (i != nickSel) {
        nk = (nk + "   ").substring(0, 6).replace(" ", "&nbsp;");
      } else {
        nk = (nk + " ").substring(0, 4).replace(" ", "&nbsp;");
      }
      if (i % 10 == 0 && i != 0) {
        nkRows.push(row);
        row = Q("tr");
      }
      row.add(Q("td")
        .style("text-align:left")
        .add(Ui.link(e -> selNick(i))
          .klass(i == nickSel ? "link frame" : "link")
          .setStyle("font-family", "monospace")
          .html(nk))
      );
    });
    final mod = nickList.length % 10;
    if (mod > 0) {
      for (i in 0...(10 - mod)) row.add(Q("td"));
      nkRows.push(row);
    } else {
      nkRows.push(row);
    }

    wg
      .removeAll()
      .add(Q("table").klass("main")
        .add(Q("tr")
          .add(Q("td")
            .style("width:50%;text-align:center")
            .add(Q("div").klass("head")
              .text(_("Companies")))
            .add(Q("table")
              .klass("frame3")
              .att("align", "center")
              .style("padding: 5px")
              .adds(nkRows)))
          .add(Q("td")
            .style("width:50%;text-align:center")
            .add(Q("div")
              .klass("head")
              .text(_("Parameters")))
            .add(Q("table")
              .att("align", "center")
              .add(Q("tr")
                .add(Q("td")
                  .add(wgParams.wg))))))
        .add(Q("tr")
          .add(Q("td")
            .att("colspan", 2)
            .style("text-align:center")
            .add(Q("div")
              .style("height:5px"))
            .add(Q("button")
              .att("id", "showBt")
              .text(_("Show"))
              .on(CLICK, e -> showChart()))
            .add(Q("div")
              .style("height:5px"))
            .add(chartDiv.removeAll()
              .add(new HistoricChart(true, []).wg.klass("frame0")))
            .add(Q("div")
              .style("height:5px"))
            .add(ordersDiv.removeAll())
            .add(Q("div")
              .style("height:5px"))
            .add(resultDiv.removeAll())))
      );
  }

  // Control -------------------------------------------------------------------

  function showChart () {
    Cts.client.send([
      "module" => Js.ws("models"),
      "source" => Js.ws("tests/references"),
      "rq" => Js.ws("chartData"),
      "modelId" => Js.ws(model.id),
      "nickName" => Js.ws(nickList[nickSel]),
      "params" => Js.wa(wgParams.value.map(e -> Js.wf(e)))
    ], rp -> {
      if (!rp["ok"].rb()) {
        Msg.error(Cts.failMsg);
        return;
      }

      final dates = rp["dates"].ra().map(e -> e.rs());
      final closes = rp["closes"].ra().map(e -> e.rf());
      final opens = rp["opens"].ra().map(e -> e.rf());
      final refs = rp["refs"].ra().map(e -> e.rf());
      final qs = [];
      for (i in 0...dates.length) {
        qs.push(new HistoricChartEntry(dates[i], closes[i], refs[i]));
      }
      chartDiv.removeAll()
        .add(new HistoricChart(true, qs).wg.klass("frame0"));

      ordersDiv.removeAll()
        .add(new Operations(dates, opens, closes, refs).wg.klass("frame0"));

      resultDiv.removeAll()
        .add(Q("div")
          .text(_args(
            _("Profits calculated by server: %0"),
            [Dec.toIso(rp["profits"].rf(), 4)]
          )));
    });
  }

  function selNick (i: Int) {
    nickSel = i;
    view();
  }

  // Static --------------------------------------------------------------------

  /// Constructor.
  ///   wg   : Container.
  ///   model: Model to test.
  public static function mk (wg: Domo, model: Model) {
    Cts.client.send([
      "module" => Js.ws("models"),
      "source" => Js.ws("tests/references"),
      "rq" => Js.ws("nickList")
    ], rp -> {
      new References(wg, model, rp["nickList"].ra().map(e -> e.rs()));
    });
  }
}
