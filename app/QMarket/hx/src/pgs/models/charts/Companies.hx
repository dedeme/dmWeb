// Copyright 14-Nov-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pgs.models.charts;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Js;
import dm.Dec;
import dm.It;
import dm.ModalBox;
import data.Cts;
import data.model.Model;
import pgs.models.wgs.HistoricChart;
import I18n._;

/// Model companies charts.
class Companies {
  var wg: Domo;
  var model: Model;
  var nicks: Array<String>;

  function new ( wg: Domo, model: Model, nicks: Array<String>
  ) {
    nicks.sort((e1, e2) -> e1 > e2 ? 1 : -1);
    this.wg = wg;
    this.model = model;
    this.nicks = nicks;

    view();
  }

  // View ----------------------------------------------------------------------

  function view () {
    final summary = Q("table").att("align", "center");
    final chs = Q("table").att("align", "center").klass("frame");
    wg
      .removeAll()
      .add(Q("div")
        .klass("head")
        .text(_("Profits")))
      .add(summary)
      .add(chs)
    ;

    function addSeparator () {
      return chs.add(Q("tr")
        .add(Q("td")
          .att("colspan", 3)
          .html("<hr>")))
      ;
    }

    final n = nicks.length;
    var sumProfs = 0.0;
    var sumWins = 0;
    var sumLosses = 0;
    var tr = Q("tr");
    It.range(n).eachSync(
      (i, fn) -> {
        final nickName = nicks[i];
        mkChart(i, model, nickName, fn);
      },
      rs -> {
        final iNick: Int = rs.iNick;
        final chart: Domo = rs.chart;
        final profits: Float = rs.profits;
        sumProfs += profits;
        if (profits < 0) ++sumLosses;
        else ++sumWins;

        switch (iNick % 3) {
        case 0:
          addSeparator();
          tr = Q("tr");
          tr.add(Q("td").add(chart));
        case 2:
          tr.add(Q("td").add(chart));
          chs.add(tr);
        default:
          tr.add(Q("td").add(chart));
        }
        return;
      },
      () -> {
        switch (n % 3) {
          case 1: chs.add(tr.add(Q("td")).add(Q("td")));
          case 2: chs.add(tr.add(Q("td")));
        }
        addSeparator();

        final profits = sumProfs / n;
        summary.add(Q("tr")
          .add(Q("td")
            .add(Ui.img("profits").style("vertical-align:middle"))
            .add(Q("span").html(': ${sumWins} | '))
            .add(Ui.img("losses").style("vertical-align:middle"))
            .add(Q("span").html(
              ': ${sumLosses} | % : ${Dec.toIso(profits * 100, 2)}%'
            ))))
        ;
      }
    );
  }

  // Static --------------------------------------------------------------------

  /// Constructor.
  ///   wg     : Container.
  ///   modelId: Flea model identifier.
  ///   params : Flea parameters.
  public static function mk (wg: Domo, model: Model) {
    Cts.client.send([
      "module" => Js.ws("models"),
      "source" => Js.ws("companies"),
      "rq" => Js.ws("nickList")
    ], rp -> {
      final nicks = rp["nickList"].ra().map(e -> e.rs());

      new Companies(wg, model, nicks);
    });
  }

  static function mkChart (
    iNick: Int, model: Model, nickName: String,
    fn: {iNick: Int, chart: Domo, profits: Float} -> Void
  ) {
    function mkCh (profits: Float, chart: Domo) {
      final wg = Q("div");
      return Q("table")
        .add(Q("tr")
          .add(Q("td")
            .style("text-align:left")
            .text(nickName))
          .add(Q("td")
            .style("text-align:right")
            .add(Q("span")
              .html(Dec.toIso(profits * 100, 2) + "%&nbsp;&nbsp;"))
            .add(Ui.img(
              profits > 0 ? "profits" : profits < 0 ? "losses" : "noresult"
            ).style("vertical-align:middle"))))
        .add(Q("tr")
          .add(Q("td")
            .att("colspan", 2)
            .add(wg)
            .add(chart
              .klass("frame")
              .setStyle("cursor", "pointer")
              .on(CLICK, e -> {
                Cts.client.send([
                  "module" => Js.ws("models"),
                  "source" => Js.ws("companies"),
                  "rq" => Js.ws("chartData"),
                  "qlevel" => Js.wi(model.qlevel),
                  "id" => Js.wi(model.id),
                  "nickName" => Js.ws(nickName)
                ], rp -> {
                var chart = new HistoricChart(true, []).wg;
                  if (rp["ok"].rb()) {
                    final dates = rp["dates"].ra().map(e -> e.rs());
                    final closes = rp["closes"].ra().map(e -> e.rf());
                    final refs = rp["refs"].ra().map(e -> e.rf());
                    final qs = [];
                    for (i in 0...dates.length) {
                      qs.push(new HistoricChartEntry(
                        dates[i], closes[i], refs[i]
                      ));
                    }

                    chart = new HistoricChart(true, qs).wg;
                  }
                  final closeBt  = Q("button").text(_("Close"));
                  final mb = new ModalBox(
                    Q("table")
                      .add(Q("tr")
                        .add(Q("td")
                          .style("text-align:center")
                          .text(nickName)))
                      .add(Q("tr")
                        .add(Q("td")
                          .add(chart
                            .klass("frame"))))
                      .add(Q("tr")
                        .add(Q("td")
                          .style("text-align:center")
                          .add(closeBt)))
                  );
                  closeBt.on(CLICK, e -> mb.show(false));
                  wg
                    .removeAll()
                    .add(mb.wg)
                  ;
                  mb.show(true);
                });
              }))))
      ;
    }

    Cts.client.send([
      "module" => Js.ws("models"),
      "source" => Js.ws("companies"),
      "rq" => Js.ws("chartData"),
      "qlevel" => Js.wi(model.qlevel),
      "id" => Js.wi(model.id),
      "nickName" => Js.ws(nickName)
    ], rp -> {
      if (!rp["ok"].rb()) {
        fn({
          iNick: iNick,
          chart: mkCh(0, new HistoricChart(false, []).wg),
          profits: 0
        });
        return;
      }

      final dates = rp["dates"].ra().map(e -> e.rs());
      final closes = rp["closes"].ra().map(e -> e.rf());
      final refs = rp["refs"].ra().map(e -> e.rf());
      final profits = rp["profits"].rf();
      final qs = [];
      for (i in 0...dates.length) {
        qs.push(new HistoricChartEntry(
          dates[i], closes[i], refs[i]
        ));
      }

      fn({
        iNick: iNick,
        chart: mkCh(profits, new HistoricChart(false, qs).wg),
        profits: profits
      });
    });
  }
}
