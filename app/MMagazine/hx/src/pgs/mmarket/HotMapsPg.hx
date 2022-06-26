// Copyright 27-Jan-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pgs.mmarket;

using StringTools;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Js;
import dm.It;
import dm.Dt;
import dm.Opt;
import dm.Dec;
import dm.Menu;
import data.HotMap;
import data.ParamsEval;
import I18n._;

/// Hot maps page.
class HotMapsPg {
  final wg: Domo;
  final models: Array<String>;
  final modelSel: String;
  final mapsGroups: Array<HotMap>;

  function new (
    wg: Domo, models: Array<String>, modelSel: String, mapsGroups: Array<HotMap>
  ) {
    this.wg = wg;
    this.models = models;
    this.modelSel = modelSel;
    this.mapsGroups = mapsGroups;
  }

  // View ----------------------------------------------------------------------

  public function show () {
    final lopts = [];
    for (m in models) {
      lopts.push(Menu.separator());
      lopts.push(Menu.tlink(m, m, "mmarket&hotmaps"));
    }
    lopts.splice(0, 1);
    final menu = new Menu(lopts, [], modelSel);


    final groups = mapsGroups.length;
    wg
      .removeAll()
      .add(menu.wg)
      .add(rowGroups(0, groups >= 4 ? 4 : groups))
    ;

    if (groups >= 4)
      wg
        .add(rowGroups(4, groups >= 8 ? 8 : groups))
      ;

    if (groups >= 8)
      wg
        .add(Q("hr"))
        .add(rowGroups(8, groups >= 13 ? 13 : groups))
      ;

    if (groups >= 13)
      wg
        .add(rowGroups(13, groups >= 18 ? 18 : groups))
      ;

    if (groups >= 18)
      wg
        .add(Q("hr"))
        .add(rowGroups(18, groups))
      ;
  }

  function rowGroups (start: Int, end: Int): Domo {
    return Q("table")
      .att("align", "center")
      .add(Q("tr")
        .adds(It.range(start, end).map(i -> Q("td")
            .add(mapChart(mapsGroups[i]))
        ).to()))
    ;
  }

  function mapChart(map: HotMap): Domo {
    return Q("table")
      .klass("flat")
      .add(Q("tr")
        .add(Q("td")
          .klass("frame")
          .style("text-align:center")
          .text(Dt.toIso(Opt.get(Dt.from(map.date))))))
      .add(Q("tr")
        .add(Q("td")
          .add(map.entries[0].params.length == 1
            ? oneChart(map.entries)
            : twoChart(map.entries))))

    ;
  }

  function oneChart (es: Array<ParamsEval>): Domo {
    final nfmt = Fns.paramFormatter(es[0].params[0], es[1].params[0]);
    final max = It.from(es)
      .reduce(es[0].eval, (r, e) -> e.eval > r ? e.eval : r);
    final min = It.from(es)
      .reduce(es[0].eval, (r, e) -> e.eval < r ? e.eval : r);
    final color = Fns.valueColor(max, min);

    return Q("table")
      .klass("flat")
      .adds(es.map(e -> Q("tr")
        .add(Q("td")
          .style(
              "padding:0px;" +
              "width: 120px; height: 6px;" +
              "background: " + color(e.eval)
            )
          .att(
              "title",
              nfmt(e.params[0]) + "\n" +
              Dec.toIso(e.eval / 100, 2)
            ))))
      ;
  }

  function twoChart (es: Array<ParamsEval>): Domo {
    final firstParam = es[0].params[0];
    final cols = It.from(es).takeWhile(e -> e.params[0] == firstParam).count();
    final rows = Std.int(es.length / cols);
    final nfmt0 = Fns.paramFormatter(es[0].params[0], es[cols].params[0]);
    final nfmt1 = Fns.paramFormatter(es[0].params[1], es[1].params[1]);
    final max = It.from(es)
      .reduce(es[0].eval, (r, e) -> e.eval > r ? e.eval : r);
    final min = It.from(es)
      .reduce(es[0].eval, (r, e) -> e.eval < r ? e.eval : r);
    final color = Fns.valueColor(max, min);

    return Q("table")
      .klass("flat")
      .adds(It.range(0, rows).map(row -> Q("tr")
          .adds(It.range(0, cols).map(col -> {
              final i = row * cols + col;
              final e = es[i];
              return Q("td")
                .style(
                    "padding:0px;" +
                    "width: 6px; height: 6px;" +
                    "background: " + color(e.eval)
                  )
                .att(
                    "title",
                    nfmt0(e.params[0]) + " - " +
                    nfmt1(e.params[1]) + "\n" +
                    Dec.toIso(e.eval / 100, 2)
                  )
              ;
            }).to())
        ).to())
    ;

  }

  // Control -------------------------------------------------------------------


  // Static --------------------------------------------------------------------

  public static function mk (wg: Domo): Void {
    final url = Ui.url();
    final model = url.exists("2") ? url.get("2").trim() : "";

    Cts.client.send([
      "module" => Js.ws("MMarket"),
      "source" => Js.ws("HotMapsPg"),
      "rq" => Js.ws("idata"),
      "model" => Js.ws(model)
    ], rp -> {
      final models = rp["models"].ra().map(e -> e.rs());
      final model = rp["model"].rs();
      final mapsGroup = rp["mapsGroup"].ra().map(e -> HotMap.fromJs(e));

      new HotMapsPg(wg, models, model, mapsGroup).show();
    });
  }


}
