// Copyright 01-Jul-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pgs.fleas.ranges;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Js;
import dm.Dec;
import data.flea.Fmodel;
import data.Cts;
import I18n._;

/// Ranges study page.
class Ranges {
  final wg: Domo;
  final md: Fmodel;
  final param: Array<Int>;
  final values: Array<Entry>;

  function new (
    wg: Domo, md: Fmodel, param: Array<Int>, values: Array<Entry>
  ) {
    this.wg = wg;
    this.md = md;
    this.param = param;
    this.values = values;

    view();
  }

  // View ----------------------------------------------------------------------

  function view () {
    wg
      .removeAll()
      .add(head())
      .add(body1())
      .add(Q("hr"))
      .add(body2())
    ;
    js.Browser.window.scroll(0, 0);
  }

  function head (): Domo {
    final tds: Array<Domo> = [];
    if (param.length == 0) {
      tds.push(Q("td").klass("frame").text("*"));
    } else {
      tds.push(Q("td")
        .add(Ui.link(e -> changeParam(Js.wa([])))
          .klass("link")
          .text("*")))
      ;
      if (param.length == 1) {
        tds.push(Q("td").klass("frame").text(Std.string(param[0])));
      } else {
        tds.push(Q("td")
          .add(Ui.link(e -> changeParam(Js.wa([Js.wi(param[0])])))
            .klass("link")
            .text(Std.string(param[0]))))
        ;
        tds.push(Q("td").text(","));
        if (param.length == 2) {
          tds.push(Q("td").klass("frame").text(Std.string(param[1])));
        } else {
          tds.push(Q("td")
            .add(Ui.link(e -> changeParam(
                  Js.wa([Js.wi(param[0]), Js.wi(param[1])])
              ))
              .klass("link")
              .text(Std.string(param[1]))))
          ;
          tds.push(Q("td").klass("frame").text(Std.string(param[2])));
        }
      }
    }

    return Q("table")
      .att("align", "center")
      .add(Q("tr")
        .adds(tds))
    ;
  }

  function body1 (): Domo {
    final trs: Array<Domo> = [];
    var n = 0;
    var evalSum = 0.0;
    var salesSum = 0;
    var currentParam = formatParam1(values[0].param);
    for (v in values) {
      final newParam = formatParam1(v.param);
      if (newParam == currentParam) {
        n++;
        evalSum += v.eval;
        salesSum += v.sales;
        continue;
      }

      final evalAvg = evalSum / n;
      final salesAvg = Std.int(salesSum / n);
      final param = currentParam;

      n = 1;
      evalSum = v.eval;
      salesSum = v.sales;
      currentParam = newParam;

      trs.push(Q("tr")
        .add(Q("td")
          .klass("fnumber")
          .add(Ui.link(e -> goIn(v.param))
            .klass("link")
            .text(param)))
        .add(Q("td")
          .klass("fnumber")
          .text(formatEval(evalAvg)))
        .add(Q("td")
          .add(Q("table")
            .add(Q("tr")
              .add(Q("td")
                .style("width:" + Std.int(evalAvg * 1000) + "px")
                .add(Q("hr")))
              .add(Q("td")))))
        .add(Q("td")
          .klass("fnumber")
          .text(formatSales(salesAvg)))
      );
    }

    return Q("table")
      .att("align", "center")
      .adds(trs)
    ;
  }

  function body2 (): Domo {
    final trs: Array<Domo> = [];
    for (v in values) {
      trs.push(Q("tr")
        .add(Q("td")
          .klass("fnumber")
          .add(Ui.link(e -> goIn(v.param))
            .klass("link")
            .text(formatParam(v.param))))
        .add(Q("td")
          .klass("fnumber")
          .text(formatEval(v.eval)))
        .add(Q("td")
          .add(Q("table")
            .add(Q("tr")
              .add(Q("td")
                .style("width:" + Std.int(v.eval * 1000) + "px")
                .add(Q("hr")))
              .add(Q("td")))))
        .add(Q("td")
          .klass("fnumber")
          .text(formatSales(v.sales)))
      );
    }

    return Q("table")
      .att("align", "center")
      .adds(trs)
    ;
  }

  function formatParam (n: Float): String {
    return switch (param.length) {
      case 1: Dec.toIso(n * 100, 2);
      case 2: Dec.toIso(n * 100, 3);
      case 3: Dec.toIso(n * 100, 4);
      default: Dec.toIso(n * 100, 1);
    }
  }

  function formatParam1 (n: Float): String {
    return switch (param.length) {
      case 1: Dec.toIso(n * 100, 1);
      case 2: Dec.toIso(n * 100, 2);
      case 3: Dec.toIso(n * 100, 3);
      default: Dec.toIso(n * 100, 0);
    }
  }

  function formatEval (n: Float): String {
    return Dec.toIso(n * 1000, 2);
  }

  function formatSales (n: Int): String {
    return Dec.toIso(n, 0);
  }

  // Control -------------------------------------------------------------------

  function changeParam (p: Js) {
    Cts.client.ssend([
      "module" => Js.ws("fleas"),
      "source" => Js.ws("ranges"),
      "rq" => Js.ws("changeParam"),
      "modelId" => Js.ws(md.id),
      "param" => p
    ], rp -> {
      mk(wg, md);
    });
  }

  function goIn (n: Float) {
    final p = switch (param.length) {
      case 1: [
        Js.wi(Std.int(n * 100)),
        Js.wi(Std.int(n * 1000) % 10)
      ];
      case 2: [
        Js.wi(Std.int(n * 100)),
        Js.wi(Std.int(n * 1000) % 10),
        Js.wi(Std.int(n * 10000) % 10)
      ];
      case 3: [
        Js.wi(Std.int(n * 100)),
        Js.wi(Std.int(n * 1000) % 10),
        Js.wi(Std.int(n * 10000) % 10),
        Js.wi(Std.int(n * 100000) % 10)
      ];
      default: [Js.wi(Std.int(n * 100))];
    }
    changeParam(Js.wa(p));
  }

  // Static --------------------------------------------------------------------

  public static function mk (wg: Domo, md: Fmodel): Void {
    if (md.parNames.length != 1) {
      wg
        .removeAll()
        .add(Q("table")
          .att("align", "center")
          .add(Q("tr")
            .add(Q("td")
              .klass("frame")
              .text(
                  _("Option only available for models with only one parameter")
                ))))
      ;
      return;
    }
    Cts.client.send([
      "module" => Js.ws("fleas"),
      "source" => Js.ws("ranges"),
      "rq" => Js.ws("idata"),
      "modelId" => Js.ws(md.id)
    ], rp -> {
      final param = rp["param"].ra().map(e -> e.ri());
      final values = rp["values"].ra().map(e -> Entry.fromJs(e));
      new Ranges(wg, md, param, values);
    });

  }

}

// Ranges entry
private class Entry {
  public final param: Float;
  public final eval: Float;
  public final sales: Int;

  public function new (param: Float, eval: Float, sales: Int) {
    this.param = param;
    this.eval = eval;
    this.sales = sales;
  }

  public static function fromJs (js: Js): Entry {
    final a = js.ra();
    return new Entry(a[0].rf(), a[1].rf(), a[2].ri());
  }
}

