// Copyright 01-Jan-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pgs.main;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Js;
import dm.It;
import dm.Dec;
import dm.Log;
import data.ParamsEval;
import I18n._;
import I18n._args;

/// Home page.
class Home {
  final wg: Domo;
  final iparamsEvals: Array<ParamsEval>;
  final models: Array<String>;
  final paramsEvals: Array<ParamsEval>;

  function new (
    wg: Domo, iparamsEvals: Array<ParamsEval>, models: Array<String>,
    paramsEvals: Array<ParamsEval>
  ) {
    this.wg = wg;
    this.models = models;
    this.paramsEvals = paramsEvals;
    this.iparamsEvals = iparamsEvals;
  }

  // View ----------------------------------------------------------------------

  public function show (): Void {
    final logDiv = Q("div");

    function load (fn: Array<LogRow> -> Void) {
      Global.client.send([
        "prg" => Js.ws("MMagazine"),
        "module" => Js.ws("Main"),
        "source" => Js.ws("Home"),
        "rq" => Js.ws("getLog")
      ], rp -> {
        final log = rp["log"].ra();
        fn(log.map(js -> LogRow.fromJs(js)));
      });
    }

    function reset (fn: () -> Void) {
      Global.client.ssend([
        "prg" => Js.ws("MMagazine"),
        "module" => Js.ws("Main"),
        "source" => Js.ws("Home"),
        "rq" => Js.ws("resetLog")
      ], rp -> {
        fn();
      });
    }

    Log.mk(logDiv, load, reset, _, true);

    wg
      .removeAll()
      .adds(It.range(2).map(i -> Q("div").klass("separator")).to())
      .add(Q("table")
        .klass("frame")
        .att("align", "center")
        .add(Q("tr")
          .add(Q("td")
            .add(Q("span")
              .html(
                "Los mejores resultados de los modelos usados por los inversores son:<br>" +
                "<pre>" +
                It.range(models.length).map(i ->
                  "Inv-" + i + " (" +
                  Std.string(iparamsEvals[i].params) +
                  ": " + Std.string(iparamsEvals[i].eval / 100) +
                  ") -> " +
                  models[i] + Std.string(paramsEvals[i].params) +
                  ": " + Std.string(paramsEvals[i].eval / 100)).to().join("\n") +
                "</pre>"
              )))))
      .add(logDiv)
    ;
  }

  // Static --------------------------------------------------------------------

  public static function mk (wg: Domo): Void {
    Global.client.send([
      "prg" => Js.ws("MMagazine"),
      "module" => Js.ws("Main"),
      "source" => Js.ws("Home"),
      "rq" => Js.ws("idata"),
    ], rp -> {
      final iparamsEvals = rp["iparamsEvals"].ra()
        .map(e -> ParamsEval.fromJs(e));
      final models = rp["models"].ra().map(e -> e.rs());
      final paramsEvals = rp["paramsEvals"].ra()
        .map(e -> ParamsEval.fromJs(e));
      new Home(wg, iparamsEvals, models, paramsEvals).show();
    });
  }
}
