// Copyright 15-Jan-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pgs;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Js;
import dm.It;
import data.Model;
import I18n._;

/// Description page.
class DescriptionPg {
  final wg: Domo;
  final model: Model;

  final formatter: Array<Float -> String>;

  function new (wg: Domo, model: Model) {
    this.wg = wg;
    this.model = model;

    formatter = model.paramEnvIncs.map(i -> Fns.paramFormatter(i));
  }

  // View ----------------------------------------------------------------------

  public function show (): Void {
    wg
      .removeAll()
      .add(Q("div")
        .klass("head")
        .text(model.name))
      .add(Q("div").klass("separator"))
      .add(Q("table")
        .att("align", "center")
        .klass("flat")
        .add(Q("tr")
          .add(Q("td")
            .klass("rhead")
            .text(_("Id") + ":"))
          .add(Q("td")
            .klass("rframe")
            .text(model.id)))
        .add(Q("tr")
          .add(Q("td")
            .klass("rhead")
            .text(_("Name") + ":"))
          .add(Q("td")
            .klass("rframe")
            .text(model.name))))
      .add(Q("div").klass("separator"))
      .add(Q("table")
        .att("align", "center")
        .klass("flat")
        .add(Q("tr")
          .add(Q("td"))
          .adds(model.paramNames.map(n -> Q("td")
              .klass("rhead")
              .text(n)
            )))
        .add(Q("tr")
          .add(Q("td")
            .klass("rhead")
            .text(_("Base") + ":"))
          .adds(It.range(model.paramBases.length).map(i -> Q("td")
              .klass("rframe")
              .text(formatter[i](model.paramBases[i]))
            )))
        .add(Q("tr")
          .add(Q("td")
            .klass("rhead")
            .text(_("Base Increment") + ":"))
          .adds(It.range(model.paramBaseIncs.length).map(i -> Q("td")
              .klass("rframe")
              .text(formatter[i](model.paramBaseIncs[i]))
            )))
        .add(Q("tr")
          .add(Q("td")
            .klass("rhead")
            .text(_("Environment Increment") + ":"))
          .adds(It.range(model.paramEnvIncs.length).map(i -> Q("td")
              .klass("rframe")
              .text(formatter[i](model.paramEnvIncs[i]))
            ))))



      .add(Q("div").klass("separator"))
      .add(Q("div")
        .klass("frame")
        .html(model.doc))
    ;
  }

  // Static --------------------------------------------------------------------

  public static function mk (wg: Domo, modelId: String): Void {
    Cts.client.send([
      "source" => Js.ws("Description"),
      "rq" => Js.ws("idata"),
      "modelId" => Js.ws(modelId)
    ], rp -> {
      final model = Model.fromJs(rp["model"]);
      new DescriptionPg(wg, model).show();
    });
  }
}
