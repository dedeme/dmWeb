// Copyright 15-Jun-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pgs.settings.investorsPg;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.It;
import dm.Js;
import dm.Menu;
import dm.ModalBox;
import data.Model;
import data.Investor;
import wgs.Params;
import wgs.Msg;
import I18n._;
import I18n._args;

/// Investors management.
class InvestorsPg {
  var wg: Domo;
  var waitBox: ModalBox;
  var investors: Int;
  var investorIx: Int;
  var models: Array<Model>;
  var investor: Investor;
  var sinvestor: String;

  final editorDiv = Q("div");
  final paramsDiv = Q("div");

  /// Constructor.
  ///   wg: Container.
  ///   investors: Total investors number.
  ///   investorIx: Current investor index.
  ///   models: Flea models.
  ///   investor: Current investor.
  function new (
    wg: Domo, investors: Int, investorIx: Int,
    models: Array<Model>, investor: Investor
  ) {
    this.wg = wg;
    this.investors = investors;
    this.investorIx = investorIx;
    sinvestor = '${_("Inv")}-${investorIx}';
    models.sort((m1, m2) -> m1.id > m2.id ? 1 : -1);
    this.models = models;
    this.investor = investor;
    waitBox = new ModalBox(Ui.img("wait2.gif"), false);

    view();
  }

  // View ----------------------------------------------------------------------

  function paramsView (
    nick: String, model: Model, ?values: Array<Float>
  ): Void {
    final sel = Q("select");
    for (m in models) {
      final op = Q("option").text(m.id);
      if (m.id == model.id) {
        op.att("selected", true);
      }
      cast(sel.e, js.html.SelectElement).add(cast(op.e, js.html.OptionElement));
    };
    sel.on(
      CHANGE,
      e -> paramsView(
        nick, models[cast(sel.e, js.html.SelectElement).selectedIndex]
      )
    );


    final params = values == null
      ? new Params(
        model.paramNames, model.paramMaxs, model.paramMins, "ps", "accept"
      )
      : new Params(
        model.paramNames, model.paramMaxs, model.paramMins, "ps", "accept", values
      )
    ;

    paramsDiv
      .removeAll()
      .add(Q("table")
        .add(Q("tr")
          .add(Q("td")
            .add(sel))
          .add(Q("td")
            .style("text-align:right")
            .add(Ui.link(e -> {
              paramsView(
                nick,
                investor.base.model,
                investor.base.params
              );
            }).klass("link")
              .text(_("Default Model")))))
        .add(Q("tr")
          .add(Q("td")
            .att("colspan", 2)
            .att("align", "center")
            .add(params.wg)))
        .add(Q("tr")
          .add(Q("td")
            .att("colspan", 2)
            .style("text-align:center")
            .add(Q("button")
              .text(_("Cancel"))
              .on(CLICK, e -> cancel()))
            .add(Q("span").text(" "))
            .add(Q("button")
              .att("id", "accept")
              .text(_("Accept"))
              .on(CLICK, e -> {
                accept(nick, model, params.value);
              })))))
    ;
  }

  function editorView (nick: String): Void {
    final v = nick == "" ? investor.base : investor.nicks[nick];

    this.paramsView(nick, v.model, v.params);

    editorDiv
      .removeAll()
      .add(Q("div")
        .klass("head")
        .text(nick == "" ? _("Default Model") : _args(_("%0 Model"), [nick])))
      .add(Q("table")
        .att("align", "center")
        .add(Q("tr")
          .add(Q("td")
            .att("colspan", 2)
            .add(paramsDiv))))
      .add(Q("hr"))
    ;
  }

  function view () {
    final base = investor.base;
    final nicks = investor.nicks;
    final knicks = It.fromMap(nicks).map(tp -> tp.e1).to();
    knicks.sort((k1, k2) -> k1 > k2 ? 1 : -1);

    final lopts = [];
    for (i in 0...investors) {
      final lb = '${_("Inv")}-${i}';
      if (i > 0) lopts.push(Menu.separator());
      lopts.push(Menu.toption(lb, lb, () -> selManager(i)));
    }
    final ropts = [Menu.toption("update", _("Update"), update)];
    final menu = new Menu(lopts, ropts, sinvestor);

    final nPar = It.fromMap(nicks).map(tp -> tp.e2).reduce(
      0, (r, v) -> v.params.length > r ? v.params.length : r
    );
    wg
      .removeAll()
      .add(waitBox.wg)
      .add(menu.wg)
      .add(editorDiv)
      .add(Q("div")
        .klass("head")
        .html(_("Default Model")))
      .add(Q("table")
        .klass("white")
        .att("align", "center")
        .add(Q("tr")
          .add(Q("td")
            .klass("header")
            .html(_("Model")))
          .adds(base.model.paramNames.map(n ->
            Q("td")
              .klass("header")
              .html(n))))
        .add(Q("tr")
          .add(Q("td")
            .klass("border")
            .add(Ui.link(e -> edit(""))
              .klass("link")
              .html(base.model.id)))
          .adds(It.range(base.params.length).to().map(ix ->
            Q("td")
              .klass("number")
              .text(Cts.nformat(base.params[ix], base.model.paramDecs[ix]))))))
      .add(Q("div")
        .klass("head")
        .html(_("Nick models")))
      .add(Q("table")
        .klass("white")
        .att("align", "center")
        .add(Q("tr")
          .add(Q("td")
            .klass("header")
            .html(_("Nick")))
          .add(Q("td")
            .klass("header")
            .html(_("Model")))
          .adds(It.range(nPar).to().map(n ->
            Q("td")
              .klass("header")
              .html("P. " + Std.string(n + 1))))
          .add(Q("td")
            .klass("header")
            .text("·")))
        .adds(knicks.map(k -> {
          final v = nicks[k];
          return Q("tr")
            .add(Q("td")
              .klass("border")
              .text(k))
            .add(Q("td")
              .klass("border")
              .add(Ui.link(e -> edit(k))
                .klass("link")
                .text(v.model.id)))
            .adds(It.range(nPar).to().map(ix ->
              ix >= v.params.length
                ? Q("td")
                  .klass("border")
                : Q("td")
                  .klass("number")
                  .text(Cts.nformat(v.params[ix], v.model.paramDecs[ix]))
            ))
            .add(Q("td")
              .klass("border")
              .add(Ui.img(v.eqParams(base) ? "blank" : "warning")))
          ;
        })))

    ;
  }

  // Controls ------------------------------------------------------------------

  function selManager (managerIx: Int): Void {
    mk(wg, managerIx);
  }

  function edit (nick: String): Void {
    editorView(nick);
    js.Browser.window.scroll(0, 0);
  }

  function cancel (): Void {
    editorDiv.removeAll();
  }

  function update () {
    waitBox.show(true);
    Cts.client.ssend([
      "module" => Js.ws("settings"),
      "source" => Js.ws("investorsPg"),
      "rq" => Js.ws("updateAll"),
    ], rp -> {
      waitBox.show(false);
      if (!rp["ok"].rb()) {
        Msg.error(
          Cts.failMsg,
          () -> {}
        );
        return;
      }
      InvestorsPg.mk(wg, investorIx);
    });
  }

  function accept (nick: String, model: Model, params: Array<Float>): Void {
    if (nick == "") waitBox.show(true);
    Cts.client.ssend([
      "module" => Js.ws("settings"),
      "source" => Js.ws("investorsPg"),
      "rq" => Js.ws("update"),
      "investorIx" => Js.wi(investorIx),
      "nickName" => Js.ws(nick),
      "modelId" => Js.ws(model.id),
      "params" => Js.wArray(params, e -> Js.wf(e))
    ], rp -> {
      waitBox.show(false);
      if (!rp["ok"].rb()) {
        Msg.error(
          Cts.failMsg,
          () -> {}
        );
        return;
      }
      InvestorsPg.mk(wg, investorIx);
    });
  }

  // Static --------------------------------------------------------------------

  /// Constructor.
  ///   wg       : Container.
  ///   investorIx: Investor index.
  public static function mk (wg: Domo, investorIx: Int) {
    Cts.client.ssend([
      "module" => Js.ws("settings"),
      "source" => Js.ws("investorsPg"),
      "rq" => Js.ws("idata"),
      "investorIx" => Js.wi(investorIx)
    ], rp -> {
      if (!rp["ok"].rb()) {
        Msg.error(
          Cts.failMsg,
          () -> {}
        );
        return;
      }

      final models = rp["models"].rArray(e -> Model.fromJs(e));
      final investor = Investor.fromJs(rp["investor"]);
      final investors = rp["investors"].ri();

      new InvestorsPg(wg, investors, investorIx, models, investor);
    });
  }

}
