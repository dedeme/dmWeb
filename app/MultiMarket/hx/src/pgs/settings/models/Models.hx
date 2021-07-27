// Copyright 18-Sep-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pgs.settings.models;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.It;
import dm.Js;
import dm.B64;
import dm.Menu;
import data.Cts;
import data.flea.Fmodel;
import data.flea.Eflea;
import data.Investor;
import wgs.Params;
import I18n._;
import I18n._args;

/// Models management.
class Models {
  var wg: Domo;
  var investors: Int;
  var investorIx: Int;
  var models: Array<Fmodel>;
  var investor: Investor;
  var sinvestor: String;
  final eflea: Eflea;

  final editorDiv = Q("div");
  final paramsDiv = Q("div");

  /// Constructor.
  ///   wg: Container.
  ///   investors: Total managders number.
  ///   investorIx: Current index of manager.
  ///   models: Flea models.
  ///   investor: Current manager.
  function new (
    wg: Domo, investors: Int, investorIx: Int,
    models: Array<Fmodel>, investor: Investor, eflea: Eflea
  ) {
    this.wg = wg;
    this.investors = investors;
    this.investorIx = investorIx;
    sinvestor = '${_("Inv")}-${investorIx}';
    models.sort((m1, m2) -> m1.id > m2.id ? 1 : -1);
    this.models = models;
    this.investor = investor;
    this.eflea = eflea;

    view();
  }

  // View ----------------------------------------------------------------------

  function paramsView (
    nick: String, model: Fmodel, ?value: Float
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


    final params = value == null
      ? new Params(
        model.parName, "ps", "accept"
      )
      : new Params(
        model.parName, "ps", "accept", value
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
                investor.base.param
              );
            }).klass("link")
              .text(_("Default Model")))))
        .add(Q("tr")
          .add(Q("td")
            .att("colspan", 2)
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

    this.paramsView(nick, v.model, v.param);

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
    final menu = new Menu(lopts, [], sinvestor);

    wg
      .removeAll()
      .add(menu.wg)
      .add(editorDiv)
      .add(Q("div")
        .klass("head")
        .html(
            _("Default Model") + "<br><small>[" +
            _("Update on first of") + " " + Cts.changeMonths[investorIx] +
            "]</small>"
          ))
      .add(Q("table")
        .klass("white")
        .att("align", "center")
        .add(Q("tr")
          .add(Q("td")
            .klass("header")
            .html(_("Model")))
          .add(Q("td")
            .klass("header")
            .html(base.model.parName))
          .add(Q("td")
            .klass("header")))
        .add(Q("tr")
          .add(Q("td")
            .klass("border")
            .add(Ui.link(e -> edit(""))
              .klass("link")
              .html(base.model.id)))
          .add(Q("td")
            .klass("number")
            .text(Cts.nformat(base.param, Cts.paramDecs)))
          .add(Q("td")
            .add(Q("a")
              .att(
                "href",
                '?fleas&${base.model.id}&charts&true' +
                  '&${B64.encode(eflea.toJs().to())}'
              )
              .html("&nbsp;" + _("Charts") + "&nbsp;")))))
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
          .add(Q("td")
            .klass("header")
            .html("Par"))
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
            .add(Q("td")
              .klass("number")
              .text(Cts.nformat(v.param, Cts.paramDecs))
            )
            .add(Q("td")
              .klass("border")
              .add(Ui.img(v.eqParam(base) ? "blank" : "warning")))
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

  function accept (nick: String, model: Fmodel, param: Float): Void {
    Cts.client.ssend([
      "module" => Js.ws("settings"),
      "source" => Js.ws("models"),
      "rq" => Js.ws("update"),
      "investorIx" => Js.wi(investorIx),
      "nickName" => Js.ws(nick),
      "modelId" => Js.ws(model.id),
      "param" => Js.wf(param)
    ], rp -> {
      Models.mk(wg, investorIx);
    });
  }

  // Static --------------------------------------------------------------------

  /// Constructor.
  ///   wg       : Container.
  ///   managerIx: Manager index.
  public static function mk (wg: Domo, investorIx: Int) {
    Cts.client.ssend([
      "module" => Js.ws("settings"),
      "source" => Js.ws("models"),
      "rq" => Js.ws("idata"),
      "investorIx" => Js.wi(investorIx)
    ], rp -> {
      final models = rp["models"].rArray(e -> Fmodel.fromJs(e));
      final investor = Investor.fromJs(rp["investor"]);
      final investors = rp["investors"].ri();
      final eflea = Eflea.fromJs(rp["eflea"]);

      new Models(wg, investors, investorIx, models, investor, eflea);
    });
  }

}
