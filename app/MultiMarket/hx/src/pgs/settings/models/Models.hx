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
import data.Manager;
import wgs.Params;
import I18n._;
import I18n._args;

/// Models management.
class Models {
  var wg: Domo;
  var managers: Int;
  var managerIx: Int;
  var models: Array<Fmodel>;
  var manager: Manager;
  var smanager: String;
  final eflea: Eflea;

  final editorDiv = Q("div");
  final paramsDiv = Q("div");

  /// Constructor.
  ///   wg: Container.
  ///   managers: Total managders number.
  ///   managerIx: Current index of manager.
  ///   models: Flea models.
  ///   manager: Current manager.
  function new (
    wg: Domo, managers: Int, managerIx: Int,
    models: Array<Fmodel>, manager: Manager, eflea: Eflea
  ) {
    this.wg = wg;
    this.managers = managers;
    this.managerIx = managerIx;
    smanager = '${_("Inv")}-${managerIx}';
    models.sort((m1, m2) -> m1.id > m2.id ? 1 : -1);
    this.models = models;
    this.manager = manager;
    this.eflea = eflea;

    view();
  }

  // View ----------------------------------------------------------------------

  function paramsView (
    nick: String, model: Fmodel, ?values: Array<Float>
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
        model.parNames, model.parMaxs, model.parMins, "ps", "accept"
      )
      : new Params(
        model.parNames, model.parMaxs, model.parMins, "ps", "accept", values
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
                manager.base.model,
                manager.base.params
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
    final v = nick == "" ? manager.base : manager.nicks[nick];

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
    final base = manager.base;
    final nicks = manager.nicks;
    final knicks = It.fromMap(nicks).map(tp -> tp.e1).to();
    knicks.sort((k1, k2) -> k1 > k2 ? 1 : -1);

    final lopts = [];
    for (i in 0...managers) {
      final lb = '${_("Inv")}-${i}';
      if (i > 0) lopts.push(Menu.separator());
      lopts.push(Menu.toption(lb, lb, () -> selManager(i)));
    }
    final menu = new Menu(lopts, [], smanager);

    final nPar = It.fromMap(nicks).map(tp -> tp.e2).reduce(
      0, (r, v) -> v.params.length > r ? v.params.length : r
    );
    wg
      .removeAll()
      .add(menu.wg)
      .add(editorDiv)
      .add(Q("div")
        .klass("head")
        .html(
            _("Default Model") + "<br><small>[" +
            _("Update on first of") + " " + Cts.changeMonths[managerIx] +
            "]</small>"
          ))
      .add(Q("table")
        .klass("white")
        .att("align", "center")
        .add(Q("tr")
          .add(Q("td")
            .klass("header")
            .html(_("Model")))
          .adds(base.model.parNames.map(n ->
            Q("td")
              .klass("header")
              .html(n)))
          .add(Q("td")
            .klass("header")))
        .add(Q("tr")
          .add(Q("td")
            .klass("border")
            .add(Ui.link(e -> edit(""))
              .klass("link")
              .html(base.model.id)))
          .adds(It.range(base.params.length).to().map(ix ->
            Q("td")
              .klass("number")
              .text(Cts.nformat(base.params[ix], base.model.parDecs[ix]))))
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
                  .text(Cts.nformat(v.params[ix], v.model.parDecs[ix]))
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

  function accept (nick: String, model: Fmodel, params: Array<Float>): Void {
    Cts.client.ssend([
      "module" => Js.ws("settings"),
      "source" => Js.ws("models"),
      "rq" => Js.ws("update"),
      "managerIx" => Js.wi(managerIx),
      "nickName" => Js.ws(nick),
      "modelId" => Js.ws(model.id),
      "params" => Js.wArray(params, e -> Js.wf(e))
    ], rp -> {
      Models.mk(wg, managerIx);
    });
  }

  // Static --------------------------------------------------------------------

  /// Constructor.
  ///   wg       : Container.
  ///   managerIx: Manager index.
  public static function mk (wg: Domo, managerIx: Int) {
    Cts.client.ssend([
      "module" => Js.ws("settings"),
      "source" => Js.ws("models"),
      "rq" => Js.ws("idata"),
      "managerIx" => Js.wi(managerIx)
    ], rp -> {
      final models = rp["models"].rArray(e -> Fmodel.fromJs(e));
      final manager = Manager.fromJs(rp["manager"]);
      final managers = rp["managers"].ri();
      final eflea = Eflea.fromJs(rp["eflea"]);

      new Models(wg, managers, managerIx, models, manager, eflea);
    });
  }

}
