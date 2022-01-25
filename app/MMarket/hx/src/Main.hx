// Copyright 15-Jan-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

using StringTools;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Js;
import dm.Menu;
import dm.Vmenu;
import dm.Store;
import I18n._;
import I18n._args;

/// Application entry.
class Main {
  final wg: Domo;
  final lang: String;
  final mainModelId: String;
  final modelIds: Array<String>;

  function new (wg: Domo, mainModelId: String, modelIds: Array<String>) {
    this.wg = wg;
    this.mainModelId = mainModelId;
    modelIds.sort((e1, e2) -> e1 > e2 ? 1 : -1);
    this.modelIds = modelIds;
    lang = switch (Store.get(Cts.langKey)) {
      case Some(l): l;
      case None: "es";
    }
    if (lang == "es") I18n.es();
    else I18n.en();
  }

  // View ----------------------------------------------------------------------

  public function show (): Void {
    final defaultPage = "description";
    var pg = defaultPage;
    var model = mainModelId;
    final url = Ui.url();
    if (url.exists("1")) {
      pg = url.get("1").trim();
      final md = url.get("0").trim();
      if (modelIds.contains(md)) model = md;
    } else if (url.exists("0")) {
      pg = url.get("0").trim();
    }
    switch (pg) {
      case "description", "results", "hot", "charts", "settings": {};
      default: pg = defaultPage;
    }

    final lopts = [
      Menu.tlink("description", _("Description"), model),
      Menu.separator2(),
      Menu.tlink("results", _("Results"), model),
      Menu.separator(),
      Menu.tlink("hot", _("Hot Map"), model),
      Menu.separator(),
      Menu.tlink("charts", _("Charts"), model),
    ];
    final ropts = [
      Menu.tlink("settings", _("Settings")),
      Menu.separator(),
      Menu.close(close)
    ];
    final menu = new Menu(lopts, ropts, pg);

    final body = Q("div");

    final body2 = Q("div");
    if (pg != "settings") {
      final vopts = [
        Vmenu.title(_("Models")),
        Vmenu.separator()
      ];
      for (id in modelIds) {
        vopts.push(Vmenu.option(id, id, () -> changeModel(pg, id)));
      }
      final vmenu = new Vmenu(vopts, model);

      body
        .removeAll()
        .add(Q("table")
          .klass("main")
          .add(Q("tr")
            .add(Q("td")
              .style("width: 5px; vertical-align: top")
              .add(vmenu.wg))
            .add(Q("td")
              .style("vertical-align: top")
              .add(body2))))
      ;
    }

    switch (pg) {
      case "results": pgs.ResultsPg.mk(body2, model);
      case "hot": pgs.HotPg.mk(body2, model);
      case "charts": new pgs.Charts(body2, model).show();
      case "settings": new pgs.Settings(body, lang).show();
      default: pgs.DescriptionPg.mk(body2, model);
    }

    wg
      .removeAll()
      .add(menu.wg)
      .add(body)
    ;
  }

  // Control -------------------------------------------------------------------

  function close (): Void {
    if (!Ui.confirm(_("Application exit?"))) {
      return;
    }
    Cts.client.send([
      "source" => Js.ws("Main"),
      "rq" => Js.ws("close"),
      "sessionId" => Js.ws(Cts.client.sessionId())
    ], rp -> {
      new pgs.MsgPg(
        Q("@body"), _args(_("Logout-message"), [Cts.appName]), false
      ).show();
    });
  }

  function changeModel (pg: String, model: String): Void {
    js.Browser.location.assign("?" + model + "&" + pg);
  }

  // Static --------------------------------------------------------------------

  static function mk (wg: Domo, fn: () -> Void): Void {
    Cts.client.connect(ok -> {
      if (ok) {
        Cts.client.send([
          "source" => Js.ws("Main"),
          "rq" => Js.ws("idata")
        ], rp -> {
          final mainModelId = rp["mainModelId"].rs();
          final modelIds = rp["modelIds"].ra().map(e -> e.rs());
          new Main(wg, mainModelId, modelIds).show();
          fn();
        });
      } else {
        new pgs.Authentication(wg, Cts.appName, () -> mk(wg, fn));
        fn();
      }
    });
  }

  /// Application entry.
  static public function main (): Void {
    var wg = Q("div");
    mk(wg, () -> {
      Q("@body")
        .removeAll()
        .add(wg)
        .add(Cts.foot)
        .add(Ui.upTop("up"))
      ;
      var fc = Q("#autofocus");
      if (fc.e != null) fc.e.focus();
    });
  }

}
