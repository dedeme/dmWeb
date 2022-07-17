// Copyright 15-Jun-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pgs.models;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Ui;
import dm.Js;
import dm.It;
import dm.Opt;
import dm.Menu;
import dm.Vmenu;
import wgs.Dmenu;
import wgs.Msg;
import data.Model;
import data.Order;
import data.AssetsRs;
import pgs.models.overview.Overview;
import pgs.models.tests.Tests;
import pgs.models.charts.Charts;
import I18n._;
import I18n._args;

/// Models main page.
class Models {
  var wg: Domo;
  var dmenu: Dmenu;
  var lcPath: Array<String>;
  var models: Array<Model>;
  var mSel: Model;
  var target: String;

  // Constructor.
  //    wg    : Container.
  //    dmenu : Double menu.
  //    lcPath: Location path.
  //    models: Model list.
  function new (
    wg: Domo, dmenu: Dmenu, lcPath: Array<String>, models: Array<Model>
  ) {
    models.sort((m1, m2) -> return m1.id > m2.id ? 1: m1.id < m2.id ? -1 : 0);
    this.wg = wg;
    this.dmenu = dmenu;
    this.models = models;

    if (lcPath.length == 0) {
      mSel = models[0];
      lcPath.push(mSel.id);
    } else {
      switch (It.from(models).find(md -> md.id == lcPath[0])) {
        case None:
          mSel = models[0];
          lcPath = [mSel.id];
        case Some(md):
          mSel = md;
      }
    }

    if (lcPath.length == 1) lcPath.push("overview");
    this.lcPath = lcPath;

    switch (lcPath[1]) {
    case "overview" | "charts" | "tests":
      target = lcPath[1];
    default:
      target = "overview";
    }

    view();
  }

  // View ----------------------------------------------------------------------

  function view () {
    final wg = Q("div");

    final ops: Array<VmenuEntry> = [];
    ops.push(Vmenu.title(_("Models")));
    ops.push(Vmenu.separator());
    It.from(models).each(md ->
      ops.push(Vmenu.option(
        md.id,
        md.id,
        () -> onModelSelection(md)
      ))
    );
    final vmenu = new Vmenu(ops, mSel.id);

    final link = "models&" + lcPath[0];
    final lopts = [
      dmenu.hiddingButton(),
      Menu.separator2(),
      Menu.tlink("overview", _("Overview"), link),
      Menu.separator(),
      Menu.tlink("charts", _("Charts"), link),
      Menu.separator(),
      Menu.tlink("tests", _("Tests"), link)
    ];

    final ropts = [];
    dmenu.setDownMenu(new Menu(lopts, ropts, target));

    switch (target) {
    case "overview":
      new Overview(wg, mSel);
    case "charts":
      charts(wg);
    case "tests":
      new Tests(wg, mSel);
    default:
      new Overview(wg, mSel);
    }

    this.wg
      .removeAll()
      .add(Q("table")
        .klass("main")
        .add(Q("tr")
          .add(Q("td")
            .style("width:5px;vertical-align:top")
            .add(vmenu.wg))
          .add(Q("td").html("&nbsp;&nbsp;&nbsp;&nbsp;"))
          .add(Q("td")
            .style("width:100%;vertical-align:top;")
            .add(wg))))
    ;
  }

  // Control -------------------------------------------------------------------

  function onModelSelection (md: Model) {
    mSel = md;
    js.Browser.location.assign("?models&" + md.id + "&" + lcPath[1]);
  }

  function charts (wg: Domo) {
    final modelId = lcPath[0];
    final model = switch (It.from(models).find(m -> m.id == modelId)) {
      case Some(m): m;
      default: models[0];
    };
    var params = None;

    if (lcPath.length > 2) {
      try {
        final ps = Js.from(lcPath[2]).ra().map(e -> e.rf());
        var ok = true;
        if (ps.length != model.paramMaxs.length) {
          ok = false;
        } else {
          for (i in 0...model.paramMaxs.length) {
            if (ps[i] > model.paramMaxs[i] || ps[i] < model.paramMins[i])
              ok = false;
          }
          if (ok) {
            params = Some(ps);
          } else {
            trace("Bad parameters");
          }
        }
      } catch (e) {
        trace("Bad parameters");
      }
    }
    Cts.client.send([
      "module" => Js.ws("models"),
      "source" => Js.ws("models"),
      "modelId" => Js.ws(model.id),
      "params" => switch(params) {
        case Some(ps): Js.wa(ps.map(e -> Js.wf(e)));
        default: Js.wn();
      },
      "rq" => Js.ws("results")
    ], rp -> {
      if (!rp["ok"].rb()) {
        Msg.error(Cts.failMsg);
        return;
      }
      final params = rp["params"].ra().map(e -> e.rf());
      final dates = rp["dates"].ra().map(e -> e.rs());
      final assets = rp["assets"].ra().map(e -> e.rf());
      final withdrawals = rp["withdrawals"].ra().map(e -> e.rf());
      final results = AssetsRs.fromJs(rp["results"]);
      final nicks = rp["nicks"].ra().map(e -> e.rs());
      final lastCloses = rp["lastCloses"].ra().map(e -> e.rf());
      final orders = rp["orders"].ra().map(e -> Order.fromJs(e));

      new Charts(
        wg, model, params, dates, assets, withdrawals,
        results, nicks, orders, lastCloses
      );
    });
  }

  // Static --------------------------------------------------------------------

  /// Constructor.
  ///   wg    : Container.
  ///   dmenu : Double menu
  ///   lcPath: Location path
  public static function mk (wg: Domo, dmenu: Dmenu, lcPath: Array<String>) {
    Cts.client.send([
      "module" => Js.ws("models"),
      "source" => Js.ws("models"),
      "rq" => Js.ws("idata")
    ], rp -> {
      final models = rp["models"].ra().map(m -> Model.fromJs(m));
      new Models(wg, dmenu, lcPath, models);
    });
  }
}
