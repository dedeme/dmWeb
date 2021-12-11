// Copyright 14-Nov-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pgs.models;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Ui;
import dm.Js;
import dm.It;
import dm.B64;
import dm.Menu;
import dm.Vmenu;
import data.Cts;
import wgs.Dmenu;
import data.model.Model;
import pgs.models.overview.Overview;
import pgs.models.tests.Tests;
import pgs.models.charts.Charts;
import pgs.models.ranges.Ranges;
import pgs.models.ranges.Rankings;
import I18n._;

/// Models main page.
class Models {
  var wg: Domo;
  var dmenu: Dmenu;
  var lcPath: Array<String>;
  var qlevels: Int;
  var mSel: Int;
  var target: String;

  // Constructor.
  //    wg    : Container.
  //    dmenu : Double menu
  //    lcPath: Location path
  //    qlevels: Number of model qlevels.
  function new (
    wg: Domo, dmenu: Dmenu, lcPath: Array<String>, qlevels: Int
  ) {
    this.wg = wg;
    this.dmenu = dmenu;
    this.qlevels = qlevels;

    if (lcPath.length == 0) lcPath.push("0");
    switch (It.range(qlevels).find(ql -> "" + ql == lcPath[0])) {
      case None:
        mSel = 0;
        lcPath.push("" + mSel);
      case Some(ql):
        mSel = ql;
    }

    if (lcPath.length == 1) lcPath.push("overview");
    this.lcPath = lcPath;

    switch (lcPath[1]) {
    case "overview" | "charts" |
      "ranges" | "ranking" | "tests":
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
    It.range(qlevels).each((ql) ->
      ops.push(Vmenu.option(
        "" + ql,
        _("Qlevel") + " " + ql,
        () -> onModelSelection(ql)
      ))
    );
    final vmenu = new Vmenu(ops, "" + mSel);

    final link = "models&" + lcPath[0];
    final lopts = [
      dmenu.hiddingButton(),
      Menu.separator2(),
      Menu.tlink("overview", _("Overview"), link),
      Menu.separator2(),
      Menu.tlink("charts", _("Charts"), link),
      Menu.separator2(),
      Menu.tlink("ranges", _("Ranges"), link),
      Menu.separator(),
      Menu.tlink("ranking", _("Ranking"), link),
      Menu.separator2(),
      Menu.tlink("tests", _("Tests"), link)
    ];

    final ropts = [];
    dmenu.setDownMenu(new Menu(lopts, ropts, target));

    switch (target) {
    case "overview":
      new Overview(wg, mSel);
    case "charts":
      charts(wg);
    case "ranges":
      Ranges.mk(wg, mSel);
    case "ranking":
      Rankings.mk(wg, mSel, lcPath[0]);
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

  function onModelSelection (ql: Int) {
    mSel = ql;
    js.Browser.location.assign("?models&" + mSel + "&" + lcPath[1]);
  }

  function charts (wg: Domo) {
    final modelId = lcPath[0];
    Cts.client.send([
      "module" => Js.ws("models"),
      "source" => Js.ws("models"),
      "rq" => Js.ws("best"),
      "qlevel" => Js.wi(mSel)
    ], rp -> {
      final bestJs = rp["best"];

      if (bestJs.isNull()) {
        wg
          .removeAll()
          .add(Q("table")
            .att("align", "center")
            .add(Q("tr")
              .add(Q("td")
                .klass("frame")
                .html(_("Without data")))));
        return;
      }

      var isAssets = true;
      var model = Model.fromJs(bestJs);

      final args = It.from(lcPath).drop(2).to();
      if (args.length == 2) {
        isAssets = Js.from(args[0]).rb();
        model = Model.fromJs(Js.from(B64.decode(args[1])));
      }

      new Charts(wg, isAssets, model);
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
      final qlevels = rp["qlevels"].ri();
      new Models(wg, dmenu, lcPath, qlevels);
    });
  }
}
