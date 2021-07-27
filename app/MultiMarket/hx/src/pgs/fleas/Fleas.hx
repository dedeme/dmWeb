// Copyright 09-Sep-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pgs.fleas;

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
import wgs.Table; // include Col
import data.flea.Fmodel;
import data.flea.Eflea;
import data.flea.EfleaDate;
import data.flea.Frank;
import pgs.fleas.overview.Overview;
import pgs.fleas.ftests.Ftests;
import pgs.fleas.charts.Charts;
import pgs.fleas.ranges.Ranges;
import pgs.fleas.ranges.Rankings;
import I18n._;

/// Fleas main page.
class Fleas {
  var wg: Domo;
  var dmenu: Dmenu;
  var lcPath: Array<String>;
  var models: Array<Fmodel>;
  var mSel: Fmodel;
  var target: String;

  // Constructor.
  //    wg    : Container.
  //    dmenu : Double menu
  //    lcPath: Location path
  //    models: Flea models.
  function new (
    wg: Domo, dmenu: Dmenu, lcPath: Array<String>, models: Array<Fmodel>
  ) {
    this.wg = wg;
    this.dmenu = dmenu;
    this.models = models;

    if (lcPath.length == 0) lcPath.push(models[0].id);
    switch (It.from(models).find(e -> e.id == lcPath[0])) {
      case None:
        mSel = models[0];
        lcPath.push(mSel.id);
      case Some(md):
        mSel = md;
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
    It.from(models).eachIx((e, ix) ->
      ops.push(Vmenu.option(
        e.id,
        e.id,
        () -> onModelSelection(ix)
      ))
    );
    ops.unshift(Vmenu.separator());
    ops.unshift(Vmenu.title(_("Models")));
    final vmenu = new Vmenu(ops, mSel.id);

    final link = "fleas&" + lcPath[0];
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
      new Ftests(wg, mSel);
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

  function onModelSelection (id: Int) {
    mSel = models[id];
    js.Browser.location.assign("?fleas&" + mSel.id + "&" + lcPath[1]);
  }

  function charts (wg: Domo) {
    final modelId = lcPath[0];
    Cts.client.send([
      "module" => Js.ws("fleas"),
      "source" => Js.ws("fleas"),
      "rq" => Js.ws("best"),
      "modelId" => Js.ws(mSel.id)
    ], rp -> {
      final bestJs = rp["best"];
      final parName = rp["parName"].rs();

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
      var eflea = Eflea.fromJs(bestJs);

      final args = It.from(lcPath).drop(2).to();
      if (args.length == 2) {
        isAssets = Js.from(args[0]).rb();
        eflea = Eflea.fromJs(Js.from(B64.decode(args[1])));
      }

      new Charts(wg, modelId, isAssets, parName, eflea);
    });
  }

  // 'isSummary' is 'true' if chart is from assets summary.
  function chart (isSummary: Bool, eflea: Eflea): String {
    return '?fleas&${lcPath[0]}&charts' +
           '&${isSummary}&${B64.encode(eflea.toJs().to())}'
    ;
  }

  // Static --------------------------------------------------------------------

  /// Constructor.
  ///   wg    : Container.
  ///   dmenu : Double menu
  ///   lcPath: Location path
  public static function mk (wg: Domo, dmenu: Dmenu, lcPath: Array<String>) {
    Cts.client.send([
      "module" => Js.ws("fleas"),
      "source" => Js.ws("fleas"),
      "rq" => Js.ws("models")
    ], rp -> {
      final models = rp["models"].ra().map(e -> Fmodel.fromJs(e));
      new Fleas(wg, dmenu, lcPath, models);
    });
  }

  static function mkHeaderBase (): Array<Col> {
    return [
      new Col(_("Nº"), Col.COUNTER, 0, false, false),
      new Col(_("Id"), Col.STRING, -1, true, false),
      new Col(_("Assets"), Col.NUMBER, 2, false, false),
      new Col(_("Pf. Avg"), Col.NUMBER, 4, false, false),
      new Col(
        _("Pf. Var"), Col.NUMBER, 4,
        false, false
      ),
      new Col(
        _("Eval."), Col.NUMBER, 2,
        false, false
      ),
      new Col(
        _("Buys"), Col.NUMBER, 0,
        false, false
      ),
      new Col(
        _("Sells"), Col.NUMBER, 0,
        false, false
      )
    ];
  }
}
