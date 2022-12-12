// Copyright 08-Dic-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pgs;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Js;
import dm.It;
import dm.Menu;
import data.MdStats;
import data.Flea;
import I18n._;

/// Models page
class ModelsPg {
  final wg: Domo;
  final modelIds: Array<String>;
  final mdStats: MdStats;

  var menuSel = "ranking";

  function new (wg: Domo, modelIds: Array<String>, mdStats: MdStats) {
    this.wg = wg;
    this.modelIds = modelIds;
    this.mdStats = mdStats;
  }

  // View ----------------------------------------------------------------------

  public function show (): Void {
    final menuLeft = It.from(modelIds).reduce(
      [Menu.toption("ranking", _("Ranking"), ranking)],
      (r, mId) -> {
        r.push(Menu.separator());
        r.push(Menu.toption(mId, mId, () -> statistics(mId)));
        return r;
      }
    );
    menuLeft[1] = Menu.separator2();

    final menu = new Menu(
      menuLeft,
      [],
      menuSel
    );

    wg
      .removeAll()
      .add(menu.wg)
    ;

    if (menuSel == "ranking") {
      showRanking();
    } else {
      showStatistics();
    }

  }

  function showRanking (): Void {
    wg
      .add(Q("table")
        .klass("border")
        .att("align", "center")
        .add(Q("tr")
          .add(Q("td")
            .klass("header")
            .text(_("Models")))
          .add(Q("td")
            .klass("header")
            .style("text-align: right")
            .text(_("Duplicates"))))
      .adds(mdStats.groupsRanking
        .map(gr -> Q("tr")
          .add(Q("td")
            .klass("border")
            .style("text-align: left;font-family:monospace;white-space:pre")
            .text(gr.formatIds()))
          .add(Q("td")
            .klass("border")
            .style("text-align: right")
            .text(Fns.nFormat(gr.duplicates, 0)))
        )))

    ;
  }

  function showStatistics (): Void {
    function bestsWorstsWg (ofs: Array<OrderFlea>) {
      function mkRow (of: OrderFlea): Domo {
        final f = of.flea;
        function mkModelsWg (): Array<Domo> {
          return f.fmtModels().map(fmt ->
            Q("td")
              .klass("borderWhite")
              .style("text-align:left;font-family:monospace;white-space:pre")
              .text(fmt)
          );
        }

        return Q("tr")
          .add(Q("td")
            .klass("number")
            .text(Fns.nFormat(of.order + 1, 0)))
          .add(Q("td")
            .klass("border")
            .style("background:" + (f.isMale ? "#a0c0f0": "#f0c0a0"))
            .text(f.fmtId()))
          .add(Q("td")
            .klass("borderWhite")
            .text(f.fmtCycle()))
          .adds(mkModelsWg())
          .add(Q("td")
            .klass("number")
            .text(Fns.nFormat(f.assets, 2)))
        ;
      }
      final trs = [];
      It.from(ofs).each(of -> trs.push(mkRow(of)));

      return Q("table")
        .klass("border")
        .att("align", "center")
        .add(Q("tr")
          .add(Q("td")
            .klass("header")
            .style("text-align: right")
            .text(_("Pos.")))
          .add(Q("td")
            .klass("header")
            .style("text-align: left")
            .text(_("Id")))
          .add(Q("td")
            .klass("header")
            .style("text-align: left")
            .text(_("Cycle")))
          .add(Q("td")
            .klass("header")
            .att("colspan", 3)
            .style("text-align: left")
            .text(_("Models")))
          .add(Q("td")
            .klass("header")
            .style("text-align: right")
            .text(_("Assets"))))
        .adds(trs)
      ;
    }

    final mds = mdStats.models;
    final md = mds[It.from(mds).indexf(m -> m.modelId == menuSel)];
    wg
      .add(Q("table")
        .klass("border")
        .att("align", "center")
        .add(Q("tr")
          .add(Q("td")
            .klass("rlabel")
            .text(_("Fleas number")))
          .add(Q("td")
            .klass("number")
            .text(Fns.nFormat(md.nfleas, 0))))
        .add(Q("tr")
          .add(Q("td")
            .klass("rlabel")
            .text(_("Position average")))
          .add(Q("td")
            .klass("number")
            .text(Fns.nFormat(md.position, 0))))
        .add(Q("tr")
          .add(Q("td")
            .klass("rlabel")
            .text(_("Assets average")))
          .add(Q("td")
            .klass("number")
            .text(Fns.nFormat(md.assets, 2)))))
      .add(Q("div")
        .klass("head")
        .text(_("Bests")))
      .add(bestsWorstsWg(md.bests))
      .add(Q("div")
        .klass("head")
        .text(_("Worsts")))
      .add(bestsWorstsWg(md.worsts))
    ;
  }

  // Control -------------------------------------------------------------------

  function ranking (): Void {
    menuSel = "ranking";
    show();
  }

  function statistics (modelId: String): Void {
    menuSel = modelId;
    show();
  }

  // Static --------------------------------------------------------------------

  public static function mk (wg: Domo): Void {
    Global.client.send([
      "prg" => Js.ws("FMarket"),
      "source" => Js.ws("ModelsPg"),
      "rq" => Js.ws("idata")
    ], rp -> {
      final modelIds = rp["modelIds"].ra().map(m -> m.rs());
      final mdStats = MdStats.fromJs(rp["mdStats"]);
      new ModelsPg(wg, modelIds, mdStats).show();
    });
  }

}
