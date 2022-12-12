// Copyright 08-Dic-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pgs;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Js;
import dm.It;
import dm.Menu;
import data.Ranking;
import data.Flea;
import I18n._;

/// Rankings page
class RankingsPg {
  final wg: Domo;
  final modelIds: Array<String>;
  // Ordered from after to before
  final rankings: Array<Ranking>;

  var menuSel = "last";

  function new (wg: Domo, modelIds: Array<String>, rankings: Array<Ranking>) {
    this.wg = wg;
    this.modelIds = modelIds;
    this.rankings = rankings;
  }

  // View ----------------------------------------------------------------------

  public function show (): Void {

    final menu = new Menu(
      [ Menu.toption("last", _("Current"), last),
        Menu.separator(),
        Menu.toption("all", _("AllRankings"), all)
      ],
      [],
      menuSel
    );

    wg
      .removeAll()
      .add(menu.wg)
    ;

    if (menuSel == "last") {
      showLast();
    } else {
      showAll();
    }
  }

  function showLast(): Void {
    function mkRow (f: Flea, i: Int): Domo {
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
          .add(positionImg(0, f, i)))
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
    It.from(rankings[0].fleas).eachIx((f, i) -> trs.push(mkRow(f, i)));

    wg
      .add(Q("table")
        .klass("border")
        .att("align", "center")
        .add(Q("tr")
          .add(Q("td")
            .klass("header"))
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
        .adds(trs))
    ;
  }

  function showAll(): Void {
    final nRks = rankings.length;
    final nRks2 = Std.int(nRks / 2);

    function mkRows(start: Int, end: Int): Array<Domo> {
      return [
        Q("tr")
          .adds(It.range(start, end).map(i ->
              Q("td")
                .klass("header")
                .att("colspan", 5)
                .text(Fns.dFormat(rankings[i].date))
            ).to()),
        Q("tr")
          .adds(It.range(start, end).reduce(It.empty(), (r, i) ->
              r.cat(It.from([
                Q("td")
                  .klass("header"),
                Q("td")
                  .klass("header")
                  .style("text-align: left")
                  .text(_("Id")),
                Q("td")
                  .klass("header")
                  .style("text-align: left")
                  .text("C."),
                Q("td")
                  .klass("header")
                  .text(_("Ms.")),
                Q("td")
                  .klass("header")
                  .style("text-align: right")
                  .text(_("Assets")),
              ]))
            ))
      ].concat(
        It.range(rankings[0].fleas.length).map(row ->
          Q("tr")
            .adds(It.range(start, end).reduce(It.empty(), (r, col) -> {
                final f = rankings[col].fleas[row];
                return r.cat(It.from([
                  Q("td")
                    .add(positionImg(col, f, row)),
                  Q("td")
                    .klass("border")
                    .style(
                        "text-align:right;" +
                        "text-decoration:"+ (
                          isRemoved(col, f)
                            ? "line-through;"
                            : "none;"
                        ) +
                        "background:" + (
                          f.id == rankings[0].fleas[0].id
                            ? "#ccad0f"
                            : f.id == rankings[0].fleas[1].id
                              ? "#b4b3ad"
                              : f.id == rankings[0].fleas[2].id
                                ? "#9e6a25"
                                : "rgb(250, 250, 250)"
                        )
                      )
                    .text("" + f.id),
                  Q("td")
                    .klass("borderWhite")
                    .style("text-align: right")
                    .text("" + f.cycle),
                  Q("td")
                    .klass("borderWhite")
                    .att("title", f.fmtModels().join("\n"))
                    .style("color:#000080")
                    .text(f.fmtModels2()),
                  Q("td")
                    .klass("number")
                    .text(Fns.nFormat(f.assets, 2))
                ]));
              }).to())
        ).to()
      );
    }

    wg
      .add(Q("table")
        .klass("border")
        .att("align", "center")
        .adds(mkRows(0, nRks2))
        .add(Q("tr")
          .add(Q("td")
            .att("colspan", nRks2 * 5)
            .add(Q("hr"))))
        .adds(mkRows(nRks2, nRks)))
    ;
  }

  // Control -------------------------------------------------------------------

  function last (): Void {
    menuSel = "last";
    show();
  }

  function all (): Void {
    menuSel = "all";
    show();
  }

  // Returns a position image
  function positionImg (rankIx: Int, flea: Flea, fIx: Int): Domo {
    final rankIx1 = rankIx + 1;
    if (rankIx1 >= rankings.length) return Ui.img("rk-new");
    final fIx1 = It.from(rankings[rankIx1].fleas).indexf(f -> f.id == flea.id);
    if (fIx1 == -1) return Ui.img("rk-new");
    if (fIx > fIx1 + 4) return Ui.img("rk-down2");
    if (fIx > fIx1) return Ui.img("rk-down");
    if (fIx < fIx1 - 4) return Ui.img("rk-up2");
    if (fIx < fIx1) return Ui.img("rk-up");
    return Ui.img("rk-eq");
  }

  function isRemoved (rankIx: Int, flea: Flea): Bool {
    if (rankIx == 0) return false;
    return It.from(rankings[rankIx - 1].fleas).indexf(f -> f.id == flea.id) == -1;
  }

  // Static --------------------------------------------------------------------

  public static function mk (wg: Domo): Void {
    Global.client.send([
      "prg" => Js.ws("FMarket"),
      "source" => Js.ws("RankingsPg"),
      "rq" => Js.ws("idata")
    ], rp -> {
      final modelIds = rp["modelIds"].ra().map(m -> m.rs());
      final rankings = rp["rankings"].ra().map(Ranking.fromJs);
      new RankingsPg(wg, modelIds, rankings).show();
    });
  }

}
