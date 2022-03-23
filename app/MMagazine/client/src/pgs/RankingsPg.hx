// Copyright 04-Jan-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pgs;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Js;
import dm.Dec;
import dm.It;
import dm.Opt;
import cm.data.Ranking;
import cm.data.RankingEntry;
import I18n._;
import I18n._args;

/// Rankings page.
class RankingsPg {
  final wg: Domo;
  final rks: Array<Ranking>;
  final ptsPercs: Array<Float>;
  final deltas: Array<Option<Float>>;
  final deltaDfs: Array<Option<Float>>;
  final deltaDfPercs: Array<Option<Float>>;

  function new (wg: Domo, rks: Array<Ranking>) {
    this.wg = wg;
    this.rks = rks;

    deltas = [];
    deltaDfPercs = [];


    final es0 = rks[rks.length - 2].ranking;
    final es1 = rks[rks.length - 1].ranking;
    var mxPts = es1[0].value;
    var mnPts = mxPts;
    var mxDf = 0.0;
    for (i in 0...es0.length) {
      final e = es1[i];
      final param1 = e.param;
      final value1 = e.value;
      if (value1 > mxPts) mxPts = value1;
      if (value1 < mnPts) mnPts = value1;

      deltas.push(switch (It.from(es0).find(e -> e.param == param1)) {
        case Some(p): Some(value1 - p.value);
        case None: None;
      });
    }

    var mxDf = None;
    deltaDfs = switch (deltas[0]) {
      case Some(df0): deltas.map(d -> switch (d) {
          case Some(df):
            final d2 = (df - df0) / Math.abs(df0);
            mxDf = switch (mxDf) {
              case Some(mx): Math.abs(d2) > mx ? Some(Math.abs(d2)) : mxDf;
              case None: Some(Math.abs(d2));
            }
            Some(d2);
          case None:
            None;
        });
      case None:
        deltas.map(d -> None);
    }

    final mxMnPts = mxPts - mnPts;
    ptsPercs = es1.map(e -> (e.value - mnPts) / mxMnPts);

    deltaDfPercs = switch(mxDf) {
      case Some(mx): deltaDfs.map(d -> switch (d) {
          case Some(df): Some(df / mx);
          case None: None;
        });
      case None: deltaDfs.map(d -> None);
    }
  }

  // View ----------------------------------------------------------------------

  public function show (): Void {
    final headTds = [Q("td")
      .klass("chead")
      .text(dateFormat(rks[0].date))
    ];
    for (i in 1...rks.length) {
      final rk = rks[i];
      headTds.push(Q("td")
        .klass("chead")
        .text(dateFormat(rk.date))
      );
    }
    headTds.push(Q("td").klass("chead").text(_("Pts.")));
    headTds.push(Q("td").klass("chead"));
    headTds.push(Q("td").klass("chead").html("&Delta;"));
    headTds.push(Q("td").klass("chead").html("&delta;"));
    headTds.push(Q("td").klass("chead"));

    final trs = [];
    for (i in 0...deltas.length) {
      trs.push(Q("tr")
        .add(Q("td") // 0 day
          .klass("cframe")
          .style(
              "text-align: center;" +
              (isDropped(0, i) ? "text-decoration: line-through;" : "")
            )
          .text(Dec.toIso(rks[0].ranking[i].param / 10000, 1)))
        .add(Q("td") // 1st day
          .klass("cframe")
          .style(
              "text-align: center;" +
              (isDropped(1, i) ? "text-decoration: line-through;" : "")
            )
          .add(movMark(1, i))
          .add(Q("span")
            .text(" " + Dec.toIso(rks[1].ranking[i].param / 10000, 1))))
        .add(Q("td") // 2nd day
          .klass("cframe")
          .style(
              "text-align: center;" +
              (isDropped(2, i) ? "text-decoration: line-through;" : "")
            )
          .add(movMark(2, i))
          .add(Q("span")
            .text(" " + Dec.toIso(rks[2].ranking[i].param / 10000, 1))))
        .add(Q("td") // 3rd day
          .klass("frame")
          .style("text-align: center")
          .add(movMark(3, i))
          .add(Q("span")
            .text(" " + Dec.toIso(rks[3].ranking[i].param / 10000, 1))))
        .add(Q("td") // pts.
          .klass("cframe")
          .style("text-align: center")
          .text(Dec.toIso(rks[3].ranking[i].value * 1000, 2)))
        .add(Q("td")  // Gr. pts.
          .klass("cframe")
          .style("text-align: left")
          .add(mkBar(Some(ptsPercs[i]))))
        .add(Q("td") // Delta
          .klass("cframe")
          .style("text-align: center")
          .text(switch(deltas[i]) {
              case Some(d): Dec.toIso(d * 1000, 3);
              case None: "*,***";
            }))
        .add(Q("td") // delta
          .klass("cframe")
          .style("text-align: center")
          .text(switch(deltaDfs[i]) {
              case Some(d): Dec.toIso(d * 100, 3);
              case None: "*,**";
            }))
        .add(Q("td") // Gr delta.
          .klass("cframe")
          .style("text-align: left")
          .add(mkBar2(deltaDfPercs[i])))
      );
    }

    wg
      .removeAll()
      .add(Q("table")
        .style("border-collapse : collapse;")
        .att("align", "center")
        .add(Q("tr")
          .adds(headTds))
        .adds(trs))
    ;
  }

  static function emptyBar (
    width = 100,
    height = 10,
    background = "#e2e5e2"
  ): Domo {
      return Q("table")
        .style(
            "border-collapse : collapse;" +
            "width: " + width + "px;" +
            "height: " + height + "px;"
          )
        .add(Q("tr")
          .add(Q("td")
            .style(
              "padding: 0px 0px 0px " + Std.int(width / 4) + "px;" +
              "border: 1px solid rgb(110,130,150);" +
              "width: " + width + "px;" +
              "background: " + background)
            .add(Q("div")
              .style(
                  "width: " + Std.int(width / 2) + "px;" +
                  "border-top: 1px solid rgb(110,130,150);"
                ))))
      ;
  }

  public static function mkBar (
    value: Option<Float>,
    width = 100,
    height = 10,
    color = "#204080",
    background = "#e2e5e2"
  ): Domo {
    final val = Opt.get(value);

    if (val == null) return emptyBar();

    return Q("table")
      .style(
          "border-collapse : collapse;" +
          "width: " + width + "px;" +
          "height: " + height + "px;"
        )
      .add(Q("tr")
        .add(Q("td")
          .style(
            "padding: 0px;" +
            "border: 1px solid rgb(110,130,150);" +
            "width: " + Std.int(val * width) + "px;" +
            "background: " + color))
        .add(Q("td")
          .style(
            "padding: 0px;" +
            "border: 1px solid rgb(110,130,150);" +
            "width: " + (width - Std.int(val * width)) + "px;" +
            "background: " + background)))
    ;
  }

  public static function mkBar2 (
    value: Option<Float>,
    width = 100,
    height = 10,
    positiveColor = "#204080",
    negativeColor = "#804020",
    background = "#e2e5e2"
  ): Domo {
    function mkTd (v: Int, color: String): Domo {
      return Q("td")
        .style(
          "padding: 0px;" +
          "border: 1px solid rgb(110,130,150);" +
          "width: " + v + "px;" +
          "background: " + color)
      ;
    }


    final val = Opt.get(value);

    if (val == null) return emptyBar();

    final lf = Std.int(width / 2);
    final rg = width - lf;
    final lfRg = Std.int(lf * -val);
    final lfLf = lf - lfRg;
    final rgLf = Std.int(rg * val);
    final rgRg = rg - rgLf;

    return Q("table")
      .style(
          "border-collapse : collapse;" +
          "width: " + width + "px;" +
          "height: " + height + "px;"
        )
      .add(Q("tr")
        .adds(
          val < 0
          ? [
              mkTd(lfLf, background),
              mkTd(lfRg, negativeColor),
              mkTd(rg, background)
            ]
          : val > 0
            ? [
                mkTd(lf, background),
                mkTd(rgLf, positiveColor),
                mkTd(rgRg, background)
              ]
            : [
                mkTd(lf, background),
                mkTd(rg, background)
              ]
          ))
    ;
  }

  // Control -------------------------------------------------------------------

  function movMark (rkIx: Int, posIx: Int): Domo {
    var img = "plus";
    if (rkIx > 0) {
      final prIx = rkIx - 1;
      final id = rks[rkIx].ranking[posIx].param;
      final prPos = It.from(rks[prIx].ranking).indexf(e -> e.param == id);
      img = prPos == -1 ? "plus"
        : prPos > posIx ? "rk-up"
        : prPos < posIx ? "rk-down"
        : "rk-eq"
      ;
    }
    return Ui.img(img).style("vertical-align:top");
  }

  function isDropped (rkIx: Int, posIx: Int): Bool {
    if (rkIx == rks.length - 1) return false;
    final id = rks[rkIx].ranking[posIx].param;
    return It.from(rks[rkIx + 1].ranking).indexf(e -> e.param == id) == -1;
  }


  // Static --------------------------------------------------------------------

  public static function mk (wg: Domo): Void {
    Cts.client.send([
      "source" => Js.ws("RankingsPg"),
      "rq" => Js.ws("idata"),
    ], rp -> {
      final rankings = rp["rankings"].ra().map(e -> Ranking.fromJs(e));
      new RankingsPg(wg, rankings).show();
    });
  }

  static function dateFormat (date: String): String {
    final m = date.substring(4, 6);
    final d = date.substring(6);
    return I18n.lang == "es" ? d + "/" + m : m + "/" + d;
  }

}
