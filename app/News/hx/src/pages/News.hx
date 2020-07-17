// Copyright 11-Jul-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pages;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Js;
import dm.Dt;
import dm.Dec;
import I18n._;
import data.NewsEntry;

/// News page.
class News {

  // Control -------------------------------------------------------------------

  static function sel(wg: Domo, n: NewsEntry, ev: UserEvalType) {
    final e = ev == NONE ? 0 : ev == UP ? 1 : 2;
    Cts.client.ssend([
      "source" => Js.ws("News"),
      "rq" => Js.ws("eval"),
      "entry" => n.toJs(),
      "ev" => Js.wi(e)
    ], rp -> {
      n.userEval = ev;
      setEDiv(wg, n);
    });
  }

  static function read (wg: Domo, downWg: Domo) {
    downWg
      .removeAll()
      .add(Ui.img("wait.gif"))
    ;
    Cts.client.ssend([
      "source" => Js.ws("News"),
      "rq" => Js.ws("newsNews"),
    ], rp -> {
      final errs = rp["errs"].rArray(e -> e.rs());
      if (errs.length != 0) {
        for (e in errs) trace(e);
        Ui.alert(_("Some erros happened.\nSee console."));
      }
      mk(wg);
    });
  }

  /// View ---------------------------------------------------------------------

  static function setEDiv (eDiv: Domo, n: NewsEntry) {
    final ev = n.userEval;
    var spanOk = ev == NONE
      ? Ui.link(e -> sel(eDiv, n, UP)).add(Ui.img("well"))
      : ev == UP
        ? Ui.link(e -> sel(eDiv, n, NONE)).add(Ui.img("well"))
        : Ui.lightImg("well")
    ;
    var spanWrong = ev == NONE
      ? Ui.link(e -> sel(eDiv, n, DOWN)).add(Ui.img("error"))
      : ev == DOWN
        ? Ui.link(e -> sel(eDiv, n, NONE)).add(Ui.img("error"))
        : Ui.lightImg("error")
    ;

    eDiv
      .removeAll()
      .add(spanOk)
      .add(Q("span")
        .html("&nbsp;&nbsp;"))
      .add(spanWrong)
    ;
  }

  static function mkNewsRow(n: NewsEntry): Domo {
    final eDiv = Q("div");
    setEDiv(eDiv, n);

    return Q("tr")
      .add(Q("td")
        .klass("frame")
        .text(DateTools.format(n.date, '%d/%m')))
      .add(Q("td")
        .klass("frame")
        .style("text-align: left")
        .add(Q("a")
          .klass("news")
          .att("title", Dec.toIso(n.ttEval * 100, 2))
          .att("href", n.url)
          .att("target", "NewsViewer")
          .text(n.text)))
      .add(Q("td")
        .klass("border")
        .style("white-space: nowrap")
        .add(eDiv))
    ;
  }

  /// Constructor.
  public static function mk (wg: Domo): Void {
    Cts.client.send([
      "source" => Js.ws("News"),
      "rq" => Js.ws("read")
    ], rp -> {
      final news = rp["news"].rArray(NewsEntry.fromJs);
      final downWg = Q("div");
      wg
        .removeAll()
        .add(Q("div")
          .style("text-align:center")
          .add(Q("div")
            .klass("head")
            .html(_("News")))
          .add(downWg
            .add(Ui.link(e -> read(wg, downWg))
              .add(Ui.img("earth"))))
          .add(Q("table")
            .att("align", "center")
            .adds(news.map(mkNewsRow))))
      ;
    });
  }
}
