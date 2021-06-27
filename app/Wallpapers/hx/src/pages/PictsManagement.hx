// Copyright 26-Apr-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pages;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Js;
import dm.It;
import dm.Str;
import data.Pict;
import I18n._;

/// Pictures management.
class PictsManagement {
  final pcols = 5;
  final prows = 4;
  final ptds: Int;
  final maxPage: Int;

  final wg: Domo;
  final fnBack: Void -> Void;
  final groups: Int;
  final group: String;
  final picts: Array<Pict>;
  final page: Int;
  final totalSights: Int;
  final shownSights: Int;

  function new (
    wg: Domo,
    groups: Int, group: String,
    picts: Array<Pict>, page: Int,
    totalSights: Int, shownSights: Int,
    fnBack: Void -> Void
  ) {
    this.wg = wg;
    picts.sort((e1, e2) ->
      Str.compare(e1.id.toUpperCase(), e2.id.toUpperCase())
    );
    ptds = pcols * prows;
    this.groups = groups;
    this.group = group;
    this.picts = picts;
    maxPage = Std.int((picts.length - 1) / ptds);
    if (page > maxPage) {
      setPage(maxPage);
      return;
    }

    this.page = page > maxPage ? maxPage : page;
    this.totalSights = totalSights;
    this.shownSights = shownSights;
    this.fnBack = fnBack;
    view();
  }

  // VIEW

  function view (): Void {
    wg
      .removeAll()
      .add(Q("div")
        .klass("head")
        .text(_('Pictures Management [ $shownSights / $totalSights ]')))
      .add(Q("table")
        .att("align", "center")
        .add(Q("tr")
          .add(Q("td")
            .add(Ui.link(fnBack)
              .klass("link")
              .text("[ " + _("Back") + " ]")))))
      .add(Q("table")
        .klass("main")
        .add(Q("tr")
          .add(Q("td")
            .add(Q("table")
              .att("align", "left")
              .add(Q("tr")
                .add(Q("td")
                  .att("colspan", groups)
                  .style("width:150px")
                  .add(Ui.hrule(_("Groups"), 25)))
                .add(Q("td")
                  .style("width:100px")
                  .add(Ui.hrule(_("Sights"), 25))))
              .add(Q("tr")
                .adds(It.range(groups).map(i ->
                    Q("td")
                      .klass(Std.string(i) == group ? "frame3": "frame")
                      .style("cursor:pointer;text-align:center")
                      .text(Std.string(i))
                      .on(CLICK, e -> setGroup(Std.string(i)))
                  ).to())
                .add(Q("td")
                  .klass("frame3")
                  .style("text-align:center")
                  .text(Std.string(It.from(picts).reduce(
                      0, (sum, pic) -> sum += pic.level
                    )))))
              .add(Q("tr")
                .add(Q("td")
                  .att("colspan", groups)
                  .add(Q("hr")))
                .add(Q("td")
                  .add(Q("hr"))))))
          .add(Q("td")
            .add(Q("table")
              .att("align", "right")
              .adds(pagesWg())))))
      .add(Q("div")
        .style("padding-top:10px;"))
      .add(Q("table")
        .klass("main")
        .adds(rows()))
      .add(Cts.foot)
    ;
  }

  function pagesWg (): Array<Domo> {
    final trs: Array<Domo> = [];
    final tds: Array<Array<Domo>> = [];
    final max = maxPage * ptds < picts.length ? maxPage + 1 : maxPage;
    for (i in 0...max) {
      final first = i * ptds;
      var last = first + ptds - 1;
      if (last >= picts.length) last = picts.length - 1;
      for (i in 0...3) tds.push([]);
      tds[0].push(Q("td")
        .style("text-align:center;color:#c9c9c9")
        .text(Str.left(picts[first].id.toUpperCase(), 2)));
      tds[1].push(Q("td")
        .klass(i == page ? "frame3": "frame")
        .style("cursor:pointer")
        .text(Cts.formatInt(i + 1, 2))
        .on(CLICK, e -> setPage(i))
      );
      tds[2].push(Q("td")
        .style("text-align:center;color:#c9c9c9")
        .text(Str.left(picts[last].id.toUpperCase(), 2)));
    }

    for (i in 0...3) trs.push(Q("tr").adds(tds[i]));
    return trs;
  }

  function rows (): Array<Domo> {
    final r: Array<Domo> = [];
    var np = page * pcols * prows;
    for (row in 0...prows) {
      final tr = Q("tr");
      for (col in 0...pcols) {
        if (np < picts.length) {
          tr.add(tdPict(picts[np]));
        } else {
          tr.add(Q("td"));
        }
        ++np;
      }
      r.push(tr);
    }
    return r;
  }

  function tdPict(p: Pict): Domo {
    final sels: Array<Domo> = [];
    for (i in Cts.minPictLevel...(Cts.maxPictLevel + 1)) {
      sels.push(Q("td")
        .add(Q("table")
          .klass("main")
          .add(Q("tr")
            .add(Q("td")
              .add(Q("input")
                .att("type", "radio")
                .att("name", p.id)
                .checked(i == p.level))
                .on(CLICK, e -> setLevel(p.id, i)))
            .add(Q("td")
              .add(Q("span")
                .text(Std.string(i))))))
      );
    }
    return Q("td")
      .style("text-align:center")
      //.klass("frame")
      .add(Ui.img("fondosEscritorio/" + group + "/" + p.id)
        .klass("frame")
        .style("width:175px")
        .on(CLICK, e -> showPict(p.id)))
      .add(Q("table")
        .att("align", "center")
        .add(Q("tr")
          .adds(sels)))
    ;
  }

  function showPict (id: String): Void {
    final table = Q("table")
      .att("align", "center")
      .add(Q("tr")
        .add(Q("td")
          .add(Ui.img("fondosEscritorio/" + group + "/" + id)
            .klass("frame")
            .style("width:800px;text-align:center")
            .on(CLICK, e -> mk(wg, fnBack)))))
    ;
    wg.removeAll().add(table);
  }

  // CONTROL

  function setGroup (g: String): Void {
    Cts.client.ssend([
      "source" => Js.ws("PictsManagement"),
      "rq" => Js.ws("setGroup"),
      "group" => Js.ws(g)
    ], rp -> {
      mk(wg, fnBack);
    });
  }

  function setPage (pg: Int): Void {
    Cts.client.ssend([
      "source" => Js.ws("PictsManagement"),
      "rq" => Js.ws("setPage"),
      "page" => Js.wi(pg)
    ], rp -> {
      mk(wg, fnBack);
    });
  }

  function setLevel (id: String, lv: Int): Void {
    Cts.client.ssend([
      "source" => Js.ws("PictsManagement"),
      "rq" => Js.ws("setLevel"),
      "group" => Js.ws(group),
      "id" => Js.ws(id),
      "level" => Js.wi(lv)
    ], rp -> {
      mk(wg, fnBack);
    });
  }

  // STATIC

  public static function mk (wg: Domo, fnBack: Void -> Void): Void {
    Cts.client.ssend([
      "source" => Js.ws("PictsManagement"),
      "rq" => Js.ws("idata")
    ], rp -> {
      final groups = rp["groups"].ri();
      final group = rp["group"].rs();
      final picts = rp["picts"].ra().map(e -> Pict.fromJs(e));
      final page = rp["page"].ri();
      final totalSights = rp["totalSights"].ri();
      final shownSights = rp["shownSights"].ri();

      new PictsManagement(
        wg,
        groups, group,
        picts, page,
        totalSights, shownSights,
        fnBack
      );
    });
  }
}
