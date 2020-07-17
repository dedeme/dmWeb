// Copyright 29-Jun-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

using StringTools;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Dec;
import dm.It;
import dm.Dt;
import dm.Str;
import data.All;
import data.Cu;
import data.DiaryEntry;
import wgs.AccountSelector;
import I18n._;
import I18n._args;

/// Acounts page.
class Accs {

  static var wg: Domo;
  static var data: All;
  static var acc: String;
  static var ixFirstRow: Int;
  static var cashEntryIxs: Array<Int>;

  static final listDiv = Q("div");

  /// Control ------------------------------------------------------------------

  static function fixUrl () {
    js.Browser.location.assign('?accs&$acc&${Std.string(ixFirstRow)}');
  }

  static function helpAccountClick(acc: String, accDesc: String) {
  }

  static function getEntriesIndex (ix: Int): Int {
    for (i in 0...cashEntryIxs.length) {
      if (cashEntryIxs[i] == ix) return i;
    }
    return cashEntryIxs.length - 1;
  }

  static function upClick () {
    final i = getEntriesIndex(ixFirstRow);
    if (ixFirstRow > -1 && i < cashEntryIxs.length - 1) {
      ixFirstRow = cashEntryIxs[i + 1];
      listDiv.removeAll().add(list());
    }
  }

  static function downClick () {
    final i = getEntriesIndex(ixFirstRow);
    if (i > 0) {
      ixFirstRow = cashEntryIxs[i - 1];
      listDiv.removeAll().add(list());
    }
  }

  static function dupClick () {
    final i = getEntriesIndex(ixFirstRow) + Std.int(Cts.tableLen / 2);
    if (ixFirstRow > -1 && i < cashEntryIxs.length) {
      ixFirstRow = cashEntryIxs[i];
      listDiv.removeAll().add(list());
    }
  }

  static function ddownClick () {
    final i = getEntriesIndex(ixFirstRow) - Std.int(Cts.tableLen / 2);
    if (i > 0) {
      ixFirstRow = cashEntryIxs[i];
      listDiv.removeAll().add(list());
    }
  }

  static function topClick () {
    if (ixFirstRow > -1) {
      ixFirstRow = cashEntryIxs[cashEntryIxs.length - 1];
      listDiv.removeAll().add(list());
    }
  }

  static function bottomClick () {
    if (ixFirstRow > -1) {
      var i = Cts.tableLen - 1;
      if (i >= cashEntryIxs.length) i = cashEntryIxs.length - 1;
      ixFirstRow = cashEntryIxs[i];
      listDiv.removeAll().add(list());
    }
  }

  static function monthClick (m: Int) {
    if (ixFirstRow > -1) {
      final diary = data.acc.diary;
      final len = cashEntryIxs.length;
      var i = 0;
      while (true) {
        final e = diary[cashEntryIxs[i++]];
        if (Dt.month(e.date) >= m || i == len) break;
      }
      ixFirstRow = cashEntryIxs[i - 1];
      listDiv.removeAll().add(list());
    }
  }

  static function goToDiary (diaryIx: Int) {
    js.Browser.location.assign('?diary&${acc}&${diaryIx}');
  }


  /// View ---------------------------------------------------------------------

  static function list () {
    var sum = 0.0;
    final entries = It.from(cashEntryIxs).map(i -> {
      final e = data.acc.diary[i];
      var amm = new Cu(0);
      for (a => c  in e.debits)
        if (a.startsWith(acc)) amm = amm.add(c);
      for (a => c  in e.credits)
        if (a.startsWith(acc)) amm = amm.sub(c);
      sum += amm.value;
      return {
        ix: i,
        date: e.date,
        desc: e.description,
        amm: amm,
        sum: new Cu(sum)
      }
    }).to();

    final td = () -> Q("td").klass("frame").style("vertical-align:top;");
    final tdr = () -> td().setStyle("text-align", "right");
    final tdl = () -> td().setStyle("text-align", "left");

    var cut = 0;
    for (ix in 0...entries.length) {
      final e = entries[ix];
      if (e.ix >= ixFirstRow) {
        cut = ix + 1;
        break;
      }
    }
    return Q("table")
      .att("align", "center")
      .adds(It.from(entries)
        .take(cut)
        .drop(cut - Cts.tableLen)
        .reverse()
        .map(e -> {
          return Q("tr")
            .add(tdr()
              .html(Std.string(e.ix + 1)))
            .add(td()
              .html(DateTools.format(e.date, "%d/%m")))
            .add(tdl()
              .add(Ui.link(ev -> goToDiary(e.ix))
                .klass("link")
                .html(e.desc)))
            .add(tdr()
              .html(e.amm.value > 0 ? e.amm.toString() : ""))
            .add(tdr()
              .html(e.amm.value < 0 ? e.amm.abs().toString() : ""))
            .add(tdr()
              .html(e.sum.toString()))
          ;
        }).to())
    ;
  }

  static function left () {
    final upAcc = acc.length == 5
      ? acc.substring(0, 3)
      : acc.substring(0, acc.length - 1)
    ;
    return Q("td")
      .klass("frame")
      .style("width:250px;vertical-align:top;white-space:nowrap")
      .add(Q("ul")
        .style("list-style:none;padding-left:0px;")
        .adds(It.range(1, acc.length + 1).map(lg ->
          Q("li")
            .html("<a href='#' onclick='return false;'>" +
              Str.cutRight(
                data.acc.descriptionOf(acc.substring(0, lg)), Cts.helpLen
              ) + "</a>")
            .add(Q("ul").att("id", "hlist")
              .style("list-style:none;padding-left:10px;")
              .adds(It.fromMap(data.acc.subOf(acc.substring(0, lg - 1)))
                .sort((tp1, tp2) -> tp1.e1 > tp2.e1 ? 1 : -1)
                .map(tp ->
                  Q("li")
                    .add(Ui.link(e ->
                        js.Browser.location.assign("?accs&" + tp.e1)
                      )
                      .klass("link")
                      .att("title", tp.e1)
                      .html(
                        Str.cutRight(tp.e2.description,
                        Cts.helpLen
                      )))).to()))).to())
        .add(Q("li")
          .add(Q("hr")))
        .adds(acc.length == 5
          ? []
          : It.fromMap(data.acc.sub(acc))
            .sort((tp1, tp2) -> tp1.e1 > tp2.e1 ? 1 : -1)
            .map(tp ->
              Q("li")
                .add(Ui.link(e ->
                  js.Browser.location.assign("?accs&" + tp.e1)
                )
                  .klass("link")
                  .att("title", Cts.accFormat(tp.e1))
                  .html(Str.cutRight(tp.e2.description, Cts.helpLen)))).to()))
    ;
  }

  static function right () {

    function mkSubmenu () {
      final separator = () -> Q("span").text("|");
      final entry = (tx, lk) ->
        Ui.link(e -> js.Browser.location.assign("?accs&" + lk))
          .klass("link")
          .text(" " + tx + " ")
      ;

      var es = [separator(), entry("*", "*"), separator()];
      final add = (tx, lk) -> {
        es.push(entry(tx, lk));
        es.push(separator());
      }
      if (acc.length > 0) add(acc.charAt(0), acc.charAt(0));
      if (acc.length > 1) add(acc.charAt(1), acc.substring(0, 2));
      if (acc.length > 2) add(acc.charAt(2), acc.substring(0, 3));
      if (acc.length > 3) add(acc.substring(3), acc);
      return Q("div")
        .add(Q("p").adds(es))
        .add(Q("p")
          .add(Q("span")
            .klass("frame")
            .html(acc == "" ? _("All") : data.acc.descriptionOf(acc))))
      ;
    }


    final r =Q("td")
      .style("text-align:center;vertical-align:top;")
      .add(Q("div")
        .klass("head")
        .text(_("Accs")))
      .add(mkSubmenu())
      .add(Q("div")
        .add(Ui.link(e -> fixUrl())
          .add(Ui.img("pin"))))
      .add(Q("table")
        .att("align", "center")
        .add(Q("tr")
          .add(Q("td"))))
      .add(Q("hr"))
      .add(Q("table")
        .att("align", "center")
        .add(Q("tr")
          .add(Q("td").att("colspan", 3))
          .add(Q("td").klass("diary").add(Ui.link(e -> upClick())
            .setStyle("font-family", "monospace").html("&nbsp;\u2191&nbsp;")))
          .add(Q("td").klass("diary").add(Ui.link(e -> downClick())
            .setStyle("font-family", "monospace").html("&nbsp;\u2193&nbsp;")))
          .add(Q("td").klass("diary").add(Ui.link(e -> dupClick())
            .setStyle("font-family", "monospace").html("\u2191\u2191")))
          .add(Q("td").klass("diary").add(Ui.link(e -> ddownClick())
            .setStyle("font-family", "monospace").html("\u2193\u2193")))
          .add(Q("td").klass("diary").add(Ui.link(e -> topClick())
            .setStyle("font-family", "monospace").html("&nbsp;\u2912&nbsp;")))
          .add(Q("td").klass("diary").add(Ui.link(e -> bottomClick())
            .setStyle("font-family", "monospace").html("&nbsp;\u2913&nbsp;")))
          .add(Q("td").att("colspan", 3)))
        .add(Q("tr")
          .adds(It.range(1, 13).map(i ->
            Q("td").klass("diary").add(Ui.link(e -> {
              monthClick(i);
            }).html('&nbsp;${i}&nbsp;'))).to())))
      .add(Q("hr"))
      .add(listDiv)
    ;

    listDiv.add(list());
    return r;
  }

  /// Constructor.
  ///   wg: Widget.
  ///   data: Accounting data.
  ///   acc: Selected account or "".
  ///   ix: Index of the first shown annotation or "".
  public static function mk (
    wg: Domo, data: All, acc: String, ix: String
  ): Void {
    function mostUsed (): String {
      final it = data.acc.mostUsedSubaccounts(false);
      final r = it.next().e1;
      return r == Cts.cash ? it.next().e1 : r;
    }

    Accs.wg = wg;
    Accs.data = data;
    Accs.acc = acc == "" || data.acc.descriptionOf(acc) == ""
      ? acc == "*" ? "" : mostUsed()
      : acc
    ;

    final diary = data.acc.diary;
    final used = data.acc.usedAccs(Accs.acc);
    final lastIx = used.length == 0 ? -1 : used[used.length - 1];
    final ixn = Dec.digits(ix) ? Std.parseInt(ix) : lastIx;
    ixFirstRow = ixn > lastIx ? lastIx : ixn;
    cashEntryIxs = used;

    wg
      .removeAll()
      .add(Q("table")
        .klass("main")
        .add(Q("tr")
          .add(left())
          .add(right())))
    ;
  }
}
