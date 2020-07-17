// Copyright 29-Jun-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

using StringTools;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Dec;
import dm.It;
import dm.Dt;
import dm.DatePicker;
import data.All;
import data.Cu;
import data.DiaryEntry;
import wgs.AccountSelector;
import I18n._;
import I18n._args;

/// Cash page.
class Cash {

  static var wg: Domo;
  static var data: All;
  static var acc: String;
  static var ixFirstRow: Int;
  static var cashEntryIxs: Array<Int>;

  static var datePicker = new DatePicker();
  static final dateField = Q("input")
    .att("type", "text")
    .klass("frame")
    .style("width:80px;color:#000000;text-align:center;")
  ;
  static final accIn: Domo = Q("input")
    .att("type", "text")
    .klass("frame")
    .style("width:45px;color:#000000;text-align:center;")
    .disabled(true)
  ;
  static final description: Domo = Ui.field("amm")
    .att("id", "autofocus")
    .style("width:270px")
    .on(FOCUS, e -> cast(description.e, js.html.InputElement).select())
  ;
  static final ammIn: Domo = Ui.field("accept")
    .att("id", "amm")
    .style("width:65px")
    .on(FOCUS, e -> cast(ammIn.e, js.html.InputElement).select())
  ;
  static final acceptBt = Q("button")
    .att("id", "accept")
    .html(_("Accept"))
    .on(CLICK, e -> accept())
  ;


  static final listDiv = Q("div");

  /// Control ------------------------------------------------------------------

  static function fixUrl () {
    js.Browser.location.assign('?cash&$acc&${Std.string(ixFirstRow)}');
  }

  static function helpAccountClick(acc: String, accDesc: String) {
    Cash.acc = acc.substring(0, 3);
    accIn
      .att("title", accDesc)
      .value(Cts.accFormat(acc))
    ;

    final sels: Array<{n: Int, desc: String, val: String}> = [];
    function addSels (desc: String, val: String):Void {
      var toDo = true;
      for (e in sels) {
        if (e.desc == desc && e.val == val){
          ++e.n;
          toDo = false;
        }
      }
      if (toDo) sels.push({n: 1, desc: desc, val: val});
    }

    for (e in data.acc.diary) {
      if (
        It.fromMap(e.debits).count() == 1 &&
        It.fromMap(e.credits).count() == 1 &&
        (e.debits.exists(Cts.cash) || e.credits.exists(Cts.cash))
      ) {
        if (e.debits.exists(acc)) {
          addSels(e.description, "-" + e.debits.get(acc).toString());
        }
        if (e.credits.exists(acc)) {
          addSels(e.description, e.credits.get(acc).toString());
        }
      }
    }

    if (sels.length > 0) {
      var n = 0;
      var desc = "";
      var val = "";
      for (e in sels) {
        if (e.n >= n) {
          n = e.n;
          desc = e.desc;
          val = e.val;
        }
      }
      description.value(desc);
      ammIn.value(val);
    }

    description.e.focus();
  }

  static function accept () {
    final date = datePicker.date;
    if (date == null) {
      Ui.alert(_("Date is missing"));
      return;
    }

    final a = accIn.getValue().trim().replace(".", "");
    if (a == "") {
      Ui.alert(_("Account is missing"));
      return;
    }

    final desc = description.getValue().trim();
    if (desc == "") {
      Ui.alert(_("Description is missing"));
      return;
    }

    final cu = switch (Cts.float(ammIn.getValue())) {
      case Some(0):
        Ui.alert(_("Ammount is 0"));
        null;
      case Some(n):
        new Cu(n);
      case None:
        Ui.alert(_args(_("'%0' is not a number"), [ammIn.getValue()]));
        null;
    };
    if (cu == null) return;

    final debits = cu.value > 0 ? [Cts.cash => cu] : [a => cu.abs()];
    final credits = cu.value > 0
      ? [a => cu]
      : [Cts.cash =>cu.abs()]
    ;
    final dentry = new DiaryEntry(date, desc, debits, credits);
    final ix = data.acc.addDiary(dentry) + 4;

    final diary = data.acc.diary;
    final used = data.acc.usedSubaccounts(Cts.cash);
    ixFirstRow = used.length == 0 ? -1 : used[used.length - 1];
    if (ix < ixFirstRow) ixFirstRow = ix;
    cashEntryIxs = used;

    data.send(() -> {
      accIn.value("");
      description.value("");
      ammIn.value("");
      listDiv.removeAll().add(list());
    });
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
      for (a => c  in e.debits) if (a == Cts.cash) amm = c;
      for (a => c  in e.credits) if (a == Cts.cash) amm = c.negate();
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
              .html(e.amm.toString()))
            .add(tdr()
              .html(e.sum.toString()))
          ;
        }).to())
    ;
  }

  static function left () {
    return new AccountSelector(data.acc, acc, helpAccountClick, true).wg;
  }

  static function right () {
    datePicker = new DatePicker();
    datePicker.action = m -> description.e.focus();
    Ui.changePoint(ammIn);
    if (!data.conf.isLastYear()) {
      description.disabled(true);
      ammIn.disabled(true);
      acceptBt.disabled(true);
    }

    final r =Q("td")
      .style("text-align:center;vertical-align:top;")
      .add(Q("div")
        .klass("head")
        .text(_("Cash")))
      .add(Q("div")
        .add(Ui.link(e -> fixUrl())
          .add(Ui.img("pin"))))
      .add(Q("table")
        .att("align", "center")
        .add(Q("tr")
          .add(Q("td").add(datePicker.mkText(dateField)))
          .add(Q("td").add(accIn))
          .add(Q("td").add(description))
          .add(Q("td").add(ammIn))
          .add(Q("td").add(acceptBt))))
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
    if (data.acc.descriptionOf(acc) == "" || acc.length > 3) acc = "";
    Cash.wg = wg;
    Cash.data = data;
    Cash.acc = acc == "" || !data.acc.accounts.exists(acc)
      ? Cts.cash.substring(0, 3) : acc;
    final diary = data.acc.diary;
    final used = data.acc.usedSubaccounts(Cts.cash);
    final lastIx = used.length == 0 ? -1 : used[used.length - 1];
    final ixn = Dec.digits(ix) ? Std.parseInt(ix) : lastIx;
    ixFirstRow = ixn > lastIx ? lastIx : ixn;
    cashEntryIxs = used;

    wg
      .removeAll()
      .add(Q("table")
        .klass("main")
        .add(Q("tr")
          .add(data.conf.isLastYear() ? left() : Q("div"))
          .add(right())))
    ;
  }
}
