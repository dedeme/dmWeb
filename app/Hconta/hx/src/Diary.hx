// Copyright 29-Jun-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

using StringTools;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.It;
import dm.Dec;
import dm.Str;
import dm.Dt;
import dm.Opt;
import dm.DatePicker;
import data.All;
import data.Acc;
import data.Cu;
import data.DiaryEntry;
import wgs.AccountSelector;
import I18n._;
import I18n._args;

/// Diary page.
class Diary {
  static var wg: Domo;
  static var data: All;
  static var acc: String;
  static var ixFirstRow: Int;

  static var ixEdited = -1;
  static var dateEdited = "";

  static var datePicker = new DatePicker();
  static final dateField = Q("input")
    .att("type", "text")
    .klass("frame")
    .style("width:80px;color:#000000;text-align:center;")
  ;
  static final number = Q("input")
    .att("type", "text").klass("frame")
    .style(
      "width:50px;background-color:#f0f0ff;color:#000000;" +
      "text-align:center;"
    )
    .disabled(true)
  ;
  static final description = Ui.field("debit")
    .att("id", "description")
    .style("width:270px")
  ;
  static var entryRows: Array<Array<Domo>> = [];
  static var entryAccSel = Ui.field("");

  static final editDiv = Q("div");
  static final listDiv = Q("div");

  /// Control ------------------------------------------------------------------

  static function fixUrl () {
    js.Browser.location.assign('?diary&$acc&${Std.string(ixFirstRow)}');
  }

  static function helpAccountClick(acc: String, desc: String) {
    Diary.acc = acc.substring(0, 3);
    entryAccSel
      .att("title", desc)
      .text(Cts.accFormat(acc))
    ;
  }

  static function newClick () {
    ixEdited = -1;
    dateEdited = "";
    editDiv.removeAll().add(entry());
  }

  static function upClick () {
    if (ixFirstRow > -1 && ixFirstRow < data.acc.diary.length - 1) {
      ixFirstRow++;
      listDiv.removeAll().add(list());
    }
  }

  static function downClick () {
    if (ixFirstRow > 0) {
      ixFirstRow--;
      listDiv.removeAll().add(list());
    }
  }

  static function dupClick () {
    if (ixFirstRow > -1) {
      ixFirstRow += Std.int(Cts.tableLen / 2);
      if (ixFirstRow >= data.acc.diary.length) {
        ixFirstRow = data.acc.diary.length - 1;
      }
      listDiv.removeAll().add(list());
    }
  }

  static function ddownClick () {
    if (ixFirstRow > -1) {
      ixFirstRow -= Std.int(Cts.tableLen / 2);
      if (ixFirstRow < 0) {
        ixFirstRow = 0;
      }
      listDiv.removeAll().add(list());
    }
  }

  static function topClick () {
    if (ixFirstRow > -1) {
      ixFirstRow = data.acc.diary.length - 1;
      listDiv.removeAll().add(list());
    }
  }

  static function bottomClick () {
    if (ixFirstRow > -1) {
      ixFirstRow = Cts.tableLen - 1;
      if (ixFirstRow >= data.acc.diary.length) {
        ixFirstRow = data.acc.diary.length - 1;
      }
      listDiv.removeAll().add(list());
    }
  }

  static function monthClick (m: Int) {
    if (ixFirstRow > -1) {
      final diary = data.acc.diary;
      final len = diary.length;
      var i = 0;
      while (true) {
        final e = diary[i++];
        if (Dt.month(e.date) >= m || i == len) break;
      }
      ixFirstRow = i - 1;
      listDiv.removeAll().add(list());
    }
  }

  static function goToAcc (subacc: String, diaryIx: Int) {
    js.Browser.location.assign('?accs&$subacc&$diaryIx');
  }

  static function deleteClick (diaryIx: Int) {
    if (Ui.confirm(_args(
      _("Delete annotation %0:\n%1?"), [
      Std.string(diaryIx + 1),
      data.acc.diary[diaryIx].description
    ]))) {
      data.acc.diary.splice(diaryIx, 1);
      final dlen = data.acc.diary.length;
      ixFirstRow = ixFirstRow >= dlen ? dlen - 1 : ixFirstRow;
      data.send(() -> {
        listDiv.removeAll().add(list());
      });
    }
  }

  static function modifyClick (diaryIx: Int) {
    ixEdited = diaryIx;
    dateEdited = Dt.to(data.acc.diary[diaryIx].date);
    editDiv.removeAll().add(entry());
  }

  static function cancelEntry () {
    editDiv.removeAll();
  }

  static function acceptEntry () {
    final date = datePicker.date;
    if (date == null) {
      Ui.alert(_("Date is missing"));
      return;
    }

    final desc = description.getValue().trim();
    if (desc == "") {
      Ui.alert(_("Description is missing"));
      return;
    }

    final debits = new Map<String, Cu>();
    final credits = new Map<String, Cu>();
    var dsum = 0.0;
    var csum = 0.0;
    for (e in entryRows) {
      var acc = e[0].getText().replace(".", "");
      if (debits.exists(acc) || credits.exists(acc)) {
        Ui.alert(_args(_("Account %0 is repeated"), [Cts.accFormat(acc)]));
        return;
      }
      switch (Cts.float(e[1].getValue())) {
      case Some(0):
      case Some(n):
        if (acc == "") {
          Ui.alert(_args(_(
            "Account for ammount %0 is missing"), [Std.string(n)]
          ));
          return;
        }
        final cu = new Cu(n);
        debits.set(acc, cu);
        dsum += cu.value;
      case None:
        Ui.alert(_args(_("'%0' is not a number"), [e[1].getValue()]));
        return;
      }

      acc = e[3].getText().replace(".", "");
      if (debits.exists(acc) || credits.exists(acc)) {
        Ui.alert(_args(_("Account %0 is repeated"), [Cts.accFormat(acc)]));
        return;
      }
      switch (Cts.float(e[2].getValue())) {
      case Some(0):
      case Some(n):
        if (acc == "") {
          Ui.alert(_args(_(
            "Account for ammount %0 is missing"), [Std.string(n)]
          ));
          return;
        }
        final cu = new Cu(n);
        credits.set(acc, cu);
        csum += cu.value;
      case None:
        Ui.alert(_args(_("'%0' is not a number"), [e[1].getValue()]));
        return;
      }
    }

    if (!Dec.eq(dsum, csum, 0.00001)) {
      Ui.alert(_("Debits sum is different from Credits sum"));
      return;
    }

    if (dsum == 0) {
      Ui.alert(_("Sums of Debits and Credits are zero"));
      return;
    }

    final dentry = new DiaryEntry(date, desc, debits, credits);

    var ix = ixEdited;
    if (ixEdited == -1) {
      ix = data.acc.addDiary(dentry) + 4;
    } else if (dateEdited != Dt.to(date)) {
      data.acc.diary.splice(ixEdited, 1);
      ix = data.acc.addDiary(dentry) + 4;
    } else {
      data.acc.diary[ixEdited] = dentry;
    }
    final dlen = data.acc.diary.length;
    ixFirstRow = ix >= dlen ? dlen - 1 : ix;

    data.send(() -> {
      editDiv.removeAll();
      listDiv.removeAll().add(list());
    });
  }

  /// View ---------------------------------------------------------------------

  static function entry () {

    var mkWg: Void -> Domo;

    function mkAccEntry (): Domo {
      final i = Q("div");
      i.klass("frame")
      .style(
        "width:48px;color:#000000;background-color:#ffffff;" +
        "text-align:center;vertical-align:middle;"
      )
      .on(DBLCLICK, e -> i.html(""));
      return i;
    }
    function mkAmmountEntry (acc: Domo): Domo {
      final amm = Ui.field("accept")
        .style("width:65px")
        .on(FOCUS, e -> entryAccSel = acc)
      ;
      Ui.changePoint(amm);
      return amm;
    }

    function addEntryRow () {
      final d = mkAccEntry();
      final c = mkAccEntry();
      entryRows.push(
        [d, mkAmmountEntry(d), mkAmmountEntry(c), c]
      );
      editDiv.removeAll().add(mkWg());
    }

    function removeEntryRow () {
      if (entryRows.length > 1) {
        entryRows.splice(entryRows.length - 1, 1);
        editDiv.removeAll().add(mkWg());
      }
    }

    function autoSum () {
      var dsum = 0.0;
      var csum = 0.0;
      for (e in entryRows) {
        for (i in 1...3) {
          switch (Cts.float(e[i].getValue())) {
          case Some(n):
            if (i == 1) dsum += n; else csum += n;
          case None:
            Ui.alert(_args(_("'%0' is not a number"), [e[i].getValue()]));
            return;
          }
        }
      }
      if (csum == dsum) {
        Ui.alert(_("Difference is 0"));
        return;
      }
      var i = 1;
      var v = csum - dsum;
      if (v < 0) {
        ++i;
        v = -v;
      }
      final cu = new Cu(v).toString();

      for (e in entryRows) {
        if (e[i].getValue().trim() == "") {
          e[i].value(cu);
          return;
        }
      }
      addEntryRow();
      entryRows[entryRows.length - 1][i].value(cu);
    }

    final diary = data.acc.diary;
    datePicker = new DatePicker();
    datePicker.action = m -> {};
    if (ixEdited == -1) {
      number.value("");
      datePicker.date = diary.length > 0
        ? diary[diary.length - 1].date
        : Date.now()
      ;
      description.value("");
      final d = mkAccEntry();
      final c = mkAccEntry();
      entryRows = [
        [d, mkAmmountEntry(d), mkAmmountEntry(c), c]
      ];
    } else {
      final dentry = diary[ixEdited];
      number.value(Std.string(ixEdited + 1));
      datePicker.date = dentry.date;
      description.value(dentry.description);
      final ds = dentry.debits;
      final cs = dentry.credits;
      final dkeys = It.fromMap(ds).map(tp -> tp.e1).to();
      final ckeys = It.fromMap(cs).map(tp -> tp.e1).to();
      final dlen = dkeys.length;
      final clen = ckeys.length;
      final mxlen = dlen > clen ? dlen : clen;
      entryRows = It.range(mxlen).map(i -> {
        final d = mkAccEntry().text(i >= dlen ? "" : Cts.accFormat(dkeys[i]));
        final c = mkAccEntry().text(i >= clen ? "" : Cts.accFormat(ckeys[i]));
        return [
          d,
          mkAmmountEntry(d).value(i >= dlen ? "" : ds.get(dkeys[i]).toString()),
          mkAmmountEntry(c).value(i >= clen ? "" : cs.get(ckeys[i]).toString()),
          c
        ];
      }).to();
    }

    mkWg = () -> {
      return Q("table")
        .att("align", "center")
        .klass("frame")
        .add(Q("tr")
          .add(Q("td")
            .att("colspan", 2)
            .style("text-align:left")
            .add(datePicker.mkText(dateField)))
          .add(Q("td")
            .att("colspan", 3)
            .add(Ui.link(e -> autoSum())
              .klass("diary")
              .html("&nbsp;&nbsp;S&nbsp;&nbsp;"))
            .add(Q("span")
              .html(" · "))
            .add(Ui.link(e -> addEntryRow())
              .klass("diary")
              .html("+"))
            .add(Q("span")
              .html(" "))
            .add(Ui.link(ev -> removeEntryRow())
              .klass("diary")
              .html("-")))
          .add(Q("td")
            .att("colspan", 2)
            .style("text-align:right;")
            .add(number)))
        .add(Q("tr")
          .add(Q("td").att("colspan", 7).add(description)))
        .adds(It.from(entryRows).map(e ->
          Q("tr")
            .add(Q("td")
              .style("text-align:left;width:5px")
              .add(e[0]))
            .add(Q("td")
              .att("colspan", 2)
              .style("text-align:left")
              .add(e[1]))
            .add(Q("td")
              .html("||"))
            .add(Q("td")
              .att("colspan", 2)
              .style("text-align:right")
              .add(e[2]))
            .add(Q("td")
              .style("text-align:right")
              .add(e[3])))
              .to())
          .add(Q("tr")
            .add(Q("td")
              .att("colspan", 7)
              .style("text-align:right")
              .add(Q("button")
                .text(_("Cancel"))
                .style("width:100px")
                .on(CLICK, e -> cancelEntry()))
              .add(Q("span")
                .html("&nbsp;&nbsp;"))
              .add(Q("button")
                .text(_("Accept"))
                .style("width:100px")
                .att("id", "accept")
                .on(CLICK, e -> acceptEntry()))))
      ;
    }

    return mkWg();
  }

  static function list () {
    final td = () -> Q("td").klass("frame").style("vertical-align:top;");
    final tdr = () -> td().setStyle("text-align", "right");
    final tdl = () -> td().setStyle("text-align", "left");
    var cut = ixFirstRow + 1;
    return Q("table")
      .att("align", "center")
      .adds(It.from(data.acc.diary)
        .take(cut)
        .drop(cut - Cts.tableLen)
        .reverse()
        .map(e -> {
          cut--;
          final lix = cut;
          final dkeys = It.fromMap(e.debits).map(tp -> tp.e1).to();
          final ckeys = It.fromMap(e.credits).map(tp -> tp.e1).to();
          final dlen = dkeys.length;
          final clen = ckeys.length;
          final n = dlen > clen ? dlen : clen;
          final descDentry = Q("table")
            .att("align", "center")
            .adds(It.range(n).map(i ->
              Q("tr")
                .add(td().add(i < dlen
                  ? Ui.link(e -> goToAcc(dkeys[i], lix))
                    .klass("link")
                    .att("title", data.acc.subaccounts.get(dkeys[i]))
                    .html(Cts.accFormat(dkeys[i]))
                  : Q("span")))
                .add(tdr().add(i < dlen
                  ? Q("span").html(e.debits.get(dkeys[i]).toString())
                  : Q("span")))
                .add(Q("td").html("||"))
                .add(tdr().add(i < clen
                  ? Q("span").html(e.credits.get(ckeys[i]).toString())
                  : Q("span")))
                .add(td().add(i < clen
                  ? Ui.link(ev -> goToAcc(ckeys[i], lix))
                      .klass("link")
                      .att("title", data.acc.subaccounts.get(ckeys[i]))
                      .html(Cts.accFormat(ckeys[i]))
                  : Q("span")))
            ).to());
          descDentry.e.style.display = "none";
          final desc = Q("div")
            .add(Ui.link(e -> {
                descDentry.e.style.display =
                  descDentry.e.style.display == "none"
                    ? "block" : "none";
              }).klass("link").html(e.description))
            .add(descDentry);
          return Q("tr")
            .add(Q("td").add(
              lix == 0
                ? Q("span").add(Ui.lightImg("delete"))
                : Ui.link(ev -> {
                    deleteClick(lix);
                  }).add(Ui.img("delete"))))
            .add(tdr().html("" + (lix + 1)))
            .add(td().add(
              data.conf.isLastYear()
                ? Ui.link(e -> modifyClick(lix))
                  .klass("link")
                  .html(DateTools.format(e.date, "%d/%m"))
                : Q("span")
                  .style("color:#800000;")
                  .html(DateTools.format(e.date, "%d/%m"))))
            .add(tdl().add(desc))
            .add(tdr().html(Dec.toIso(
              It.fromMap(e.debits)
                .reduce(0.0, (s, tp) -> s += tp.e2.value),
              2)))
          ;
        }).to()
      );
  }

  static function left () {
    return new AccountSelector(data.acc, acc, helpAccountClick).wg;
  }

  static function right () {
    final r = Q("td")
      .style("text-align:center;vertical-align:top;")
      .add(Q("div")
        .klass("head")
        .text(_("Diary")))
      .add(Q("div")
        .add(Ui.link(e -> fixUrl())
          .add(Ui.img("pin"))))
      .add(editDiv)
      .add(Q("hr"))
      .add(Q("table")
        .att("align", "center")
        .add(Q("tr")
          .add(Q("td").att("colspan", 2))
          .add(Q("td").att("colspan", 2).klass("diary").add(
            data.conf.isLastYear()
              ? Ui.link(ev -> newClick()).html(_("New"))
              : Q("span").style("color: #800000;").html(_("New"))))
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
          .add(Q("td").att("colspan", 2)))
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
    Diary.wg = wg;
    Diary.data = data;
    Diary.acc = acc == "" || !data.acc.accounts.exists(acc)
      ? Cts.cash.substring(0, 3) : acc;
    final dlen = data.acc.diary.length;
    final ixn = Dec.digits(ix) ? Std.parseInt(ix) : dlen - 1;
    ixFirstRow = ixn >= dlen ? dlen - 1 : ixn;


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
