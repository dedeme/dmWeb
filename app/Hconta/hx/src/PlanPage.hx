// Copyright 28-Jun-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Str;
import dm.Menu;
import I18n._;
import I18n._args;
import data.All;
import data.Balance;
import data.Profits;

/// Plan page.
class PlanPage {
  static var wg: Domo;
  static var data: All;
  static var acc: String;

  static var modId = "";

  static final selectIdDiv = Q("div");
  static var selectId: Domo;
  static final nameField = Ui.field("newBt")
    .att("id", "autofocus")
  ;
  static final groupField = Q("input")
    .att("type", "text")
    .klass("frame")
    .style("width:45px;color:#000000;text-align:center;")
    .disabled(true)
  ;
  static final modIdTd = Q("td")
    .att("colspan", "2");

  // Control -------------------------------------------------------------------

  static function update (): Void {
    final desc = nameField.getValue().trim();
    if (desc == "") {
      Ui.alert(_("Description is missing"));
      return;
    }
    final summary = groupField.getValue().trim();
    if (acc.length == 2 && summary == "") {
      Ui.alert(_("Group is missing"));
      return;
    }
    final id = acc + selectId.getValue();

    final error = modId == ""
    ? switch (acc.length) {
      case 1: data.acc.subgroupAdd(id, desc);
      case 2: data.acc.accountAdd(id, desc, summary);
      default: data.acc.subaccountAdd(id, desc);
      }
    : switch (acc.length) {
      case 1: data.acc.subgroupMod(modId, id, desc);
      case 2: data.acc.accountMod(modId, id, desc, summary);
      default: data.acc.subaccountMod(modId, id, desc);
      }
    ;

    if (error != "") {
      Ui.alert(error);
      return;
    }
    data.send(() -> mk(wg, data, acc));
  }

  static function modifyCancel (): Void {
    modId = "";
    modIdTd
      .removeAll()
      .att("colspan", "2");
    selectId = Ui.select("enterId", data.acc.available(acc, ""));
    selectIdDiv
      .removeAll()
      .add(selectId)
    ;
    nameField.value("");
    groupField.value("");
  }

  static function modifyStart (
    id: String, description: String, summary: String
  ): Void {
    modId = id;
    modIdTd
      .removeAll()
      .att("colspan", "2")
      .add(Ui.link(e -> modifyCancel())
            .add(Ui.img("cancel")));
    final idKey = id.substring(acc.length);
    selectId = Ui.select("enterId", data.acc.available(acc, idKey));
    selectId.value(idKey);
    selectIdDiv
      .removeAll()
      .add(selectId)
    ;
    nameField.value(description);
    groupField.value(summary);
  }

  static function delete (id: String, description: String): Void {
    if (!Ui.confirm(_args(_("Delete '%0'?"), [id + " - " + description]))) {
      return;
    }
    final error = switch (acc.length) {
    case 1: data.acc.subgroupDeletable(id);
    case 2: data.acc.accountDeletable(id);
    default: data.acc.subaccountDeletable(id);
    };
    if (error != "") {
      Ui.alert(error);
      return;
    }
    switch (acc.length) {
    case 1: data.acc.subgroupDel(id);
    case 2: data.acc.accountDel(id);
    default: data.acc.subaccountDel(id);
    }
    data.send(() -> mk(wg, data, acc));
  }

  /// View ---------------------------------------------------------------------

  static function left () {
    function mkBalance () {
      final r = [];
      for (gkey => gname in Balance.groups()) {
        final sub = [];
        for (ekey => ename in Balance.entries()) {
          if (Balance.groupOf(ekey) == gkey) {
            sub.push(Q("li")
              .add(Ui.link(e -> groupField.value("B" + ekey))
                .klass("link")
                .att("title", "B" + ekey)
                .html(Str.cutRight(ename, 45)))
            );
          }
        }
        r.push(Q("li")
          .html("<a href='#' onclick='return false;'>" + gname + "</a>")
          .add(Q("ul").att("id", "hlist")
            .style("list-style:none;padding-left:10px;")
            .adds(sub))
        );
      }
      return r;
    }

    function mkProfits () {
      final r = [];
      for (gkey => gname in Profits.groups()) {
        final sub = [];
        for (ekey => ename in Profits.entries()) {
          if (Profits.groupOf(ekey) == gkey) {
            sub.push(Q("li")
              .add(Ui.link(e -> groupField.value("P" + ekey))
                .klass("link")
                .att("title", "P" + ekey)
                .html(Str.cutRight(ename, 45)))
            );
          }
        }
        r.push(Q("li")
          .html("<a href='#' onclick='return false;'>" + gname + "</a>")
          .add(Q("ul").att("id", "hlist")
            .style("list-style:none;padding-left:10px;")
            .adds(sub))
        );
      }
      return r;
    }

    return Q("td")
      .klass("frame")
      .style("width:350px;vertical-align:top;")
        .add(Q("p")
          .html("<b>Balance</b>"))
        .add(Q("ul")
          .style("list-style:none;padding-left:0px;")
          .adds(mkBalance()))
        .add(Q("p")
          .html("<b>PyG</b>"))
        .add(Q("ul")
          .style("list-style:none;padding-left:0px;")
          .adds(mkProfits()))
    ;
  }

  static function right () {
    var title = _("Groups");
    switch (acc.length) {
    case 1:
      title = _("Subgroups");
    case 2:
      title = _("Accounts");
    case 3:
      title = _ ("Subaccounts");
    }

    function mkSubmenu () {
      final separator = () -> Q("span").text("|");
      final entry = (tx, lk) ->
        Ui.link(e -> js.Browser.location.assign("?plan&" + lk))
          .klass("link")
          .text(" " + tx + " ")
      ;

      var es = [separator(), entry("*", ""), separator()];
      final add = (tx, lk) -> {
        es.push(entry(tx, lk));
        es.push(separator());
      }
      if (acc.length > 0) add(acc.charAt(0), acc.charAt(0));
      if (acc.length > 1) add(acc.charAt(1), acc.substring(0, 2));
      if (acc.length > 2) add(acc.charAt(2), acc);
      return Q("p").adds(es);
    }

    nameField.value("");
    groupField.value("");
    selectId = Ui.select("enterId", data.acc.available(acc, ""));
    selectIdDiv
      .removeAll()
      .add(selectId)
    ;
    if (!data.conf.isLastYear()) {
      nameField.disabled(true);
      selectId.disabled(true);
    }

    final rows = [];
    var cols = 2;
    if (acc != "") cols += 2;
    if (acc.length == 2) ++cols;

    // Edition row
    if (acc != "") {
      final tds = [
        Q("td")
          .att("colspan", 2)
          .add(Q("button")
            .style("border:1px solid #a0a9ae;padding:0px;width:32px;")
            .att("id", "newBt")
              .add(Ui.img("enter")
                .att("style", "padding:0px;margin:0px;vertical-align:-20%"))
            .disabled(!data.conf.isLastYear())
            .on(CLICK, e -> update())),
        Q("td")
          .add(selectIdDiv),
        Q("td")
          .add(nameField)
      ];
      if (acc.length == 2) tds.push(Q("td").add(groupField));

      rows.push(Q("tr").adds(tds));
      rows.push(Q("tr")
        .add(Q("td")
          .att("colspan", cols)
          .add(Q("hr")))
      );
    }

    // Head row
    final tds = [];
    if (acc != "") {
      tds.push(modIdTd);
    }
    tds.push(Q("td")
      .html(_("Nº")));
    tds.push(Q("td")
      .style("text-align:left;").html(_("Description")));
    if (acc.length == 2) {
      tds.push(Q("td")
        .html(_("Group")));
    }
    rows.push(Q("tr").adds(tds));
    rows.push(Q("tr")
      .add(Q("td")
        .att("colspan", cols)
        .add(Q("hr")))
    );

    // Body rows
    for (k => v in data.acc.sub(acc)) {
      final tds = [];
      if (acc != "") {
        if (
          k == Cts.cash.substring(0, 2) ||
          k == Cts.cash.substring(0, 3) ||
          k == Cts.cash ||
          k == Cts.capital.substring(0, 2) ||
          k == Cts.capital.substring(0, 3) ||
          k == Cts.capital ||
          k == Cts.results.substring(0, 2) ||
          k == Cts.results.substring(0, 3) ||
          k == Cts.results ||
          !data.conf.isLastYear()
        ) {
          tds.push(Q("td").add(Ui.lightImg("edit")));
          tds.push(Q("td").add(Ui.lightImg("delete")));
        } else {
          tds.push(Q("td")
            .add(Ui.link(e -> modifyStart(k, v.description, v.summary))
              .add(Ui.img("edit"))));
          tds.push(Q("td")
            .add(Ui.link(e -> delete(k, v.description))
              .add(Ui.img("delete"))));
        }
      }
      tds.push(Q("td")
        .style("text-align:right;")
        .text(k.substring(acc.length))
      );
      tds.push(Q("td")
        .style("text-align:left;")
        .add(acc.length < 3
          ? Ui.link(e -> js.Browser.location.assign("?plan&" + k))
            .klass("link")
            .add(Q("span").text(v.description))
          : Q("span").text(v.description))
      );
      if (acc.length == 2) {
        tds.push(Q("td")
          .style("text-align:left;")
          .text(v.summary)
        );
      }
      rows.push(Q("tr").adds(tds));
    }

    return Q("td")
      .style("text-align:center;vertical-align:top;")
      .add(Q("div")
        .klass("head")
        .html(title))
      .add(mkSubmenu())
      .add(Q("table")
        .att("align", "center")
        .klass("frame")
        .adds(rows))
    ;
  }

  /// Constructor.
  public static function mk (wg: Domo, data: All, acc: String): Void {
    if (data.acc.descriptionOf(acc) == "" || acc.length > 3) acc = "";
    PlanPage.wg = wg;
    PlanPage.data = data;
    PlanPage.acc = acc;

    wg
      .removeAll()
      .add(Q("table")
        .klass("main")
        .add(Q("tr")
          .add(acc.length == 2 && data.conf.isLastYear() ? left() : Q("div"))
          .add(right())))
    ;
  }

}
