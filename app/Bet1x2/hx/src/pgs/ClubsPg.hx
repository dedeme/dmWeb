// Copyright 17-Sep-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pgs;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Js;
import dm.It;
import data.Club;
import I18n._;
import I18n._args;

/// Clubs page.
class ClubsPg {
  final wg: Domo;
  final selectedYear: String;
  final allClubs: Array<Club>;
  final clubs: Array<Club>;

  var modifyId = "";

  function new (
    wg: Domo, selectedYear: String, allClubs: Array<Club>, clubs: Array<Club>
  ) {
    this.wg = wg;
    this.selectedYear = selectedYear;
    this.allClubs = allClubs;
    this.clubs = clubs;

    view ();
  }

  // View ----------------------------------------------------------------------

  function view (): Void {
    final entryWg = Q("div");
    entry(entryWg);

    final selectorWg = Q("div");
    selector(selectorWg);

    wg
      .removeAll()
      .add(entryWg)
      .add(selectorWg)
    ;
  }

  function entry (w: Domo): Void {
    final idWg = modifyId == ""
      ? Ui.field("name").att("id", "id")
      : Q("div").klass("frame").text(modifyId)
    ;

    final nameWg = Ui.field("id").att("id", "name");
    if (modifyId != "") nameWg.value(Club.getName(allClubs, modifyId));

    final link = modifyId == ""
      ? Ui.link(e -> newClub(idWg.getValue(), nameWg.getValue()))
        .klass("link")
        .text(_("Add"))
      : Ui.link(e -> modifyClub(nameWg.getValue()))
        .klass("link")
        .text(_("Modify"))
    ;

    w
      .removeAll()
      .add(Q("table")
        .att("align", "center")
        .add(Q("tr")
          .add(Q("td")
            .add(link))
          .add(Q("td")
            .add(idWg))
          .add(Q("td")
            .add(nameWg))))
    ;
  }

  function selector (w: Domo): Void {
    final allClubsDiv = Q("div");
    allClubsWg(allClubsDiv);

    final clubsDiv = Q("div");
    clubsWg(clubsDiv);

    w
      .removeAll()
      .add(Q("table")
        .att("align", "center")
        .add(Q("tr")
          .add(Q("td")
            .style("text-align:center")
            .html("<b>" + _("All Clubs") + "</b>"))
          .add(Q("td")
            .style("text-align:center")
            .html("<b>" + _("Year Clubs") + "</b>")))
        .add(Q("tr")
          .add(Q("td")
            .klass("frame")
            .style("vertical-align:top")
            .add(allClubsDiv))
          .add(Q("td")
            .klass("frame")
            .style("vertical-align:top")
            .add(clubsDiv))))
    ;
  }

  function allClubsWg (w: Domo): Void {
    function entry (id: String): Domo {
      return Q("tr")
        .add(Q("td")
          .add(modifyId == ""
            ? Ui.link(() -> delAllClubs(id))
              .add(Ui.img("delete"))
            : Ui.lightImg("delete")))
        .add(Q("td")
          .add(modifyId == ""
            ? Ui.link(() -> modifyAllClubs(id))
              .add(Ui.img("edit"))
            : Ui.lightImg("edit")))
        .add(Q("td")
          .klass("frameTx")
          .add(Q("span")
            .add(Ui.img("clubs/" + id)
            .style("height:32px;vertical-align:middle")))
          .add(Q("span")
            .text(Club.getName(allClubs, id))))
        .add(Q("td")
          .add(modifyId == ""
            ? Ui.link(() -> selectAllClubs(id))
              .add(Ui.img("add"))
            : Ui.lightImg("add")))
      ;
    }

    w
      .removeAll()
      .add(Q("table")
        .klass("border")
        .adds(allClubs.length == 0
          ? [ Q("tr")
              .add(Q("td")
                .klass("frame")
                .text(_("Without clubs")))
            ]
          : It.from(allClubs)
            .filter(c ->
                !It.from(clubs).contains(c, (c1, c2) -> c1.id == c2.id)
              )
            .sort((c1, c2) -> dm.Str.compare(c1.id, c2.id))
            .map(c -> entry(c.id))
            .to()))
    ;
  }

  function clubsWg (w: Domo): Void {
    function entry (id: String): Domo {
      return Q("tr")
        .add(Q("td")
          .add(modifyId == ""
            ? Ui.link(() -> delClubs(id))
              .add(Ui.img("remove"))
            : Ui.lightImg("remove")))
        .add(Q("td")
          .klass("frameTx")
          .add(Q("span")
            .add(Ui.img("clubs/" + id)
            .style("height:32px;vertical-align:middle")))
          .add(Q("span")
            .text(Club.getName(allClubs, id))))
      ;
    }

    w
      .removeAll()
      .add(Q("table")
        .klass("border")
        .adds(clubs.length == 0
          ? [ Q("tr")
              .add(Q("td")
                .klass("frame")
                .text(_("Without clubs")))
            ]
          : clubs.map(c -> entry(c.id))))
    ;
  }

  // Control -------------------------------------------------------------------

  function updateAllClubs (fn: Void -> Void): Void {
    Cts.client.ssend([
      "source" => Js.ws("Clubs"),
      "rq" => Js.ws("updateAllClubs"),
      "allClubs" => Js.wa(allClubs.map(c -> c.toJs()))
    ], rp -> {
      fn();
    });
  }

  function updateClubs (fn: Void -> Void): Void {
    Cts.client.ssend([
      "source" => Js.ws("Clubs"),
      "rq" => Js.ws("updateClubs"),
      "year" => Js.ws(selectedYear),
      "clubs" => Js.wa(clubs.map(c -> c.toJs()))
    ], rp -> {
      fn();
    });
  }

  function validateEntry (e: String, ?isId = false): String {
    if (e == "") {
      if (isId) return _("Identifier is missing");
      return _("Name is missing");
    }
    if (isId && Club.getName(allClubs, e) != "") {
      return _("Identifier duplicated");
    }
    return "";
  }

  function newClub(id: String, name: String): Void {
    var err = validateEntry(id, true);
    if (err != "") {
      Ui.alert(err);
      return;
    }
    err = validateEntry(name);
    if (err != "") {
      Ui.alert(err);
      return;
    }

    allClubs.push(new Club(id, name));

    updateAllClubs(() -> mk(wg, selectedYear));
  }

  function modifyClub(name: String): Void {
    final err = validateEntry(name);
    if (err != "") {
      Ui.alert(err);
      return;
    }

    for (i in 0...allClubs.length) {
      final cid = allClubs[i].id;
      if (cid == modifyId) {
        allClubs[i] = new Club(modifyId, name);
        break;
      }
    }

    updateAllClubs(() -> mk(wg, selectedYear));
  }

  function delAllClubs(id: String): Void {
    final ix = It.from(allClubs).indexf(c -> c.id == id);
    if (ix == -1) {
      view();
    }
    if (!Ui.confirm(_args(_("Delete '%0'?"), [allClubs[ix].name]))) {
      return;
    }

    allClubs.splice(ix, 1);

    updateAllClubs(() -> mk(wg, selectedYear));
  }

  function modifyAllClubs(id: String): Void {
    modifyId = id;
    view();
    js.Browser.window.scroll(0, 0);
  }

  function selectAllClubs(id: String): Void {
    final ix = It.from(allClubs).indexf(c -> c.id == id);
    if (ix != -1) {
      clubs.push(allClubs[ix]);
      updateClubs(() -> mk(wg, selectedYear));
    }
  }

  function delClubs(id: String): Void {
    final ix = It.from(clubs).indexf(c -> c.id == id);
    if (ix == -1) {
      view();
    }
    if (!Ui.confirm(_args(_("Quit '%0'?"), [clubs[ix].name]))) {
      return;
    }

    clubs.splice(ix, 1);

    updateClubs(() -> mk(wg, selectedYear));
  }

  // Static --------------------------------------------------------------------

  /// Constructor.
  ///   wg          : Widget.
  ///   selectedYear: Selected year.
  public static function mk (wg: Domo, selectedYear: String): Void {
    Cts.client.send([
      "source" => Js.ws("Clubs"),
      "rq" => Js.ws("idata"),
      "year" => Js.ws(selectedYear)
    ], rp -> {
      final allClubs = rp["allClubs"].ra().map(e -> Club.fromJs(e));
      final clubs = rp["clubs"].ra().map(e -> Club.fromJs(e));
      new ClubsPg(wg, selectedYear, allClubs, clubs);
    });
  }

}
