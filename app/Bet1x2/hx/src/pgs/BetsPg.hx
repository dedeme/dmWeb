// Copyright 18-Sep-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pgs;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Js;
import dm.It;
import dm.Dt;
import dm.Opt;
import dm.Dec;
import dm.DatePicker;
import data.Club;
import data.Matchday;
import data.Match;
import data.Bet;
import wgs.NumberField;
import I18n._;
import I18n._args;

/// Bets page.
class BetsPg {
  final wg: Domo;
  final selectedYear: String;
  final clubs: Array<Club>;
  final matchdays: Array<Matchday>;
  final maxMatches: Int;

  var matchdayIx = 0;

  function new (
    wg: Domo, selectedYear: String,
    clubs: Array<Club>, matchdays: Array<Matchday>
  ) {
    this.wg = wg;
    this.selectedYear = selectedYear;
    this.clubs = clubs;
    this.matchdays = matchdays;
    maxMatches = (clubs.length - 1) * 2;

    view ();
  }

  // View ----------------------------------------------------------------------

  function view (): Void {
    wg
      .removeAll()
      .add(Q("table")
        .att("align", "center")
        .add(Q("tr")
          .add(Q("td")
            .att("colspan", "3")
            .style("text-align:center")
            .html("<b>" + _("Matchday") + "</b>"))
          .add(Q("td")
            .style("text-align:center")
            .html("<b>" + _("Bets") + "</b>")))
        .add(Q("tr")
          .add(Q("td")
            .att("colspan", "3")
            .style("white-space:nowrap")
            .add(matchdays.length >= maxMatches
              ? Q("hr")
              : Ui.link(addMatchday)
                .klass("link")
                .text(_args(
                    _("Add matchday %0"), ["" + (matchdays.length + 1)]
                  ))))
          .add(Q("td")
            .add(Q("hr"))))
        .adds(matchdays.length == 0
            ? [
                Q("tr")
                  .add(Q("td")
                    .klass("frame")
                    .style("text-align:center")
                    .att("colspan", "4")
                    .text(_("Without matchdays")))
              ]
            : It.range(matchdays.length)
              .map(i -> matchdayEntry(i))
              .to())
          )
    ;
  }

  function matchdayEntry (ix:Int): Domo {
    final deleteWg = Q("div")
      .add(Ui.lightImg("delete"))
    ;
    final completeWg = Q("div")
      .add(Ui.img(matchdays[ix].matches.length == 0
        ? "cross"
        : matchdays[ix].isComplete() ? "ok" : "warning"))
    ;
    final matchsWg = Q("div");

    if (ix == matchdayIx) {
      new MatchsWg(
        matchsWg, deleteWg, completeWg,
        clubs, matchdays[ix], ix,
        delMatchday,
        updateMatchdays
      );
    }

    final m = matchdays[ix];
    return Q("tr")
      .add(Q("td")
        .style("width:5px")
        .add(deleteWg))
      .add(Q("td")
        .klass("frame")
        .style("text-align:center")
        .add(ix == matchdayIx
          ? Q("span")
            .text("" + m.number)
          : Ui.link(() -> changeMatchIx(ix))
            .klass("link")
            .text("" + m.number)))
      .add(Q("td")
        .style("width:5px")
        .add(completeWg))
      .add(Q("td")
        .klass("frame")
        .add(matchsWg))
    ;
  }

  // Control -------------------------------------------------------------------

  function updateMatchdays (fn: Void -> Void): Void {
    matchdays.sort((m1, m2) -> m2.number - m1.number);
    Cts.client.ssend([
      "source" => Js.ws("Bets"),
      "rq" => Js.ws("update"),
      "year" => Js.ws(selectedYear),
      "matchdays" => Js.wa(matchdays.map(m -> m.toJs()))
    ], rp -> {
      fn();
    });
  }

  function validateDate (d: Date): String {
    return "" + d.getFullYear() == selectedYear
      ? ""
      : _("Date does not match to the selected year")
    ;
  }

  function addMatchday (): Void {
    matchdays.push(new Matchday(matchdays.length + 1));

    updateMatchdays(() -> mk(wg, selectedYear));
  }

  function delMatchday (ix: Int): Void {
    final d = matchdays[ix];
    if (!Ui.confirm(_args(_("Delete '%0'?"), ["" + d.number]))) {
      return;
    }

    matchdays.splice(ix, 1);

    updateMatchdays(() -> mk(wg, selectedYear));
  }

  function changeMatchIx (ix: Int): Void {
    if (ix >= matchdays.length || ix < 0) ix = 0;
    matchdayIx = ix;
    view();
  }

  // Static --------------------------------------------------------------------

  /// Constructor.
  ///   wg          : Widget.
  ///   selectedYear: Selected year.
  public static function mk (wg: Domo, selectedYear: String): Void {
    Cts.client.send([
      "source" => Js.ws("Bets"),
      "rq" => Js.ws("idata"),
      "year" => Js.ws(selectedYear)
    ], rp -> {
      final clubs = rp["clubs"].ra().map(e -> Club.fromJs(e));
      final matchdays = rp["matchdays"].ra().map(e -> Matchday.fromJs(e));
      new BetsPg(wg, selectedYear, clubs, matchdays);
    });
  }

}

// MatchsWg ====================================================================
// =============================================================================

private class MatchsWg {
  final rsOpts = ["", "1", "x", "2"];

  final w: Domo;
  final deleteWg: Domo;
  final completeWg: Domo;
  final clubs: Array<Club>;
  final matchday: Matchday;
  final matchdayIx: Int;
  final deleteMatchday: Int -> Void;
  final updateMatchdays: (Void -> Void) -> Void;

  public function new (
    w: Domo, deleteWg: Domo, completeWg: Domo,
    clubs: Array<Club>, matchday: Matchday, matchdayIx: Int,
    deleteMatchday: Int -> Void,
    updateMatchdays: (Void -> Void) -> Void
  ) {
    this.w = w;
    this.deleteWg = deleteWg;
    this.completeWg = completeWg;
    this.clubs = clubs;
    this.matchday = matchday;
    this.matchdayIx = matchdayIx;
    this.deleteMatchday = deleteMatchday;
    this.updateMatchdays = updateMatchdays;

    view();
  }

  // View ----------------------------------------------------------------------

  function view (): Void {
    final matches = matchday.matches;
    final cls = It.from(clubs)
      .filter(c -> {
        var exists = false;
        for (m in matches) {
          if (c.id == m.home.id || c.id == m.away.id) {
            exists = true;
            break;
          }
        }

        return !exists;
      })
      .sort((c1, c2) ->
        dm.Str.compare(c1.id.toUpperCase(), c2.id.toUpperCase())
      )
      .to();

    final trs = [];

    trs.push(Q("tr")
      .add(Q("td")
        .att("colspan", "7"))
      .add(Q("td")
        .att("colspan", "4")
        .klass("border")
        .style("text-align:center")
        .text(_("Payments")))
      .add(Q("td")
        .att("colspan", "3"))
    );

    trs.push(Q("tr")
      .add(Q("td"))
      .add(Q("td"))
      .add(Q("td")
        .att("colspan", "3")
        .style("text-align:center")
        .text(_("Match")))
      .add(Q("td")
        .style("text-align:center")
        .text(_("Bet.")))
      .add(Q("td")
        .style("text-align:center")
        .text(_("Res.")))
      .add(Q("td")
        .klass("border")
        .style("text-align:center")
        .text("1"))
      .add(Q("td")
        .klass("border")
        .style("text-align:center")
        .text("x"))
      .add(Q("td")
        .klass("border")
        .style("text-align:center")
        .text("2"))
      .add(Q("td")
        .klass("border")
        .style("text-align:center")
        .add(Ui.img("insert")))
      .add(Q("td")
        .style("text-align:center")
        .text(_("Date")))
      .add(Q("td")
        .style("text-align:center")
        .text(_("Fees.")))
      .add(Q("td")
        .style("text-align:center")
        .text(_("Prof.")))
    );

    for (i in 0...matches.length) {
      trs.push(mkTr(i));
    }

    if (cls.length > 0) {
      final selHome = Ui.select(
        "home",
        It.unary("+").cat(It.from(cls).map(c -> c.id)).to()
      );

      final selAway = Ui.select(
        "away",
        It.unary("+").cat(It.from(cls).map(c -> c.id)).to()
      );

      trs.push(Q("tr")
        .add(Q("td")
          .att("colspan", "10")
          .style("padding-top:5px")
          .text(" ")));

      trs.push(Q("tr")
        .add(Q("td"))
        .add(Q("td")
          .add(Ui.link(e -> addMatch(
              cls, Cts.selectedIndex(selHome), Cts.selectedIndex(selAway)
            ))
            .add(Ui.img("add"))))
        .add(selHome)
        .add(Q("td")
          .text(" - "))
        .add(selAway)
        .add(Q("td")
          .att("colspan", "6"))
      );
    }

    w
      .removeAll()
      .add(Q("table")
        .klass("summary")
        .adds(trs))
    ;

    deleteWg
      .removeAll()
      .add(matchday.matches.length == 0 && matchdayIx == 0
        ? Ui.link(e -> deleteMatchday(matchdayIx))
           .add(Ui.img("delete"))
        : Ui.lightImg("delete"))
    ;

    completeWg
      .removeAll()
      .add(Ui.img(matchday.matches.length == 0
        ? "cross"
        : matchday.isComplete() ? "ok" : "warning"))
    ;
  }


  function mkTr (i: Int) {
    final today = Dt.to(Date.now());
    final m = matchday.matches[i];

    final selBet = Ui.select(
      "res_" + i,
      switch (m.bet.bet) {
        case None: rsOpts.map(r -> r == "" ? "+" : r);
        case Some(rs): rsOpts
          .map(r -> r == Bet.resultToStr(rs) ? "+" + r : r);
      });
    selBet.on(CHANGE, e -> updateBet(i, Cts.selectedIndex(selBet)));

    final selResult = Ui.select(
      "res_" + i,
      switch (m.bet.result) {
        case None: rsOpts.map(r -> r == "" ? "+" : r);
        case Some(rs): rsOpts
          .map(r -> r == Bet.resultToStr(rs) ? "+" + r : r);
      });
    selResult.on(CHANGE, e -> updateResult(i, Cts.selectedIndex(selResult)));

    final in1 = new NumberField(
      "in_" + i + "1", "in_" + i + "x",
      switch (m.bet.pay1) { case None: 0; case Some(v): v; },
      (id, value) -> {}
    );

    final inx = new NumberField(
      "in_" + i + "x", "in_" + i + "2",
      switch (m.bet.payx) { case None: 0; case Some(v): v; },
      (id, value) -> {}
    );

    final in2 = new NumberField(
      "in_" + i + "2", "bt_" + i,
      switch (m.bet.pay2) { case None: 0; case Some(v): v; },
      (id, value) -> {}
    );

    final dtInput = Q("input").style("width:80px");
    final dt = new DatePicker();
    dt.lang = Storage.getLang();
    dt.action = s -> {
      changeDate(i, s == "" ? None : Some(dt.date));
    }
    dt.date = m.bet.date != None ? Opt.get(m.bet.date) : dt.date;
    final dtWg = dt.mkText(dtInput);
    switch (m.bet.date) {
      case None: dtInput.value("");
      case Some(d): {
        final ds = Dt.to(d);
          dtInput.setStyle("color",
            ds == today
              ? switch (m.bet.bet) { case None: "#800000"; default: "#008080"; }
              : ds < today
                ? switch (m.bet.result) {
                    case None: "#ff9999";
                    default: "#d9d9d9";
                  }
                : "#000020"
          );
      }
    }

    final feesTx = switch (m.bet.fees()) {
        case None: ""; case Some(am): Dec.toIso(am, 2);
    }

    final profitsTx = switch (m.bet.profits()) {
      case None: switch (m.bet.profits(true)) {
          case None: ""; case Some(am): Dec.toIso(am, 2);
        }
      case Some(am): Dec.toIso(am, 2);
    }

    return Q("tr")
      .add(Q("td")
        .add(Ui.img(m.bet.isComplete() ? "ok" : "warning")))
      .add(Q("td")
        .add(Ui.link(() -> delMatch(i))
          .add(Ui.img("delete"))))
      .add(Q("td")
        .klass("frameTx")
        .style("white-space:nowrap")
        .add(Q("span")
          .add(Ui.img("clubs/" + m.home.id)
          .style("height:32px;vertical-align:middle")))
        .add(Q("span")
          .text(Club.getName(clubs, m.home.id))))
      .add(Q("td")
        .text(" - "))
      .add(Q("td")
        .klass("frameTx")
        .style("white-space:nowrap")
        .add(Q("span")
          .add(Ui.img("clubs/" + m.away.id)
          .style("height:32px;vertical-align:middle")))
        .add(Q("span")
          .text(Club.getName(clubs, m.away.id))))
      .add(Q("td")
        .add(selBet))
      .add(Q("td")
        .add(selResult))
      .add(Q("td")
        .add(in1.wg.style("width:40px")))
      .add(Q("td")
        .add(inx.wg.style("width:40px")))
      .add(Q("td")
        .add(in2.wg.style("width:40px")))
      .add(Q("td")
        .add(Q("button")
          .att("id", "bt_" + i)
          .html("&nbsp;")
          .on(CLICK, e -> updatePayment(i, in1, inx, in2))))
      .add(Q("td")
        .add(dtWg))
      .add(Q("td")
        .klass("frameNm")
        .text(feesTx))
      .add(Q("td")
        .klass("frameNm")
        .style(m.bet.profits() == None ? "color:#008080" : "")
        .text(profitsTx))
    ;
  }

  // Control -------------------------------------------------------------------

  function addMatch (cls: Array<Club>, homeIx: Int , awayIx: Int): Void {
    if (homeIx < 1) {
      Ui.alert(_("Home team is missing"));
      return;
    }
    if (awayIx < 1) {
      Ui.alert(_("Away team is missing"));
      return;
    }

    final home = cls[homeIx - 1];
    final away = cls[awayIx - 1];

    if (home.id == away.id) {
      Ui.alert(_("Home and away teams are the same"));
      return;
    }

    matchday.matches.push(new Match(home, away));

    updateMatchdays(view);
  }

  function delMatch (matchIx: Int): Void {
    final ms = matchday.matches;
    final m = ms[matchIx];
    if (!Ui.confirm(_args(
      _("Delete '%0'?"), [m.home.name + " - " + m.away.name]
    ))) {
      return;
    }

    ms.splice(matchIx, 1);

    updateMatchdays(view);
  }

  function updateResult (matchIx: Int, selection: Int): Void {
    final sel = rsOpts[selection];

    matchday.matches[matchIx].bet.result = sel == ""
      ? None
      : Some(Bet.resultFromStr(sel))
    ;

    updateMatchdays(view);
  }

  function updateBet (matchIx: Int, selection: Int): Void {
    final sel = rsOpts[selection];

    matchday.matches[matchIx].bet.bet = sel == ""
      ? None
      : Some(Bet.resultFromStr(sel))
    ;

    updateMatchdays(view);
  }

  function updatePayment (
    matchIx: Int, in1: NumberField, inx: NumberField, in2: NumberField
  ): Void {
    final v1 = in1.value;
    final vx = inx.value;
    final v2 = in2.value;

    final bet = matchday.matches[matchIx].bet;
    bet.pay1 = v1 <= 0 ? None : Some(v1);
    bet.payx = vx <= 0 ? None : Some(vx);
    bet.pay2 = v2 <= 0 ? None : Some(v2);

    updateMatchdays(view);
  }

  function changeDate (matchIx: Int, date: Option<Date>): Void {
    matchday.matches[matchIx].bet.date = date;
    updateMatchdays(view);
  }
}
