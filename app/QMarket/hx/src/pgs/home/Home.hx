// Copyright 16-Oct-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pgs.home;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Js;
import dm.Dt;
import data.LogRow;
import data.Cts;
import I18n._;

/// Home page.
class Home {
  static final lineWidth = 120;
  final wg: Domo;
  final log: Array<LogRow>;
  var is2Days: Bool;
  var isErrors: Bool;

  function new (wg: Domo, log: Array<LogRow>) {
    this.wg = wg;
    this.log = log;

    is2Days = true;
    isErrors = true;

    view();
  }

  // view ----------------------------------------------------------------------

  function view (): Void {
    final lmenu = Q("div");
    final rmenu = Q("div");
    final area = Q("textarea")
      .att("spellcheck", false)
      .att("readOnly", true)
      .att("rows", 25)
      .att("cols", lineWidth + 5)
    ;

    lmenu
      .add(Q("span")
        .add(mkOption(
          false, is2Days, _("2 Days"), on2Days
        )))
      .add(Q("span").html(" · "))
      .add(Q("span")
        .add(mkOption(
          false, !is2Days, _("All"), () -> onAllDays()
        )))
    ;

    rmenu
      .add(Q("span")
        .add(mkOption(
          false, false, _("Reload"), () -> onReload()
        )))
      .add(Q("span").html(" · "))
      .add(Q("span")
        .add(mkOption(
          false, false, _("Delete"), () -> onDelete()
        )))
      .add(Q("span").html(" | "))
      .add(Q("span")
        .add(mkOption(
          false, isErrors, _("Errors"), () -> onErrors()
        )))
      .add(Q("span").html(" · "))
      .add(Q("span")
        .add(mkOption(
          false, !isErrors, _("All"), () -> onAll()
        )))
    ;

    final today = Date.now();
    final lg = log.copy();
    lg.reverse();
    area.value(
      lg
        .filter(e ->
          (is2Days ? Dt.df(today, e.date) < 3 : true) &&
          (isErrors ? e.isError : true)
        )
        .map(e -> e.format(lineWidth))
        .join("\n")
    );

    wg
      .removeAll()
      .add(Q("div").klass("head").style("padding-bottom:10px").text(_("Log")))
      .add(Q("table").att("align", "center").klass("frame3")
        .add(Q("tr")
          .add(Q("td").style("text-align:left")
            .add(lmenu))
          .add(Q("td").style("text-align:right")
            .add(rmenu)))
        .add(Q("tr").add(Q("td").att("colspan", 2)))
        .add(Q("tr")
          .add(Q("td").att("colspan", 2).add(area))))
    ;
  }

  function mkOption (
    isImg: Bool, isSel: Bool, id: String, action: () -> Void
  ): Domo {
    if (isImg) {
      final r = Ui.link(e -> action());
      if (isSel) r.klass("frame");
      return r.add(Ui.img(id).style("vertical-align:top"));
    }
    var r = Q("span").klass("frame");
    if (!isSel) r = Ui.link(e -> action()).klass("link");
    return r.text(id);
  }

  // control -------------------------------------------------------------------

  function on2Days (): Void {
    is2Days = true;
    view();
  }

  function onAllDays (): Void {
    is2Days = false;
    view();
  }

  function onReload () {
    mk(wg);
  }

  function onDelete () {
    if (Ui.confirm(_("All log entries will be deleted.\nContinue?"))) {
      Cts.client.send([
        "module" => Js.ws("home"),
        "rq" => Js.ws("reset")
      ], rp -> {
        mk(wg);
      });
    }
  }

  function onErrors () {
    isErrors = true;
    view();
  }

  function onAll () {
    isErrors = false;
    view();
  }

  // static --------------------------------------------------------------------

  /// Constructor.
  ///   wg: Container.
  public static function mk (wg: Domo): Void {
    Cts.client.send([
      "module" => Js.ws("home"),
      "rq" => Js.ws("log")
    ], rp -> {
      final log = rp["log"].ra().map(e -> LogRow.fromJs(e));
      new Home(wg, log);
    });
  }
}
