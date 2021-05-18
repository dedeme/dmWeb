// Copyright 16-Aug-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package wgs;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Dt;
import I18n._;
import data.LogRow;

/// Log widget.
class Log {
  static final lineWidth = 120;
  static var wg: Domo;
  static var load: (Array<LogRow> -> Void) -> Void;
  static var reset: (() -> Void) -> Void;

  static var is2Days: Bool;
  static var isErrors: Bool;
  static var rows: Array<LogRow>;

  // View ----------------------------------------------------------------------

  public static function mk (
    wg: Domo,
    load: (Array<LogRow> -> Void) -> Void,
    reset: (() -> Void) -> Void
  ) {
    Log.wg = wg;
    Log.load = load;
    Log.reset = reset;
    is2Days = true;
    isErrors = true;

    load(rows -> {
      Log.rows = rows;
      view();
    });
  }

  static function view () {
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

    final lmenu = Q("div");
    final rmenu = Q("div");
    final area = Q("textarea").att("spellcheck", false)
      .att("readOnly", true)
      .att("rows", 25).att("cols", lineWidth + 5);

    lmenu
      .add(Q("span")
        .add(mkOption(
          false, is2Days, _("2 Days"), () -> on2Days()
        )))
      .add(Q("span").html(" · "))
      .add(Q("span")
        .add(mkOption(
          false, !is2Days, _("All"), () -> onAllD()
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
    final log = rows.copy();
    log.reverse();
    area.value(
      log
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

  // Control -------------------------------------------------------------------

  static function on2Days () {
    is2Days = true;
    view();
  }

  static function onAllD () {
    is2Days = false;
    view();
  }

  static function onReload () {
    load(rows -> {
      Log.rows = rows;
      view();
    });
  }

  static function onDelete () {
    if (Ui.confirm(_("All log entries will be deleted.\nContinue?"))) {
      reset(() -> {
        rows = [];
        view();
      });
    }
  }

  static function onErrors () {
    isErrors = true;
    view();
  }

  static function onAll () {
    isErrors = false;
    view();
  }

}
