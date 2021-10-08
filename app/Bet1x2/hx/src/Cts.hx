// Copyright 14-Sep-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

using StringTools;

import dm.Client;
import dm.Ui.Q;
import dm.Dec;
import dm.Domo;
import dm.Dt;
import I18n._;
import dm.Opt;

/// Constants and global functions.
class Cts {
  /// Application name.
  public static final app = "Bet1x2";
  /// Application version.
  public static final version = "202109";
  /// Page foot.
  public static final foot = Q("table")
    .klass("main")
    .add(Q("tr")
      .add(Q("td")
        .add(Q("hr"))))
    .add(Q("tr")
      .add(Q("td")
        .style("text-align: right;color:#808080;font-size:x-small;")
        .html('- © ºDeme. ${app} (${version}) -')))
  ;
  /// Bet amount
  public static final bet: Float = 20.0;
  /// Application client.
  public static final client = new Client(true, app, () -> {
    final wg = Q("div");
    MsgPage.mk(wg, app, _("Session is expired."));
    Q("@body")
      .removeAll()
      .add(wg)
      .add(foot)
    ;
  });
  /// Returns the float value of 'n' with 2 decimals.
  public static function float (n: String): Option<Float> {
    n = n.trim().replace(".", "").replace(",", ".");
    if (n == "") return Some(0.0);
    final r = Std.parseFloat(n);
    return Math.isNaN(r) ? None : Some(Dec.round(r, 2));
  }
  /// Returns date with format matching selected language.
  public static function dateToStr (d: Date): String {
    return Storage.getLang() == "es"
      ? Dt.toIso(d)
      : Dt.toEn(d)
    ;
  }
  /// Move focus to a widget with id "autofocus".
  public static function autofocus (): Void {
    haxe.Timer.delay(() -> {
      var fc = Q("#autofocus");
      if (fc.e != null) fc.e.focus();
    }, 200);
  }
  /// Returns the selected index of a Domo type Select.
  public static function selectedIndex (e: Domo): Int {
    return cast(e.e, js.html.SelectElement).selectedIndex;
  }
}
