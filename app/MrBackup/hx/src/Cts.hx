// Copyright 04-Oct-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import dm.Ui.Q;
import dm.Client;
import I18n._;
import I18n._args;

/// Constants and global functions.
class Cts {
  /// Application name.
  public static final appName = "MrBackup";
  /// Application version.
  public static final appVersion = "202010";
  /// Page foot.
  public static final foot = Q("table")
    .klass("main")
    .add(Q("tr")
      .add(Q("td")
        .add(Q("hr"))))
    .add(Q("tr")
      .add(Q("td")
        .style("text-align: right;color:#808080;font-size:x-small;")
        .html('- © ºDeme. ${appName} (${appVersion}) -')))
  ;
  /// Offset for messurements.
  public static final measureWg = Q("span").style("visibility:hidden");

  /// Application client.
  public static final client = new Client(true, appName, () -> {
    final wg = Q("div");
    MsgPage.mk(wg, appName, _("Session is expired."));
    Q("@body")
      .removeAll()
      .add(wg)
      .add(foot)
    ;
  });
  /// Validate directory identifier.
  /// Returns an error message or "" if everything is ok.
  public static function validateDirId (id: String): String {
    if (id == "") {
      return "Name is missing.";
    }
    for (i in 0...id.length) {
      final ch = id.charAt(i);
      if ( ch == "_" ||
          (ch >= "0" && ch <= "9") ||
          (ch >= "A" && ch <= "Z") ||
          (ch >= "a" && ch <= "z")
      ) {
        return continue;
      }
      return _args(_("Character '%0' is not allowed"), [ch]);
    }
    return "";
  }

  /// Returns the width of a string in pixels.
  public static function measure (tx: String): Int {
    measureWg.text(tx);
    return measureWg.e.offsetWidth;
  }

}
