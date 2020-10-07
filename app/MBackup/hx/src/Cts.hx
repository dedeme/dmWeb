// Copyright 04-Oct-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import dm.Ui.Q;
import dm.Client;
import I18n._;

/// Constants and global functions.
class Cts {
  /// Application name.
  public static final appName = "MBackup";
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

}
