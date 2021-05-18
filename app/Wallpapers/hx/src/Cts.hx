// Copyright 26-Apr-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import js.Browser.window;
import dm.Client;
import dm.Ui.Q;
import I18n._;

/// Constants and global functions.
class Cts {

  // CONSTANTS

  /// Application name.
  public static final app = "Wallpapers";
  /// Application version.
  public static final version = "202104";
  /// Store language key.
  public static final langKey = "Lang_" + app;
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
  /// Screen width.
  public static final screenWidth:Int = window.screen.width;
  /// Screen height.
  public static final screenHeight: Int = window.screen.height;
  /// Time to access server in Pictures page (milliseconds)
	public static final picturesTime = 15000;
  /// Time to access server in songs page (milliseconds)
	public static final songsTime = 5000;
  /// Time of long dance (milliseconds)
  public static final longDanceTime = 2700000.0; // 45'
  /// Time of short dance (milliseconds)
  public static final shortDanceTime = 60000.0;//900000.0; // 15'
  /// Time of fade out in dance (milliseconds)
  public static final fadeOutDanceTime = 30000.0;//300000.0; // 5'

  // FUNCTIONS

  /// Returns n with 'digits' digits.
  public static function formatInt (n: Int, digits: Int): String {
    var r = Std.string(n);
    while (r.length < digits) r = "0" + r;
    return r;
  }

}
