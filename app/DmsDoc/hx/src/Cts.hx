// Copyright 18-Jun-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import dm.Client;
import dm.Ui.Q;
import I18n._;

/// Constants.
class Cts {
  /// Application name.
  public static var app(default, null) = "DmsDoc";
  /// Application version.
  public static var version(default, null) = "202010";
  /// Application client.
  public static var client = new Client(true, app, () -> {
    MsgPage.mk(Q("@body"), app, _("Session is expired."));
  });
}
