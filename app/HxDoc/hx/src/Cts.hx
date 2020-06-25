// Copyright 18-Jun-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import dm.Client;
import dm.Ui.Q;
import I18n._;

/// Constants.
class Cts {
  /// Application name.
  public static var app(default, null) = "HxDoc";
  /// Application version.
  public static var version(default, null) = "202006";
  /// Application client.
  public static var client = new Client(true, app, () -> {
    MsgPage.mk(Q("@body"), app, _("Session is expired."));
  });
}
