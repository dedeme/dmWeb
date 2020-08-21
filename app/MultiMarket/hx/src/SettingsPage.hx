// Copyright 28-Jun-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import I18n._;
import I18n._args;
import data.All;

/// Settings page.
class SettingsPage {
  // Control -------------------------------------------------------------------

  static function changeLang (data: All): Void {
    data.conf.language = data.conf.language == "en" ? "es" : "en";
    data.send(() -> js.Browser.location.assign(""));
  }

  static function changePass (wg: Domo, data: All): Void {
    ChangePass.mk(wg, Cts.appName, () -> mk(wg, data));
  }

  /// View ---------------------------------------------------------------------

  /// Constructor.
  public static function mk (wg: Domo, data: All): Void {
    wg
      .removeAll()
      .add(Q("div")
        .style("text-align:center")
        .add(Q("div")
          .klass("head")
          .html(_("Settings")))
        .add(Q("table")
          .att("align", "center")
          .add(Q("tr")
            .add(Q("td")
              .klass("frame")
              .add(Q("div")
                .add(Ui.link(e -> changeLang(data))
                  .klass("link")
                  .html(_args(
                    _("Change Language to %0"),
                    [data.conf.language == "es" ? "EN" : "ES"]
                  ))))
              .add(Q("p")
                .html("<p></p>"))
              .add(Q("div")
                .add(Ui.link(e -> changePass(wg, data))
                  .klass("link")
                  .html(_("Change Password"))))))))
    ;
  }
}
