// Copyright 25-Nov-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pgs;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Js;
import I18n._;
import I18n._args;

/// Settings page.
class Settings {
  final wg: Domo;

  function new (wg: Domo) {
    this.wg = wg;
  }

  // View ----------------------------------------------------------------------

  function show (): Void {
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
                .add(Ui.link(e -> changeLang())
                  .klass("link")
                  .html(_("Change Language <small>(External)</small>"))))
              .add(Q("p")
                .html("<p></p>"))
              .add(Q("div")
                .add(Ui.link(e -> changePass())
                  .klass("link")
                  .html(_("Change Password <small>(External)</small>"))))))))
    ;
  }

  // Control -------------------------------------------------------------------

  function changeLang (): Void {
    js.Browser.window.open("../Main/?settings", "_blank");
  }

  function changePass (): Void {
    js.Browser.window.open("../Main/?settings", "_blank");
  }

  /// Constructor.
  public static function mk (wg: Domo): Void {
    new Settings(wg).show();
  }

}
