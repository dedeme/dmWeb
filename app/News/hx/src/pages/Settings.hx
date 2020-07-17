// Copyright 28-Jun-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pages;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Js;
import I18n._;
import I18n._args;
import data.Conf;

/// Settings page.
class Settings {
  // Control -------------------------------------------------------------------

  static function changeLang (conf: Conf): Void {
    conf.lang = conf.lang == "en" ? "es" : "en";
    Cts.client.ssend([
      "source" => Js.ws("Main"),
      "rq" => Js.ws("lang"),
      "lang" => Js.ws(conf.lang)
    ], rp -> {
      js.Browser.location.assign("");
    });
  }

  static function changePass (wg: Domo, conf: Conf): Void {
    ChangePass.mk(wg, Cts.app, () -> mk(wg, conf));
  }

  /// View ---------------------------------------------------------------------

  /// Constructor.
  public static function mk (wg: Domo, conf: Conf): Void {
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
                .add(Ui.link(e -> changeLang(conf))
                  .klass("link")
                  .html(_args(
                    _("Change Language to %0"),
                    [conf.lang == "es" ? "EN" : "ES"]
                  ))))
              .add(Q("p")
                .html("<p></p>"))
              .add(Q("div")
                .add(Ui.link(e -> changePass(wg, conf))
                  .klass("link")
                  .html(_("Change Password"))))))))
    ;
  }
}
