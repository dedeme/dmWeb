// Copyright 03-Nov-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Js;
import I18n._;
import I18n._args;

/// Settings page.
class Settings {
  final wg: Domo;
  final lang: String;

  function new (wg: Domo, lang: String) {
    this.wg = wg;
    this.lang = lang;
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
                  .html(_args(
                    _("Change Language to %0"),
                    [lang == "es" ? "EN" : "ES"]
                  ))))
              .add(Q("p")
                .html("<p></p>"))
              .add(Q("div")
                .add(Ui.link(e -> changePass())
                  .klass("link")
                  .html(_("Change Password"))))))))
    ;
  }

  // Control -------------------------------------------------------------------

  function changeLang (): Void {
   Global.client.ssend([
      "prg" => Js.ws("Main"),
      "source" => Js.ws("Settings"),
      "rq" => Js.ws("setLang"),
      "lang" => Js.ws(lang == "es" ? "en" : "es")
    ], rp -> {
      js.Browser.location.assign("?settings");
    });
  }

  function changePass (): Void {
    new ChangePass(wg, () -> mk(wg)).show();
  }

  /// Constructor.
  public static function mk (wg: Domo): Void {
   Global.client.send([
      "prg" => Js.ws("Main"),
      "source" => Js.ws("Settings"),
      "rq" => Js.ws("getLang")
    ], rp -> {
      final lang = rp["lang"].rs();
      new Settings(wg, lang).show();
    });
  }

}
