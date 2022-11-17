// Copyright 03-Nov-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pgs;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Js;
import I18n._;
import I18n._args;
import data.Conf;

/// Settings page.
class Settings {
  final wg: Domo;
  final color: String;

  function new (wg: Domo, color: String) {
    this.wg = wg;
    this.color = color;
  }

  // View ----------------------------------------------------------------------

  function show (): Void {
    final colorPicker = Q("input")
      .att("type", "color")
      .value(color)
    ;
    colorPicker.on(CHANGE, () -> setColor(colorPicker.getValue()));

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
                .add(Q("span")
                  .html(_("Message background") + ": "))
                .add(colorPicker))
              .add(Q("p")
                .html("<p></p>"))
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

  function setColor(color: String): Void {
   Global.client.ssend([
      "prg" => Js.ws("Template"),
      "source" => Js.ws("Settings"),
      "rq" => Js.ws("setColor"),
      "color" => Js.ws(color)
    ], rp -> {
      mk(wg);
    });
  }

  function changeLang (): Void {
    js.Browser.window.open("../Main/?settings", "_blank");
  }

  function changePass (): Void {
    js.Browser.window.open("../Main/?settings", "_blank");
  }

  /// Constructor.
  public static function mk (wg: Domo): Void {
   Global.client.send([
      "prg" => Js.ws("Template"),
      "source" => Js.ws("Settings"),
      "rq" => Js.ws("idata")
    ], rp -> {
      final conf = Conf.fromJs(rp["conf"]);
      new Settings(wg, conf.color).show();
    });
  }

}
