// Copyright 16-Oct-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

using StringTools;
import dm.Domo;
import dm.Ui.Q;
import dm.Ui;
import dm.Captcha;
import I18n;
import I18n._;

/// Authentication page.
class Authentication {
  /// Constructor.
  ///   wg  : Container widget
  ///   app : Application name.
  ///   fnOk: Action after a valid acceptation.
  public static function mk (wg: Domo, app: String, fnOk: Void -> Void): Void {
    var captcha = new Captcha('${app}__captcha');
    var failed = false;
    var view = () -> {};

    // Control -----------------------------------------------------------------

    final changeLanguage = () -> {
      if (I18n.lang == "es") I18n.en();
      else I18n.es();
      view();
    }

    final send = (user, pass, withExpiration) -> {
      if (user == "") {
        Ui.alert(_("User name is missing"));
        return;
      }
      if (pass == "") {
        Ui.alert(_("Password is missing"));
        return;
      }

      if (captcha.isUpLimit() && !captcha.check()) {
        Ui.alert(_("Grey squares checks are wrong"));
        captcha = new Captcha('${app}__captcha');
        view();
        return;
      }

      Cts.client.authentication(user, pass, withExpiration, ok -> {
        if (ok) {
          captcha.reset();
          fnOk();
        } else {
          failed = true;
          captcha.increment();
          captcha = new Captcha('${app}__captcha');
          view();
        }
      });
    }

    // View --------------------------------------------------------------------
    view = () -> {
      final pass = Ui.pass("accept").att("id", "autofocus");
      final userIn = Ui.field("autofocus").value("admin");
      final persistent = Q("input")
        .att("type", "checkbox")
        .style("vertical-align: middle")
        .checked(true);
      final accept = Q("button")
        .att("id", "accept")
        .on(CLICK, e -> {
          send(
            cast(userIn.getValue(), String).trim(),
            cast(pass.getValue(), String).trim(),
            !persistent.getChecked()
          );
        })
        .text(_("Accept"));

      final rows = [
        Q("tr")
          .add(Q("td")
            .style("padding: 10px 0px 0px 10px;text-align:right;")
            .html(_("User")))
          .add(Q("td")
            .style("padding: 10px 10px 0px 10px;")
            .add(userIn)),
        Q("tr")
          .add(Q("td")
            .style("padding: 10px 0px 0px 10px;text-align:right;")
            .html(_("Password")))
          .add(Q("td")
            .style("padding: 10px 10px 5px 10px;")
            .add(pass)),
        Q("tr")
          .add(Q("td")
            .att("colspan", 2)
            .style("border-top:1px solid #c9c9c9;" +
                   "padding: 5px 10px 10px;text-align:right;")
            .add(Q("table")
              .style(
                "border-collapse : collapse;" +
                "border : 0px;" +
                "width : 100%;")
              .add(Q("tr")
                .add(Q("td")
                  .att("align", "center")
                  .att("colspan", 2)
                  .add(persistent)
                  .add(Q("span")
                    .html("&nbsp;" + _("Keep connected")))))
              .add(Q("tr")
                .add(Q("td")
                  .add(Ui.link(e -> changeLanguage ())
                    .att("class", "link")
                    .html(I18n.lang == "en" ? "ES" : "EN")))
                .add(Q("td").att("align", "right").add(accept)))))
      ];

      if (failed) {
        rows.push(
          Q("tr")
            .add(Q("td")
              .att("colspan", 2)
              .style("border-top:1px solid #c9c9c9;" +
                     "adding: 10px 10px 10px;text-align:right;")
              .add(Q("table")
                .att("align", "center")
                .style(
                  "background-color: rgb(250, 250, 250);" +
                  "border: 1px solid rgb(110,130,150);" +
                  "font-family: sans;font-size: 14px;" +
                  "padding: 4px;border-radius: 4px;")
                .add(Q("tr")
                  .add(Q("td")
                    .html(_("Wrong password"))))))
        );
      }

      if (captcha.isUpLimit()) {
        rows.push(
          Q("tr")
            .add(Q("td")
              .att("colspan", 2)
              .att("align", "center")
              .add(captcha.wg))
        );
        rows.push(
          Q("tr")
            .add(Q("td")
              .att("colspan", 2)
              .style("padding: 5px 0px 5px 10px;text-align:center;")
              .html(_("Check gray squares")))
        );
      }

      wg
        .removeAll()
        .add(Q("div")
          .klass("head")
          .html('&nbsp;<br>${app}<br>&nbsp;'))
        .add(Q("table")
          .att("align", "center")
          .style(
            "background-color: #f8f8f8;" +
            "border-collapse: collapse;" +
            "padding: 10px;" +
            "border: 1px solid rgb(110,130,150);")
          .add(Q("tr")
            .add(Q("td")
              .att("colspan", 2)
              .style(
                "background-color:#e8e8e8;" +
                "border-bottom:1px solid #c9c9c9;" +
                "padding: 10px;" +
                "color:#505050;"
              )
              .html("<big><big><b>" + _("Login") + "</big></big></b>")))
          .adds(rows));

      pass.e.focus();
    }

    view();
  }

}
