// Copyright 31-Jul-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pgs;

using StringTools;
import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Js;
import dm.Client;
import dm.Captcha;
import I18n._;

/// Change password page.
class ChangePass {
  final wg: Domo;
  final fn: Void -> Void;

  final app: String;
  final oldPass: Domo;
  final newPass: Domo;
  final newPass2: Domo;

  var captcha: Captcha;
  var failed: Bool;

  /// Constructor.
  ///   wg : Container widget
  ///   fn : Callback after cancellation or a valid acceptation.
  public function new (wg: Domo, fn: Void -> Void) {
    this.wg = wg;
    this.fn = fn;

    app = Cts.appName;
    oldPass = Ui.pass("newPass").att("id", "autofocus");
    newPass = Ui.pass("newPass2").att("id", "newPass");
    newPass2 = Ui.pass("acceptBt").att("id", "newPass2");

    captcha = new Captcha('${app}__captcha');
    failed = false;
  }

  // view --------------------------------------------------------------------

  public function show (): Void {
    oldPass.value("");
    newPass.value("");
    newPass2.value("");

    final cancelBt = Q("button")
      .on(CLICK, e -> cancel())
      .text(_("Cancel"))
    ;
    final acceptBt = Q("button")
      .att("id", "acceptBt")
      .on(CLICK, e -> accept())
      .text(_("Accept"))
    ;

    final rows = [
      Q("tr")
        .add(Q("td")
          .style("padding: 10px 0px 0px 10px;text-align:right;")
          .html(_("Current password")))
        .add(Q("td")
          .style("padding: 10px 10px 0px 10px;")
          .add(oldPass)),
      Q("tr")
        .add(Q("td")
          .style("padding: 5px 0px 0px 10px;text-align:right;")
          .html(_("New password")))
        .add(Q("td")
          .style("padding: 5px 10px 0px 10px;")
          .add(newPass)),
      Q("tr")
        .add(Q("td")
          .style("padding: 5px 0px 10px 10px;text-align:right;")
          .html(_("New password")))
        .add(Q("td")
          .style("padding: 5px 10px 10px 10px;")
          .add(newPass2)),
      Q("tr")
        .add(Q("td")
          .att("colspan", 2)
          .style(
            "border-top:1px solid #c9c9c9;" +
            "padding: 10px 10px 10px;text-align:right;")
          .add(Q("span")
            .add(cancelBt))
          .add(Q("span")
            .text("  "))
          .add(Q("span")
            .add(acceptBt)))
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

    oldPass.e.focus();
  }

  // Control -----------------------------------------------------------------

  function cancel (): Void {
    fn();
  }

  function accept (): Void {
    final opass = cast(oldPass.getValue(), String).trim();
    final npass = cast(newPass.getValue(), String).trim();
    final npass2 = cast(newPass2.getValue(), String).trim();

    if (opass == "") {
      Ui.alert(_("Current password is missing"));
      show();
      return;
    }
    if (npass == "") {
      Ui.alert(_("New password is missing"));
      return;
    }
    if (npass2 == "") {
      Ui.alert(_("Confirm password is missing"));
      return;
    }
    if (npass != npass2) {
      Ui.alert(_("New password and confirm password do not match"));
      return;
    }

    if (captcha.isUpLimit() && !captcha.check()) {
      Ui.alert(_("Grey squares checks are wrong"));
      captcha = new Captcha('${app}__captcha');
      show();
      return;
    }

    Cts.client.send([
      "source" => Js.ws("ChangePass"),
      "rq" => Js.ws("changePass"),
      "user" => Js.ws(Cts.client.user),
      "old" => Js.ws(Client.crypPass(opass)),
      "new" => Js.ws(Client.crypPass(npass))
    ], rp -> {
      if (rp["ok"].rb()) {
        captcha.reset();
        Ui.alert(_("Password successfully changed"));
        fn();
      } else {
        failed = true;
        captcha.increment();
        captcha = new Captcha('${app}__captcha');
        show();
      }
    });
  }
}
