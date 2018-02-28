// Copyright 23-Sep-2017 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// User authentication.

goog.provide("user_Auth");

goog.require("github_dedeme.Captcha");

user_Auth = class {
  /**
   * @param {!Main} control
   */
  constructor (control, client) {
    /** @private */
    this._control = control;
    /** @private */
    this._client = client;
  }

  /**
   * @return {void}
   */
  show () {
    const control = this._control;
    const captchaStore = Main.captchaAuthStore();

    /** @const {!Domo} */
    const user = Ui.field("pass");
    /** @const {!Domo} */
    const pass = Ui.pass("accept").att("id", "pass");
    /** @const {!Domo} */
    const persistent = $("input")
      .att("type", "checkbox")
      .style("vertical-align: middle");
    /** @const {!Domo} */
    const accept = $("input")
      .att("type", "button")
      .style("width:90px;")
      .att("id", "accept")
      .value(_("Accept"));

    /** @type {github_dedeme.Captcha} */
    let captcha = new github_dedeme.Captcha(captchaStore, 3);

    /**
     * Change selected language
     * @type {function(?):void}
     */
    const changeLanguage = (ev) => {
      Store.put(
        Main.langStore(),
        Store.get(Main.langStore()) === "en" ? "es" : "en"
      );
      control.run();
    }

    /**
     * Accept button pressed
     * @type {function(string, string, boolean):void}
     */
    const faccept = (user, pass, persistent) => {
      if (user == "") {
        alert(_("User name is missing"));
        return;
      }
      if (pass == "") {
        alert(_("Password is missing"));
        return;
      }
      this._client.authentication(user, pass, !persistent, (ok) => {
        if (ok) {
          captcha.resetCounter();
        } else {
          captcha.incCounter();
        }
        control.run();
      })
    }

    /**
     * @private
     * @return {!Domo}
     */
    function body () {
      const counter = captcha.counter();
      const counterLimit = captcha.counterLimit();

      accept.on("click", e => {
        if (counter > counterLimit && !captcha.match()) {
          alert(_("Grey squares checks are wrong"));
          control.run();
          return;
        }
        faccept(
          user.value().trim(),
          pass.value().trim(),
          persistent.checked()
        );
      });

      let rows = [
        $("tr")
          .add($("td")
            .style("padding: 10px 0px 0px 10px;text-align:right;")
            .html(_("User")))
          .add($("td").style("padding: 10px 10px 0px 10px;").add(user)),
        $("tr")
          .add($("td")
            .style("padding: 10px 0px 0px 10px;text-align:right;")
            .html(_("Password")))
          .add($("td").style("padding: 10px 10px 5px 10px;").add(pass)),
        $("tr")
          .add($("td")
            .att("colspan", 2)
            .style('border-top:1px solid #c9c9c9;' +
              "padding: 5px 10px 10px;text-align:right;")
            .add($("table")
              .style(
                "border-collapse : collapse;" +
                "border : 0px;" +
                "width : 100%;")
              .add($("tr")
                .add($("td").att("align", "center").att("colspan", 2)
                  .add(persistent)
                  .add($("span").html("&nbsp;" + _("Keep connected")))))
              .add($("tr")
                .add($("td")
                  .add(Ui.link(changeLanguage).att("class", "link")
                    .html(Store.get(
                      Main.langStore()) === "en" ? "ES" : "EN")))
                .add($("td").att("align", "right").add(accept)))))
      ];

      if (counter > 0) {
        rows.push(
          $("tr")
            .add($("td")
              .att("colspan", 2)
              .style('border-top:1px solid #c9c9c9;' +
                "padding: 10px 10px 10px;text-align:right;")
              .add($("table")
                .att("align", "center")
                .style("background-color: rgb(250, 250, 250);" +
                  "border: 1px solid rgb(110,130,150);" +
                  "font-family: sans;font-size: 14px;" +
                  "padding: 4px;border-radius: 4px;")
                .add($("tr")
                  .add($("td").html(_("Wrong password"))))))
        );
      }

      if (counter > counterLimit) {
        rows.push(
          $("tr")
            .add($("td").att("colspan", 2).att("align", "center")
              .add(captcha.make()))
        );
        rows.push(
          $("tr")
            .add($("td")
              .att("colspan", 2)
              .style("padding: 5px 0px 5px 10px;text-align:center;")
              .html(_("Check gray squares")))
        );
      }

      return $("table")
        .att("align", "center")
        .style(
          'background-color: #f8f8f8;' +
          "border-collapse: collapse;" +
          "padding: 10px;" +
          "border: 1px solid rgb(110,130,150);")
        .add($("tr")
          .add($("td")
            .att("colspan", 2)
            .style(
              'background-color:#e8e8e8;' +
              'border-bottom:1px solid #c9c9c9;' +
              "padding: 10px;" +
              'color:#505050;'
            )
            .html("<big><big><b>" + _("Login") + "</big></big></b>")))
        .addIt(It.from(rows));
    }


    Store.get(Main.langStore()) === "en" ? I18n.en() : I18n.es();

    control.dom().showRoot(
      $("div")
        .add($("div").klass("title").html(
          "&nbsp;<br>" + Main.app() + "<br>&nbsp;"))
        .add($("div").add(body()))
    );

    user.e().focus();
  }
}

