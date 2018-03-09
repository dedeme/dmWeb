// Copyright 06-Mar-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

goog.provide("Dom");

Dom = class {
  /**
   * @param {!Main} control
   */
  constructor (control) {
    /** @private */
    this._control = control;
  }

  /**
   * @param {!Domo} o
   * @return {void}
   */
  showRoot (o) {
    $$("body").next().removeAll().add(
      $("div")
        .add(o)
        .add($("p").html("&nbsp;"))
        .add($("hr"))
        .add($("table").klass("main")
          .add($("tr")
            .add($("td")
              .add($("a")
                .att("href", "doc/about.html")
                .att("target", "blank")
                .html("<small>" + _("Help & Credits") + "</small>")))
            .add($("td")
              .style("text-align: right;font-size: 10px;" +
                "color:#808080;font-size:x-small;")
              .html("- © ºDeme. " + Main.app() + " (" +
                Main.version() + ") -"))))
    );
  }

  /**
   * @param {string} page
   * @param {string} subpage
   * @param {!Domo} o
   * @return {void}
   */
  show (page, subpage, o) {

    const control = this._control;
    const user = control.client().user();

    function entry(id, target) {
      return Ui.link(ev => { control.go(target) })
        .klass(target === page ? "frame" : "link").html(id);
    }
    function separator() {
      return $("span").html(" · ");
    }

    const lopts = [
      separator(),
      entry(_("Accounting"), "accounting"),
      separator(),
      entry(_("Portfolio"), "portfolio"),
      separator(),
      entry(_("Market"), "market"),
    ];

    const ropts = [
      entry(_("Backs"), "backups"),
      separator(),
      entry(_("Settings"), "settings"),
      separator(),
      Ui.link(ev => { control.go("bye"); })
        .add(Ui.img("cross").style("vertical-align:bottom"))
    ];

    const menu1 = $("table").klass("main").add($("tr")
      .add($("td")
        .addIt(It.from(lopts)))
      .add($("td").style("text-align:right")
        .addIt(It.from(ropts)))
    );

    this.showRoot(
      $("div")
        .add(menu1)
        .add($("hr"))
        .add(o)
    );

  }
}
