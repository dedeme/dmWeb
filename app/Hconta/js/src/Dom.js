// Copyright 23-Sep-2017 ºDeme
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
                .html("<small>Help & Credits</small>")))
            .add($("td")
              .style("text-align: right;font-size: 10px;" +
                "color:#808080;font-size:x-small;")
              .html("- © ºDeme. " + Main.app() + " (" +
                Main.version() + ") -"))))
    );
  }

  /**
   * @param {string} page
   * @param {!Domo} o
   * @return {void}
   */
  show (page, o) {
    const control = this._control;
    const conf = control.conf();

    function entry(id, target) {
      return Ui.link(ev => { control.go(target) })
        .klass(target === page ? "frame" : "link").html(id);
    }
    function separator() {
      return $("span").html(" · ");
    }

    const menu = $("table").klass("main").add($("tr")
      .add($("td")
        .add(
          entry(conf.year(), "year")
            .addStyle(conf.isLastYear() ? "" : "color:#800000")
        ).add(separator())
        .add(entry(_("Diary"), "diary"))
        .add(separator())
        .add(entry(_("Cash"), "cash"))
        .add(separator())
        .add(entry(_("Accs"), "accs"))
        .add(separator())
        .add(entry(_("Summaries"), "summaries"))
        .add(separator())
        .add(entry(_("Plan"), "plan")))
      .add($("td").style("text-align:right")
        .add(entry(_("Backs"), "backups"))
        .add(separator())
        .add(entry(_("Settings"), "settings"))
        .add(separator())
        .add(Ui.link(ev => { control.bye(); })
          .add(Ui.img("cross").style("vertical-align:bottom")))))
    ;

    this.showRoot(
      $("div")
        .add(menu)
        .add($("hr"))
        .add(o)
    );
  }

  /**
   * Returns true if 's' is a valid number in language "l"
   * @param {string} s
   * @return {boolean}
   */
  isNumber (s) {
    if (this._control.conf().language() === "es") {
      return Dec.isNumberEu(s);
    }
    return Dec.isNumberEn(s);
  }

  /**
   * Returns 0 if s is an empty string.
   * @param {string} s
   * @return {!Dec}
   */
  toDec (s) {
    if (s === "") {
      return new Dec(0, 2);
    }
    return this._control.conf().language() === "es"
      ? Dec.newEu(s, 2)
      : Dec.newEn(s, 2);
  }

  /**
   * Returns 'd' in european or inglish format, depending on conf.language
   * @param {!Dec} d
   * @return {string}
   */
  decToStr (d) {
    return this._control.conf().language() === "es" ? d.toEu() : d.toEn();
  }

  /**
   * Returns text width of a text of type "14px sans"
   * @param {string} tx
   * @return {number}
   */
  static textWidth (tx) {
    const c = $("canvas");
    const e = c.e();
    const ctx = e.getContext("2d");
    ctx.font/**/ = "14px sans";
    return ctx.measureText(tx).width/**/;
  }

  /**
   * Adjusts tx to 'px' pixels. Font family must be "14px sans"
   * @param {string} tx
   * @param {number} px
   * @return {string}
   */
  static textAdjust (tx, px) {
    if (Dom.textWidth(tx) < px) {
      return tx;
    }

    tx = tx.substring(0, tx.length - 3) + "...";
    while (Dom.textWidth(tx) >= px) {
      tx = tx.substring(0, tx.length - 4) + "...";
    }
    return tx;
  }

  /**
   * Formats a subaccount like xxx.xx
   * @param {string} acc
   * @return {string}
   */
  static accFormat (acc) {
    return acc.substring(0, 3) + "." + acc.substring(3);
  }

}
