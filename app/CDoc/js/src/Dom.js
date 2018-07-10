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
   * @param {!Array<!Path>} paths
   * @param {string} selected
   * @param {!Domo} o
   * @return {void}
   */
  show (paths, selected, o) {
    const self = this;
    let mkMenu = () =>
      $("table").att("border", "0").att("width", "100%")
        .add($("tr")
          .add($("td")
            .add((selected === ""
              ? $("a").klass("frame")
                .att("href", "?@")
              : $("a").att("href", "?@")
              )
              .add(Dom.img("asterisk").att("align", "top")))
            .addIt(
              It.from(paths).filter(r =>
                r.show()
              ).sortf((r1, r2) =>
                r1.id() > r2.id() ? 1 : -1
              ).map(row => {
                let sourceName = row.id();
                return $("span").text(" · ")
                  .add(
                    (selected === sourceName
                    ? $("span").att("class", "frame")
                      .add($("a")
                        .att("href", "?" + sourceName)
                        .text(sourceName))
                    : $("a").att("href", "?" + sourceName)
                      .text(sourceName)
                    )
                  );
              })))
          .add($("td").style("text-align:right;")
            .add(Ui.link(ev => {
                const data = {
                  "page": "Main",
                  "rq": "logout"
                };
                self._control.client().send0(data, rp => {
                  new user_Expired(self._control).show();
                });
              })
              .add(Dom.img("cross").att("align", "middle")))));

    self.showRoot(
      $("table").att("class", "main")
      .add($("tr")
        .add($("td").add(mkMenu())))
        .add($("tr")
          .add($("td").add($("hr"))))
        .add($("tr")
          .add($("td").add(o)))
    );
  }

  /**
   * @param {string} id
   * @return {!Domo}
   */
  static img (id) {
    return $("img")
      .att("src", "img/" + (id.endsWith(".gif") ? id : id + ".png"))
      .att("border", "0");
  }

  /**
   * @param {string} id
   * @return {!Domo}
   */
  static lightImg (id) {
    return Dom.img(id).style("opacity:0.4");
  }
}
