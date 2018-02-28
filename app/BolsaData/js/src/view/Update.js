// Copyright 12-Nov-2017 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

goog.provide("view_Update");

view_Update = class {
  /**
   * @param {!Main} control
   */
  constructor (control) {
    /** @private */
    this._control = control;
  }

  /**
   * @return {void}
   */
  show () {
    function quoteTranslator() {
      function dateFormatInvertia(date) {
        const ps = date.split("/");
        return 20 + ps[2] + ps[1] + ps[0];
      }
      function dateFormatFinanzas(date) {
        const ps = date.split("/");
        return ps[2] + ps[1] + ps[0];
      }
      function format (dateFormat, l, dec, sep) {
        let prev = "";
        return It.range(l.length).reduce("", (s, i) => {
          const ch = l.charAt(i);
          if (prev === "") {
            if (ch <= " ") {
              prev = " ";
              return dateFormat(s) + ":";
            } else {
              return s + ch;
            }
          } else {
            if (ch <= " ") {
              if (prev === " ") {
                return s;
              } else {
                prev = " ";
                return s + ":";
              }
            } else {
              prev = ch;
              return s + (ch === dec ? "." : ch === sep ? "" : ch);
            }
          }
        });
      }
      const input = $("textarea").att("rows", 1).att("cols", 60);
      const output = $("textarea").att("rows", 1).att("cols", 60);
      return $("table")
        .add($("tr").add($("td").att("colspan", 5).add(input)))
        .add($("tr")
          .add($("td").add($("button").html("Invertia").on("click", ev => {
              const p = format(
                dateFormatInvertia, input.value(), ",", "."
              ).split(":");
              input.value("");
              output.value(
                p[0] + ":" + p[2] + ":" + p[1] + ":" +
                p[4] + ":" + p[5] + ":" + p[6] + ":false"
              ).e().select();
            })))
          .add($("td").add($("span").html("&nbsp;&nbsp;")))
          .add($("td").add($("button").html("Finanzas").on("click", ev => {
              const p = format(
                dateFormatFinanzas, input.value(), ",", "."
              ).split(":");
              input.value("");
              output.value(
                p[0] + ":" + p[1] + ":" + p[2] + ":" +
                p[4] + ":" + p[5] + ":" + p[6] + ":false"
              ).e().select();
            })))
          .add($("td").add($("span").html("&nbsp;&nbsp;")))
          .add($("td").add($("button").html("Informercados").on("click", ev => {
              const p = format(
                dateFormatFinanzas, input.value(), ",", "."
              ).split(":");
              input.value("");
              output.value(
                p[0] + ":" + p[1] + ":" + p[4] + ":" +
                p[2] + ":" + p[3] + ":" + p[5] + ":false"
              ).e().select();
            }))))
        .add($("tr").add($("td").att("colspan", 5).add(output)))
    }

    const control = this._control;
    const db = control.db();

    let pages = 1;
    const pagesSpan = $("span")
      .att("title", _("Pages to update"))
      .html(_args(_("%0 pages"), pages));
    const editor = $("textarea").att("rows", 25).att("cols", 60);

    const right = $("div").style("width:100%;height:100%").klass("frame")
      .add($("div").style("width:100%;text-align:center")
        .html("Total = " + It.keys(db.invertiaId()).size() +
          " : Ibex = " + It.keys(db.ibex()).reduce(0, (s, k) =>
              db.ibex()[k] ? s + 1 : s
            )));

    const left = $("table")
      .add($("tr")
        .add($("td").style("width:5px;"))
        .add($("td").style("width:5px;"))
        .add($("td").add(Ui.link(ev => {
            if (pages < 19) {
              ++pages;
            }
            pagesSpan.html(_args(_("%0 pages"), pages));
          })
          .add(Ui.img("plus"))))
        .add($("td").add(Ui.link(ev => {
            if (pages > 1) {
              --pages;
            }
            pagesSpan.html(_args(_("%0 pages"), pages));
          })
          .add(Ui.img("minus"))))
        .add($("td").style("text-align:left;white-space:nowrap;")
          .add(pagesSpan)))
      .add($("tr")
        .add($("td").style("width:5px;"))
        .add($("td").style("width:5px;"))
        .add($("td").add(db.quoteTranslator()
          ? Ui.link(ev => { control.quoteTranslator(false); })
              .add(Ui.img("transOff"))
          : Ui.link(ev => { control.quoteTranslator(true); })
              .add(Ui.img("transOn"))))
        .add($("td").style("width:5px;"))
        .add($("td").style("text-align:left;white-space:nowrap;")
          .add(Ui.link(ev => {
              const counter = $("div").klass("frame");
              right.removeAll().add(counter);
              control.updateAll(pages, counter);
            }).klass("link").html(_("Update All")))))
      .add($("tr").add($("td").att("colspan", 5).html("<hr>")))
      .addIt(It.keys(db.invertiaId()).sort().map(k =>
        $("tr")
          .add($("td").add(Ui.img(db.ibex()[k] ? "flag" : "blank")))
          .add($("td").add(db.status()[k] === ""
              ? Ui.img("well")
              : Ui.img("error").att("title", db.status()[k])
            ))
          .add($("td").add(Ui.link(ev => {
              if (confirm(_args(_("Delete '%0'?"), k))) {
                control.del(k);
              }
            })
            .add(Ui.img("delete"))))
          .add($("td").add(Ui.link(ev => {
              if (confirm(_args(_("Update '%0' from %1?"), k, db.source()))) {
                const counter = $("div").klass("frame");
                right.removeAll().add(counter);
                control.update(k, pages, counter);
              }
            })
            .add(Ui.img("download"))))
          .add($("td").style("text-align:left;white-space:nowrap;")
            .add(Ui.link(ev => {
              right.removeAll().add($("table").att("align", "center")
                .add($("tr")
                  .add($("td").att("colspan", 2).style("text-align:center")
                    .html("<b>" + k + "</b>")))
                .add($("tr")
                  .add($("td").style("text-align:left;")
                    .add(Ui.link(ev => { control.setModel(k); }).klass("link")
                      .html(_("Set Model")))
                    .add($("span").html(" · "))
                    .add(db.ibex()[k]
                      ? Ui.link(ev => { control.ibex(k, false); }).klass("link")
                        .html(_("Remove from Ibex"))
                      : Ui.link(ev => { control.ibex(k, true); }).klass("link")
                        .html(_("Add to Ibex"))))
                  .add($("td").style("text-align:right;")
                    .add($("button").html(_("Modify")).on("click", ev => {
                        if (confirm(_("Modify data?"))) {
                          control.modify(k, editor.value());
                        }
                      }))))
                .add($("tr")
                  .add($("td").att("colspan", 2).style("text-align:left;")
                    .klass("frame")
                    .html(_("Date:Open:Close:Max:Min:Vol:Status"))))
                .add($("tr")
                  .add($("td").att("colspan", 2)
                    .add(editor.value(db.quotes()[k].join("\n")))))
              );
            }).klass("link").html(
              (k === db.model() ? "<b>" : "") +
              k +
              (k === db.model() ? "</b>" : "") +
              " [" + db.quotes()[k].length +
              " (" + db.fixeds()[k] + ")]"
            )))
      ));

    control.dom().show("update", $("table").klass("main").add($("tr")
      .add($("td").style("vertical-align:top;text-align:left;width:5px")
        .klass("frame")
        .add(left))
      .add($("td").style("vertical-align:top;text-align:center")
        .add(right))));
    $$("body").next()
      .add($("div")
        .style("position: fixed;bottom: 0px;right: 20px")
        .add(db.quoteTranslator() ? quoteTranslator() : $("span"))
        .add(Ui.link(ev => { window.scrollTo(0, 0); }).add(Ui.img("up"))));
  }
}


