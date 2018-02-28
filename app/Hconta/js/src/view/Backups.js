// Copyright 23-Oct-2017 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

goog.provide("view_Backups");

view_Backups = class {
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
    const self = this;
    const control = self._control;

    function download() {
      const div = $("div");
      return $("tr").add($("td").att("colspan", 2)
        .add($("table").att("align", "center")
          .add($("tr")
            .add($("td").style("width:250px").klass("frame")
              .add(div)))
          .add($("tr")
            .add($("td")
              .add($("button").html(_("Make backup")).on("click", () => {
                div.add($("img").att("src", "img/wait.gif"));
                control.backupDownload(fileName => {
                  div.html("<a href='tmp/" + fileName + "'>backup.zip</a>");
                });
              }))))));
    }

    function upload() {
      const progress = $("div")
        .style("background-color:#000080;width:0px;height:6px");
      const bar = $("div")
        .style("text-align:left;width:200px;background-color:#cccccc")
        .klass("frame")
        .add(progress);
      const input = $("input").att("type", "file").klass("frame");
      const div = $("div");
      div.add($("button").html(_("Restore backup")).on("click", () => {
        const n = input.e().files/**/.length;
        if (n === 0) {
          alert(_("Backup file is missing"));
          return
        }
        if (n !== 1) {
          alert(_("Only one file can be selected"));
          return
        }
        const file = input.e().files/**/[0];
        if (file.size/**/ === 0) {
          alert(_args(_("'%0' is an empty file"), file.name/**/));
        }
        if (!confirm(_("All the data will be replaced"))) {
          return;
        }
        div.removeAll().add(bar);
        control.backupRestore(file, n => {
          const pc = Math.round(n * 100 / file.size/**/);
          const progress = $("div")
            .style("background-color:#000080;width:" + pc + "%;height:6px");
          bar.removeAll().add(progress);
        });
      }));
      return $("tr").add($("td").att("colspan", 2)
        .add($("table").att("align", "center")
          .add($("tr")
            .add($("td").add(input)))
          .add($("tr")
            .add($("td").att("align", "center").add(div)))));
    }

    function lists() {
      return $("tr").add($("td").att("colspan", 2)
        .add($("table").att("align", "center")
          .add($("tr")
            .add($("td").html("<b>" + _("Backs") + "</b>"))
            .add($("td").style("width:25px"))
            .add($("td")
              .add($("span").html("<b>" + _("Trash") + "</b>"))
              .add(Ui.link(ev => {
                  if (confirm(_("Clear trash?"))) {
                    control.clearTrash();
                  }
                }).klass("link").html(" [ " + _("Clear") + " ]"))))
          .add($("tr")
            .add($("td").klass("frame").style("vertical-align:top")
              .add($("table")
                .addIt(It.from(control.backups()).sort().reverse().map(f =>
                  $("tr").add($("td")
                    .add(Ui.link(ev => {
                        if (confirm(_("All the data will be replaced"))) {
                          control.autorestore(f);
                        }
                      }).klass("link").html(f)))))))
            .add($("td"))
            .add($("td").klass("frame").style("vertical-align:top")
              .add($("table")
                .addIt(It.from(control.trash()).sort().reverse().map(f =>
                  $("tr").add($("td")
                    .add(Ui.link(ev => {
                        if (confirm(_("All the data will be replaced"))) {
                          control.restoreTrash(f);
                        }
                      }).klass("link").html(f))))))))));
    }

    control.dom().show("backups", $("table")
      .style("width:100%;text-align:center")
      .add($("tr").add($("td").att("colspan", 2)
        .html("<b>" + _("Backups") + "<b>")))
      .add($("tr")
        .add($("td").style("width:5px;white-space: nowrap;text-align:right")
          .html(_("Download")))
        .add($("td").add($("hr"))))
      .add(download())
      .add($("tr")
        .add($("td").style("width:5px;white-space: nowrap;text-align:right")
          .html(_("Restore")))
        .add($("td").add($("hr"))))
      .add(upload())
      .add($("tr")
        .add($("td").att("colspan", 2).add($("hr"))))
      .add(lists())
    );
  }
}
