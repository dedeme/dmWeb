// Copyright 09-07-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

goog.provide("Backups");

Backups = class {
  /**
   * @param {!Main} main
   * @param {string} menu
   * @return {!Backup}
   */
  constructor (main, menu) {
    /** @private */
    this._main = main;

    /** @private */
    this._menu = menu;
  }

  /**
   * @param {!Domo} div
   * @return {void}
   */
  backupDownload (div) {
    const fileName = "f";
    const data = {"page": "backups", "rq": "backup"};
    this._main.client().send(data, rp => {
      div.html("<a href='tmp/" + rp["name"] + "'>backup.zip</a>");
    });
  }

  /**
   * @param {*} file
   * @param {!Domo} bar
   * @return {void}
   */
  backupRestore (file, bar) {

    function progress(n) {
      const pc = Math.round(n * 100 / file.size/**/);
      const progress = $("div")
        .style("background-color:#000080;width:" + pc + "%;height:6px");
      bar.removeAll().add(progress);
    }

    const self = this;
    const main = self._main;
    const client = main.client();
    const step = 25000;
    let start = 0;

    const reader = new FileReader();
    reader.onerror/**/ = evt => {
      alert(_args(_("'%0' can not be read"), file.name/**/));
      const data = {"page": "backups", "rq": "restoreAbort"};
      client.send(data, () => {
        main.run();
      });
    }
    reader.onloadend/**/ = evt => {
      if (evt.target/**/.readyState/**/ === FileReader.DONE/**/) { // DONE == 2
        const bindata = new Uint8Array(evt.target/**/.result/**/);
        progress(start);
        if (bindata.length > 0) {
          const data = {
            "page": "backups",
            "rq": "restoreAppend",
            "data": B64.encodeBytes(bindata)
          };
          client.send(data, rp => {
            start += step;
            var blob = file.slice(start, start + step);
            reader.readAsArrayBuffer(blob);
          });
        } else {
          progress(file.size/**/);
          const data = {"page": "backups", "rq": "restoreEnd"};
          client.send(data, (rp) => {
            const fail = rp["fail"];
            if (fail === "restore:unzip") {
              alert(_("Fail unzipping backup"));
            } else if (fail === "restore:version") {
              alert(_("File is not a Hconta backup"));
            }
            main.run();
          });
        }
      }
    }

    function append() {
      var blob = file.slice(start, start + step);
      reader.readAsArrayBuffer(blob);
    }

    const data = {"page": "backups", "rq": "restoreStart"};
    client.send(data, () => {
      append();
    });
  }

  /** @return {void} */
  clearTrash () {
    const self = this;
    const client = self._main.client();
    const data = {"page": "backups", "rq": "clearTrash"};
    client.send(data, () => {
      self._main.run();
    });
  }

  /**
   * @param {function(!Array<string>):void} f
   * @return {void}
   */
  backups (f) {
    const self = this;
    const client = self._main.client();
    const data = {"page": "backups", "rq": "backups"};
    client.send(data, rp => {
      f(rp["backups"]);
    });
  }

  /**
   * @param {string} file
   * @return {void}
   */
  autorestore (file) {
    const self = this;
    const client = self._main.client();
    const data = {"page": "backups", "rq": "autorestore", "file": file};
    client.send(data, () => {
      self._main.run();
    });
  }

  /**
   * @param {function(!Array<string>):void} f
   * @return {void}
   */
  trash (f) {
    const self = this;
    const client = self._main.client();
    const data = {"page": "backups", "rq": "trash"};
    client.send(data, rp => {
      f(rp["trash"]);
    });
  }

  /**
   * @param {string} file
   * @return {void}
   */
  restoreTrash (file) {
    const self = this;
    const client = self._main.client();
    const data = {"page": "backups", "rq": "restoreTrash", "file": file};
    client.send(data, () => {
      self._main.run();
    });
  }

/* View --------------------------------------------------------- */

  /**
   * @param {!Array<string>} backups
   * @param {!Array<string>} trash
   * @return {!Domo}
   */
  mk (backups, trash) {
    const self = this;

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
                self.backupDownload(div);
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
        self.backupRestore(file, bar);
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
                    self.clearTrash();
                  }
                }).klass("link").html(" [ " + _("Clear") + " ]"))))
          .add($("tr")
            .add($("td").klass("frame").style("vertical-align:top")
              .add($("table")
                .addIt(It.from(backups).sort().reverse().map(f =>
                  $("tr").add($("td")
                    .add(Ui.link(ev => {
                        if (confirm(_("All the data will be replaced"))) {
                          self.autorestore(f);
                        }
                      }).klass("link").html(f)))))))
            .add($("td"))
            .add($("td").klass("frame").style("vertical-align:top")
              .add($("table")
                .addIt(It.from(trash).sort().reverse().map(f =>
                  $("tr").add($("td")
                    .add(Ui.link(ev => {
                        if (confirm(_("All the data will be replaced"))) {
                          self.restoreTrash(f);
                        }
                      }).klass("link").html(f))))))))));
    }

    return $("table")
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
    ;
  }
}
