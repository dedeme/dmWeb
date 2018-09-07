// Copyright 05-Sept-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import B64 from "../dmjs/B64.js";
// eslint-disable-next-line
import Main from "../Main.js";
import {_, _args} from "../I18n.js";
import Ui from "../dmjs/Ui.js";

const $ = Ui.$;

/** Backups page. */
export default class Backups {
  /**
   * @param {!Main} main Main page
   */
  constructor (main) {
    /**
     * @private
     * @type {!Main}
     */
    this._main = main;
  }

  /**
   * Downloads a backup
   * @param {function(string):void} action This callback passes the name of
   *  backup file.
   * @return {void}
   */
  backupDownload (action) {
    const data = {
      "page": "backups",
      "rq": "backup"
    };
    this._main.client.send(data, rp => {
      action(rp["name"]);
    });
  }

  /**
   * Restores a backup
   * @param {*} file Javascrip file object.
   * @param {function(number):void} progress For slide bar.
   * @return {void}
   */
  backupRestore (file, progress) {
    const main = this._main;
    const client = main.client;
    const step = 25000;
    let start = 0;

    const reader = new FileReader();
    reader.onerror/**/ = () => {
      alert(_args(_("'%0' can not be read"), file.name/**/));
      const data = {
        "page": "backups",
        "rq": "restoreAbort"
      };
      client.send(data, () => {
        main.run();
      });
    };
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
          client.send(data, () => {
            start += step;
            const blob = file.slice(start, start + step);
            reader.readAsArrayBuffer(blob);
          });
        } else {
          progress(file.size/**/);
          const data = {
            "page": "backups",
            "rq": "restoreEnd"
          };
          client.send(data, (rp) => {
            const fail = rp["fail"];
            if (fail === "restore:unzip") {
              alert(_("Fail unzipping backup"));
            } else if (fail === "restore:version") {
              alert(_("File is not a Selectividad backup"));
            }
            main.run();
          });
        }
      }
    };

    function append () {
      const blob = file.slice(start, start + step);
      reader.readAsArrayBuffer(blob);
    }

    const data = {
      "page": "backups",
      "rq": "restoreStart"
    };
    client.send(data, () => {
      append();
    });
  }

  /** @return {void} */
  clearTrash () {
    const main = this._main;
    const data = {
      "page": "backups",
      "rq": "clearTrash"
    };
    main.client.send(data, () => {
      main.run();
    });
  }

  /**
   * @param {string} f File name of trash file
   * @return {void}
   */
  restoreTrash (f) {
    const main = this._main;
    const data = {
      "page": "backups",
      "rq": "restoreTrash", "file": f
    };
    main.client.send(data, () => {
      main.run();
    });
  }

  // ____
  // View ------------------------------------------------------------
  // TTTT

  /**
   * @private
   * @param {!Array<string>} list Trash list
   * @return {void}
   */
  show2 (list) {
    const self = this;
    const main = self._main;

    function download () {
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
                self.backupDownload(fileName => {
                  div.html("<a href='tmp/" + fileName + "'>backup.zip</a>");
                });
              }))))));
    }

    function upload () {
      const progress = $("div")
        .style("background-color:#000080;width:0px;height:6px");
      const bar = $("div")
        .style("text-align:left;width:200px;background-color:#cccccc")
        .klass("frame")
        .add(progress);
      const input = $("input").att("type", "file").klass("frame");
      const div = $("div");
      div.add($("button").html(_("Restore backup")).on("click", () => {
        const n = input.e.files.length;
        if (n === 0) {
          alert(_("Backup file is missing"));
          return;
        }
        if (n !== 1) {
          alert(_("Only one file can be selected"));
          return;
        }
        const file = input.e.files[0];
        if (file.size === 0) {
          alert(_args(_("'%0' is an empty file"), file.name));
        }
        if (!confirm(_("All the data will be replaced"))) {
          return;
        }
        div.removeAll().add(bar);
        self.backupRestore(file, n => {
          const pc = Math.round(n * 100 / file.size);
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

    function lists () {
      return $("tr").add($("td").att("colspan", 2)
        .add($("table").att("align", "center")
          .add($("tr")
            .add($("td")
              .add($("span").html("<b>" + _("Trash") + "</b>"))
              .add(Ui.link(() => {
                if (confirm(_("Clear trash?"))) {
                  self.clearTrash();
                }
              }).klass("link").html(" [ " + _("Clear") + " ]"))))
          .add($("tr")
            .add($("td").klass("frame").style("vertical-align:top")
              .add($("table")
                .adds(list.sort().reverse().map(f =>
                  $("tr").add($("td")
                    .add(Ui.link(() => {
                      if (confirm(_("All the data will be replaced"))) {
                        self.restoreTrash(f);
                      }
                    }).klass("link").html(f))))))))));
    }

    main.dom.show("backups", $("table")
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

  /**
   * @return {void}
   */
  show () {
    const self = this;
    const rq = {
      "page": "backups",
      "rq": "trashList"
    };
    self._main.client.send(rq, rp => {
      const list = rp["list"];
      self.show2(list);
    });
  }

}
