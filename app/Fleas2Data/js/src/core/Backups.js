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
   * @return {Promise}
   */
  async backupDownload (action) {
    const data = {
      "source": "backups",
      "rq": "backup"
    };
    const rp = await this._main.client.send(data);
    action(rp["name"]);
  }

  /**
   * Restores a backup
   * @param {*} file Javascrip file object.
   * @param {function(number):void} progress For slide bar.
   * @return {Promise}
   */
  async backupRestore (file, progress) {
    const main = this._main;
    const client = main.client;
    const step = 25000;
    let start = 0;

    const reader = new FileReader();
    reader.onerror = async () => {
      alert(_args(_("'%0' can not be read"), file.name));
      const data = {
        "source": "backups",
        "rq": "restoreAbort"
      };
      await client.send(data);
      main.run();
    };
    reader.onloadend = async evt => {
      if (evt.target.readyState === FileReader.DONE) { // DONE == 2
        const bindata = new Uint8Array(evt.target.result);
        progress(start);
        if (bindata.length > 0) {
          const data = {
            "source": "backups",
            "rq": "restoreAppend",
            "data": B64.encodeBytes(bindata)
          };
          await client.send(data);
          start += step;
          const blob = file.slice(start, start + step);
          reader.readAsArrayBuffer(blob);
        } else {
          progress(file.size);
          const data = {
            "source": "backups",
            "rq": "restoreEnd"
          };
          const rp = await client.send(data);
          const fail = rp["fail"];
          if (fail === "restore:unzip") {
            alert(_("Fail unzipping backup"));
          } else if (fail === "restore:version") {
            alert(_("File is not a Selectividad backup"));
          }
          main.run();
        }
      }
    };

    function append () {
      const blob = file.slice(start, start + step);
      reader.readAsArrayBuffer(blob);
    }

    const data = {
      "source": "backups",
      "rq": "restoreStart"
    };
    await client.send(data);
    append();
  }

  /** @return {Promise} */
  async clearTrash () {
    const main = this._main;
    const data = {
      "source": "backups",
      "rq": "clearTrash"
    };
    await main.client.send(data);
    main.run();
  }

  /**
   * @param {string} f File name of backup file
   * @return {Promise}
   */
  async autorestore (f) {
    const main = this._main;
    const data = {
      "source": "backups",
      "rq": "autorestore",
      "file": f
    };
    await main.client.send(data);
    main.run();
  }

  /**
   * @param {string} f File name of trash file
   * @return {Promise}
   */
  async restoreTrash (f) {
    const main = this._main;
    const data = {
      "source": "backups",
      "rq": "restoreTrash",
      "file": f
    };
    await main.client.send(data);
    main.run();
  }

  // ____
  // View ------------------------------------------------------------
  // TTTT

  /**
   * @private
   * @param {!Array<string>} backups Automatic backups list
   * @param {!Array<string>} trash Trash list
   * @return {void}
   */
  show2 (backups, trash) {
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
              .add($("button")
                .style("width: 150px")
                .html(_("Make backup"))
                .on("click", () => {
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
      div.add($("button")
        .style("width: 150px")
        .html(_("Restore backup"))
        .on("click", () => {
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
            .add($("td").html("<b>" + _("Automatic Backups") + "</b>"))
            .add($("td").style("width:25px"))
            .add($("td")
              .add($("span").html("<b>" + _("Trash") + "</b>"))
              .add(Ui.link(() => {
                if (confirm(_("Clear trash?"))) {
                  self.clearTrash();
                }
              }).klass("link").html(" [ " + _("Clear") + " ]"))))
          .add($("tr")
            .add($("td").klass("frame").style("vertical-align:top")
              .add($("table").att("align", "center")
                .adds(backups.sort().reverse().map(f =>
                  $("tr").add($("td")
                    .add(Ui.link(() => {
                      if (confirm(_("All the data will be replaced"))) {
                        self.autorestore(f);
                      }
                    }).klass("link").html(f)))))))
            .add($("td"))
            .add($("td").klass("frame").style("vertical-align:top")
              .add($("table").att("align", "center")
                .adds(trash.sort().reverse().map(f =>
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
   * @return {Promise}
   */
  async show () {
    const self = this;
    const rq = {
      "source": "backups",
      "rq": "lists"
    };
    const rp = await self._main.client.send(rq);
    const backups = rp["backups"];
    const trash = rp["trash"];
    self.show2(backups, trash);
  }

}
