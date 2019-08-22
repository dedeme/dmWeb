// Copyright 05-Sept-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import Domo from "../dmjs/Domo.js"; //eslint-disable-line
import B64 from "../dmjs/B64.js";
import Main from "../Main.js";
import SysMain from "./SysMain.js"; //eslint-disable-line
import {_, _args} from "../I18n.js";
import Ui from "../dmjs/Ui.js";

const $ = e => Ui.$(e);

/** Backups page. */
export default class Backups {

  // CONSTRUCTOR ---------------------------------
  // TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT

  /**
   * @param {!SysMain} sysMain Main page
   */
  constructor (sysMain) {
    /** @private */
    this._sysMain = sysMain;

    /**
     * @private
     * @type {!Array<string>}
     */
    this._backups = [];

    /**
     * @private
     * @type {!Array<string>}
     */
    this._trash = [];

    // VIEW -------

    /** @private */
    this._downloadFileName = $("div");

    /** @private */
    this._restoreInput = $("input").att("type", "file").klass("frame");

    /** @private */
    this._restoreButtonDiv = $("div");

    /** @private */
    this._restoreBar = $("div")
      .style("text-align:left;width:200px;background-color:#cccccc")
      .klass("frame")
      .add($("div")
        .style("background-color:#000080;width:0px;height:6px"));

  }

  // METHODS -------------------------------------
  // TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT

  /**
   * Downloads a backup
   * @private
   * @return {!Promise}
   */
  async backupDownload () {
    this._downloadFileName.removeAll()
      .add($("img").att("src", "img/wait.gif"));

    const data = {
      "module": "sys",
      "source": "Backups",
      "rq": "backup"
    };
    const /** !Object<string, string> */ rp = await Main.client.send(data);
    const fileName = rp["name"];

    this._downloadFileName
      .html("<a href='tmp/" + fileName + "'>backup.zip</a>");
  }

  /**
   * Restores a backup
   * @private
   * @return {!Promise}
   */
  async backupRestore () {

    /**
     * @param {number} n Bytes read
     * @param {number} fileSize Total bytes
     * @return {!Domo}
     */
    function progress (n, fileSize) {
      const pc = Math.round(n * 100 / fileSize);
      return $("div")
        .style("background-color:#000080;width:" + pc + "%;height:6px");
    }

    const client = Main.client;

    const n = this._restoreInput.e.files.length;
    if (n === 0) {
      alert(_("Backup file is missing"));
      return;
    }
    if (n !== 1) {
      alert(_("Only one file can be selected"));
      return;
    }

    const file = this._restoreInput.e.files[0];
    if (file.size === 0) {
      alert(_args(_("'%0' is an empty file"), file.name));
    }
    if (!confirm(_("All the data will be replaced"))) {
      return;
    }

    this._restoreButtonDiv.removeAll().add(this._restoreBar);

    const step = 25000;
    let /** number */ start = 0;

    const reader = new FileReader();

    reader.onerror = async () => {
      alert(_args(_("'%0' can not be read"), file.name));
      const data = {
        "module": "sys",
        "source": "Backups",
        "rq": "restoreAbort"
      };
      await client.send(data);
      new Backups(this._sysMain).show();
    };

    reader.onloadend = async (/** Event */ evt) => {
      if (evt.target.readyState === FileReader.DONE) { // DONE == 2
        const bindata = new Uint8Array(evt.target.result);
        this._restoreBar.removeAll().add(progress(start, file.size));
        if (bindata.length > 0) {
          const data = {
            "module": "sys",
            "source": "Backups",
            "rq": "restoreAppend",
            "data": B64.encodeBytes(bindata)
          };
          await client.send(data);
          start += step;
          const blob = file.slice(start, start + step);
          reader.readAsArrayBuffer(blob);
        } else {
          this._restoreBar.removeAll().add(progress(file.size, file.size));
          const data = {
            "module": "sys",
            "source": "Backups",
            "rq": "restoreEnd"
          };
          const rp = await client.send(data);
          const fail = rp["fail"];
          if (fail === "wrong") {
            alert(_("Backup version is wrong"));
          } else if (fail === "missing") {
            alert(_("Backup version is missing"));
          } else {
            this._sysMain.main.update();
          }
          new Backups(this._sysMain).show();
        }
      }
    };

    /** @return void */
    function append () {
      const blob = file.slice(start, start + step);
      reader.readAsArrayBuffer(blob);
    }

    const data = {
      "module": "sys",
      "source": "Backups",
      "rq": "restoreStart"
    };
    await client.send(data);
    append();
  }

  /**
   * @private
   * @return {Promise}
   */
  async clearTrash () {
    if (!confirm(_("Clear trash?"))) {
      return;
    }

    const data = {
      "module": "sys",
      "source": "Backups",
      "rq": "clearTrash"
    };
    await Main.client.send(data);
    new Backups(this._sysMain).show();
  }

  /**
   * @private
   * @param {string} f File name of backup file
   * @return {!Promise}
   */
  async autorestore (f) {
    if (!confirm(_("All the data will be replaced"))) {
      return;
    }

    const data = {
      "module": "sys",
      "source": "Backups",
      "rq": "autorestore",
      "file": f
    };
    await Main.client.send(data);
    this._sysMain.main.update();
  }

  /**
   * @private
   * @param {string} f File name of trash file
   * @return {!Promise}
   */
  async restoreTrash (f) {
    if (!confirm(_("All the data will be replaced"))) {
      return;
    }

    const data = {
      "module": "sys",
      "source": "Backups",
      "rq": "restoreTrash",
      "file": f
    };
    await Main.client.send(data);
    this._sysMain.main.update();
  }

  // VIEW ----------------------------------------
  // TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT

  /**
   * @private
   * @return {!Domo}
   */
  get wg () {

    const download = $("tr").add($("td").att("colspan", 2)
      .add($("table").att("align", "center")
        .add($("tr")
          .add($("td").style("width:250px").klass("frame")
            .add(this._downloadFileName)))
        .add($("tr")
          .add($("td")
            .add($("button")
              .style("width: 150px")
              .html(_("Make backup"))
              .on("click", this.backupDownload.bind(this)))))));

    const upload = $("tr").add($("td").att("colspan", 2)
      .add($("table").att("align", "center")
        .add($("tr")
          .add($("td").add(this._restoreInput)))
        .add($("tr")
          .add($("td").att("align", "center")
            .add(this._restoreButtonDiv
              .add($("button")
                .style("width: 150px")
                .html(_("Restore backup"))
                .on("click", this.backupRestore.bind(this))))))));

    const lists = $("tr").add($("td").att("colspan", 2)
      .add($("table").att("align", "center")
        .add($("tr")
          .add($("td")
            .add($("div").klass("head").html(_("Automatic Backups"))))
          .add($("td").style("width:25px"))
          .add($("td")
            .add($("div").klass("head").html(_("Trash")))
            .add(Ui.link(this.clearTrash.bind(this))
              .klass("link").html(" [ " + _("Clear") + " ]"))))
        .add($("tr")
          .add($("td").klass("frame").style("vertical-align:top")
            .add($("table").att("align", "center")
              .adds(this._backups.sort().reverse().map(f =>
                $("tr").add($("td")
                  .add(Ui.link(() => { this.autorestore(f) })
                    .klass("link").html(f.substring(0, f.length - 4))))))))
          .add($("td"))
          .add($("td").klass("frame").style("vertical-align:top")
            .add($("table").att("align", "center")
              .adds(this._trash.sort().reverse().map(f =>
                $("tr").add($("td")
                  .add(Ui.link(() => { this.restoreTrash(f) })
                    .klass("link").html(f.substring(0, f.length - 4)))))))))));

    return $("table")
      .style("width:100%;text-align:center")
      .add($("tr").add($("td").att("colspan", 2)
        .add($("div").klass("head").html("<b>" + _("Backups") + "<b>"))))
      .add($("tr")
        .add($("td").style("width:5px;white-space: nowrap;text-align:right")
          .html(_("Download")))
        .add($("td").add($("hr"))))
      .add(download)
      .add($("tr")
        .add($("td").style("width:5px;white-space: nowrap;text-align:right")
          .html(_("Restore")))
        .add($("td").add($("hr"))))
      .add(upload)
      .add($("tr")
        .add($("td").att("colspan", 2).add($("hr"))))
      .add(lists)
    ;
  }

  /** return {!Promise} */
  async show () {
    const rq = {
      "module": "sys",
      "source": "Backups",
      "rq": "lists"
    };
    const rp = await Main.client.send(rq);
    this._backups = rp["backups"];
    this._trash = rp["trash"];

    this._sysMain.view.removeAll().add(this.wg);
  }

}
