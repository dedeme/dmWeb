// Copyright 19-06-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import Main from "../Main.js";
import SysMain from "./SysMain.js"; //eslint-disable-line
import {_, _args} from "../I18n.js";
import Ui from "../dmjs/Ui.js";
import It from "../dmjs/It.js";
import Dec from "../dmjs/Dec.js";
import DateDm from "../dmjs/DateDm.js";
import DatePicker from "../dmjs/DatePicker.js";

const $ = e => Ui.$(e);

const controlBase = (id, value) => {
  if (value === "") {
    return _args(_("'%0' is empty"), id);
  }
  return "";
};

const controlInt = (id, value) => {
  let r = "";
  It.range(value.length).each(c => {
    if (r === "" && (value.charAt(c) < "0" || value.charAt(c) > "9")) {
      r = _args(_("'%0' is not a positive integer"), id);
    }
  });
  return r;
};

const controlDouble = (lang, id, value) => {
  if (
    (lang === "es" &&
      (
        !Dec.isNumberIso(value) || Dec.newIso(value, 4).value <= 0 ||
        value.indexOf(".") !== -1
      )
    ) ||
    (lang === "en" &&
      (
        !Dec.isNumberEn(value) || Dec.newEn(value, 4).value <= 0 ||
        value.indexOf(",") !== -1
      )
    )
  ) {
    return _args(_("'%0' is not a positive number"), id);
  }
  return "";
};

/** Annotations page. */
export default class Annotations {

  /**
   * @param {!SysMain} sysMain Main
   */
  constructor (sysMain) {
    this._sysMain = sysMain;
    /** @type{string} */
    this._lang = sysMain.main.lang;

    // VIEW ------------------
    // TTTTTTTTTTTTTTTTTTTTTTT

    this._editor = $("div");
    this._cash = $("span");
    this._historic = $("div");
  }

  // VIEW ----------------------------------------
  // TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT

  datePicker (txf) {
    const dt = new DatePicker();
    dt.action = () => {};
    dt.lang = this._lang;
    const r = dt.makeText(txf);
    txf.style("width:90px");
    txf.value("");
    return r;
  }

  typeDate (tp, dt) {
    return $("tr").add($("td")
      .add($("table").klass("main")
        .add($("tr")
          .add($("td").klass("frame2")
            .style("text-align:center;vertical-align:bottom;")
            .html("<big><b>" + tp + "</big></b>"))
          .add($("td")
            .add($("table").att("align", "right")
              .add($("tr")
                .add($("td").style("text-align:center;").html(_("Date"))))
              .add($("tr")
                .add($("td").add(dt))))))))
    ;
  }

  cancelAccept (f) {
    const lk = (tx, f) => Ui.link(f).klass("link").html(tx);
    return $("tr")
      .add($("td").style("text-align:right;")
        .add(lk(_("Cancel"), () => {
          this._editor.removeAll().add(this.selector());
        }))
        .add($("span").html("&nbsp;&nbsp;&nbsp;"))
        .add(lk(_("Accept"), f)))
    ;
  }

  fields (fs) {
    return $("table").klass("main")
      .add($("tr")
        .adds(fs.map(e => $("td").style("text-align:center;").html(e[0]))))
      .add($("tr")
        .adds(fs.map(e => $("td").style("text-align:center;")
          .add(e[1].style(`width:${e[2]}px`)))))
    ;
  }

  sell () {
    const v = field => field.value().trim();
    const mkin = () => $("input").att("type", "text");
    const dtf = mkin();
    const nkf = mkin();
    const stf = mkin();
    const prf = mkin();
    if (this._lang === "es") Ui.changePoint(prf);
    const f = () => {
      let r = controlBase(_("Date"), v(dtf));
      if (r === "") r = controlBase(_("Nicks"), v(nkf));
      if (r === "") r = controlBase(_("Stocks"), v(stf));
      if (r === "") r = controlInt(_("Stocks"), v(stf));
      if (r === "") r = controlBase(_("Price"), v(prf));
      if (r === "") r = controlDouble(this._lang, _("Price"), v(prf));
      if (r === "") {
        this.sendAnn(
          "se", v(dtf), v(nkf), Number(v(stf)),
          this._lang === "es"
            ? Dec.newIso(v(prf), 4).value
            : Dec.newEn(v(prf), 4).value,
          ""
        );
      } else {
        alert(r);
      }
    };
    const wg = () => {
      return $("table").klass("main")
        .add(this.typeDate(_("Sell"), this.datePicker(dtf)))
        .add(this.fields(
          [[_("Nick"), nkf, 50], [_("Stocks"), stf, 40], [_("Price"), prf, 75]]
        ))
        .add(this.cancelAccept(f))
      ;
    };
    this._editor.removeAll().add(wg());
  }

  buy () {
    const v = field => field.value().trim();
    const mkin = () => $("input").att("type", "text");
    const dtf = mkin();
    const nkf = mkin();
    const stf = mkin();
    const prf = mkin();
    if (this._lang === "es") Ui.changePoint(prf);
    const f = () => {
      let r = controlBase(_("Date"), v(dtf));
      if (r === "") r = controlBase(_("Nicks"), v(nkf));
      if (r === "") r = controlBase(_("Stocks"), v(stf));
      if (r === "") r = controlInt(_("Stocks"), v(stf));
      if (r === "") r = controlBase(_("Price"), v(prf));
      if (r === "") r = controlDouble(this._lang, _("Price"), v(prf));
      if (r === "") {
        this.sendAnn(
          "bu", v(dtf), v(nkf), Number(v(stf)),
          this._lang === "es"
            ? Dec.newIso(v(prf), 4).value
            : Dec.newEn(v(prf), 4).value,
          ""
        );
      } else {
        alert(r);
      }
    };
    const wg = () => {
      return $("table").klass("main")
        .add(this.typeDate(_("Buy"), this.datePicker(dtf)))
        .add(this.fields(
          [[_("Nick"), nkf, 50], [_("Stocks"), stf, 40], [_("Price"), prf, 75]]
        ))
        .add(this.cancelAccept(f))
      ;
    };
    this._editor.removeAll().add(wg());
  }

  income () {
    const v = field => field.value().trim();
    const mkin = () => $("input").att("type", "text");
    const dtf = mkin();
    const amf = mkin();
    if (this._lang === "es") Ui.changePoint(amf);
    const f = () => {
      let r = controlBase(_("Date"), v(dtf));
      if (r === "") r = controlBase(_("Ammount"), v(amf));
      if (r === "") r = controlDouble(this._lang, _("Ammount"), v(amf));
      if (r === "") {
        this.sendAnn(
          "in", v(dtf), "", 0,
          this._lang === "es"
            ? Dec.newIso(v(amf), 4).value
            : Dec.newEn(v(amf), 4).value,
          ""
        );
      } else {
        alert(r);
      }
    };
    const wg = () => {
      return $("table").klass("main")
        .add(this.typeDate(_("Income"), this.datePicker(dtf)))
        .add(this.fields([[_("Ammount"), amf, 75]]))
        .add(this.cancelAccept(f))
      ;
    };
    this._editor.removeAll().add(wg());
  }

  withdrawal () {
    const v = field => field.value().trim();
    const mkin = () => $("input").att("type", "text");
    const dtf = mkin();
    const amf = mkin();
    if (this._lang === "es") Ui.changePoint(amf);
    const f = () => {
      let r = controlBase(_("Date"), v(dtf));
      if (r === "") r = controlBase(_("Ammount"), v(amf));
      if (r === "") r = controlDouble(this._lang, _("Ammount"), v(amf));
      if (r === "") {
        this.sendAnn(
          "wi", v(dtf), "", 0,
          this._lang === "es"
            ? Dec.newIso(v(amf), 4).value
            : Dec.newEn(v(amf), 4).value,
          ""
        );
      } else {
        alert(r);
      }
    };
    const wg = () => {
      return $("table").klass("main")
        .add(this.typeDate(_("Withdrawal"), this.datePicker(dtf)))
        .add(this.fields([[_("Ammount"), amf, 75]]))
        .add(this.cancelAccept(f))
      ;
    };
    this._editor.removeAll().add(wg());
  }

  profits () {
    const v = field => field.value().trim();
    const mkin = () => $("input").att("type", "text");
    const dtf = mkin();
    const amf = mkin();
    if (this._lang === "es") Ui.changePoint(amf);
    const def = mkin();
    const f = () => {
      let r = controlBase(_("Date"), v(dtf));
      if (r === "") r = controlBase(_("Ammount"), v(amf));
      if (r === "") r = controlDouble(this._lang, _("Ammount"), v(amf));
      if (r === "") r = controlBase(_("Description"), v(def));
      if (r === "") {
        this.sendAnn(
          "pr", v(dtf), "", 0,
          this._lang === "es"
            ? Dec.newIso(v(amf), 4).value
            : Dec.newEn(v(amf), 4).value,
          v(def)
        );
      } else {
        alert(r);
      }
    };
    const wg = () => {
      return $("table").klass("main")
        .add(this.typeDate(_("Profits"), this.datePicker(dtf)))
        .add(this.fields(
          [[_("Ammount"), amf, 75], [_("Description"), def, 220]]
        ))
        .add(this.cancelAccept(f))
      ;
    };
    this._editor.removeAll().add(wg());
  }

  fees () {
    const v = field => field.value().trim();
    const mkin = () => $("input").att("type", "text");
    const dtf = mkin();
    const amf = mkin();
    if (this._lang === "es") Ui.changePoint(amf);
    const def = mkin();
    const f = () => {
      let r = controlBase(_("Date"), v(dtf));
      if (r === "") r = controlBase(_("Ammount"), v(amf));
      if (r === "") r = controlDouble(this._lang, _("Ammount"), v(amf));
      if (r === "") r = controlBase(_("Description"), v(def));
      if (r === "") {
        this.sendAnn(
          "fe", v(dtf), "", 0,
          this._lang === "es"
            ? Dec.newIso(v(amf), 4).value
            : Dec.newEn(v(amf), 4).value,
          v(def)
        );
      } else {
        alert(r);
      }
    };
    const wg = () => {
      return $("table").klass("main")
        .add(this.typeDate(_("Fees"), this.datePicker(dtf)))
        .add(this.fields(
          [[_("Ammount"), amf, 75], [_("Description"), def, 220]]
        ))
        .add(this.cancelAccept(f))
      ;
    };
    this._editor.removeAll().add(wg());
  }

  diffP () {
    const v = field => field.value().trim();
    const mkin = () => $("input").att("type", "text");
    const dtf = mkin();
    const amf = mkin();
    if (this._lang === "es") Ui.changePoint(amf);
    const def = mkin();
    const f = () => {
      let r = controlBase(_("Date"), v(dtf));
      if (r === "") r = controlBase(_("Ammount"), v(amf));
      if (r === "") r = controlDouble(this._lang, _("Ammount"), v(amf));
      if (r === "") r = controlBase(_("Description"), v(def));
      if (r === "") {
        this.sendAnn(
          "pd", v(dtf), "", 0,
          this._lang === "es"
            ? Dec.newIso(v(amf), 4).value
            : Dec.newEn(v(amf), 4).value,
          v(def)
        );
      } else {
        alert(r);
      }
    };
    const wg = () => {
      return $("table").klass("main")
        .add(this.typeDate(_("Diff. +"), this.datePicker(dtf)))
        .add(this.fields(
          [[_("Ammount"), amf, 75], [_("Description"), def, 220]]
        ))
        .add(this.cancelAccept(f))
      ;
    };
    this._editor.removeAll().add(wg());
  }

  diffN () {
    const v = field => field.value().trim();
    const mkin = () => $("input").att("type", "text");
    const dtf = mkin();
    const amf = mkin();
    if (this._lang === "es") Ui.changePoint(amf);
    const def = mkin();
    const f = () => {
      let r = controlBase(_("Date"), v(dtf));
      if (r === "") r = controlBase(_("Ammount"), v(amf));
      if (r === "") r = controlDouble(this._lang, _("Ammount"), v(amf));
      if (r === "") r = controlBase(_("Description"), v(def));
      if (r === "") {
        this.sendAnn(
          "nd", v(dtf), "", 0,
          this._lang === "es"
            ? Dec.newIso(v(amf), 4).value
            : Dec.newEn(v(amf), 4).value,
          v(def)
        );
      } else {
        alert(r);
      }
    };
    const wg = () => {
      return $("table").klass("main")
        .add(this.typeDate(_("Diff. -"), this.datePicker(dtf)))
        .add(this.fields(
          [[_("Ammount"), amf, 75], [_("Description"), def, 220]]
        ))
        .add(this.cancelAccept(f))
      ;
    };
    this._editor.removeAll().add(wg());
  }

  close () {
    const v = field => field.value().trim();
    const mkin = () => $("input").att("type", "text");
    const dtf = mkin();
    const f = () => {
      const r = controlBase(_("Date"), v(dtf));
      if (r === "") {
        this.sendAnn("cl", v(dtf), "", 0, 0.0, "");
      } else {
        alert(r);
      }
    };
    const wg = () => {
      return $("table").klass("main")
        .add(this.typeDate(_("Close"), this.datePicker(dtf)))
        .add(this.cancelAccept(f))
      ;
    };
    this._editor.removeAll().add(wg());
  }

  selector () { // --------------------------------------------------- selector
    const td = () => $("td").style("width:50%;");
    const lk = (tx, f) => Ui.link(f).klass("link").html(tx);
    return $("table").klass("main")
      .add($("tr")
        .add(td().add(lk(_("Sell"), () => { this.sell() })))
        .add(td().add(lk(_("Buy"), () => { this.buy() }))))
      .add($("tr")
        .add(td().add(lk(_("Income"), () => { this.income() })))
        .add(td().add(lk(_("Withdrawal"), () => { this.withdrawal() }))))
      .add($("tr")
        .add(td().add(lk(_("Profits"), () => { this.profits() })))
        .add(td().add(lk(_("Fees"), () => { this.fees() }))))
      .add($("tr")
        .add(td().add(lk(_("Diff. +"), () => { this.diffP() })))
        .add(td().add(lk(_("Diff. -"), () => { this.diffN() }))))
      .add($("tr")
        .add($("td").style("text-align:center;").att("colspan", 2)
          .add(lk(_("Year close"), () => { this.close() }))))
    ;
  }

  body () { // ----------------------------------------------------------- body
    return $("div")
      .add($("div").klass("head").html(_("Annotations")))
      .add($("table").att("align", "center").klass("frame3")
        .add($("tr")
          .add($("td")
            .add(this._editor)))
        .add($("tr")
          .add($("td")
            .add($("hr"))))
        .add($("tr")
          .add($("td")
            .add($("table").att("align", "right")
              .add($("tr")
                .add($("td").klass("rlabel")
                  .add($("span").html(_("Cash:"))))
                .add($("td").klass("number")
                  .add(this._cash))
                .add($("td"))))))
        .add($("tr")
          .add($("td").klass("frame")
            .add(this._historic))))
      .add(Ui.upTop("up"))
    ;
  }

  /**
   * @return {Promise<?>}
   */
  async show () {
    this._sysMain.view.removeAll().add(this.body());
    this._editor.removeAll().add(this.selector());

    const rq = {
      "module": "sys",
      "source": "Annotations",
      "rq": "idata"
    };
    const rp = await Main.client.send(rq);
    const errors = rp["errors"];
    if (errors > 0) {
      alert(_args(_("There are wrong annotations (%0).\nSee log."), errors));
    }
    this.setData(rp["cash"], rp["annotations"]);
  }

  // CONTROL -------------------------------------
  // TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT

  /**
   * @private
   * @param {number} cash
   * @param {!Array<!Array<?>>} annotations
   */
  setData (cash, annotations) { // ------------------------------------ setData
    const fDate = d => DateDm.fromStr(d).format(
      this._lang === "es" ? "%D/%M/%Y" : "%M%D%Y"
    );
    const fNumber = (n, d) => {
      const nm = new Dec(n, d);
      return this._lang === "es" ? nm.toIso() : nm.toEn();
    };

    this._cash.removeAll().html(fNumber(cash, 2));

    const tdDate = d => $("td").klass("border").html(fDate(d));
    const tdTp = t => $("td").klass("border").html(t);
    const tdRest = s => $("td").klass("border")
      .style("text-align: left;").html(s);

    const addCl = ann => [tdDate(ann[2]), tdTp(_("CloseN"))];
    const addSeBu = ann => [
      tdDate(ann[2]),
      ann[0] === "se" ? tdTp(_("Sell")) : tdTp(_("Buy")),
      tdRest(`${ann[3]} | ${fNumber(ann[4], 0)} | ${fNumber(ann[5], 4)}`)];
    const addInWi = ann => [
      tdDate(ann[2]),
      ann[0] === "in" ? tdTp(_("Income")) : tdTp(_("Withdrawal")),
      tdRest(`${fNumber(ann[3], 2)}`)];
    const addPrFePdNd = ann => [
      tdDate(ann[2]),
      ann[0] === "pr" ? tdTp(_("Profits"))
        : ann[0] === "fe" ? tdTp(_("Fees"))
          : ann[0] === "pd" ? tdTp(_("Diff. +"))
            : tdTp(_("Diff. -")),
      tdRest(`${fNumber(ann[3], 2)} | ${ann[4]}`)];

    const addAnn = ann =>
      ann[0] === "se" || ann[0] === "bu" ? addSeBu(ann)
        : ann[0] === "in" || ann[0] === "wi" ? addInWi(ann)
          : ann[0] === "pr" || ann[0] === "fe" ||
            ann[0] === "pd" || ann[0] === "nd" ? addPrFePdNd(ann)
            : addCl(ann);

    this._historic.removeAll()
      .add($("table")
        .adds(annotations
          .map(ann =>
            $("tr")
              .add($("td")
                .add(Ui.link(() => {
                  this.removeAnn(ann);
                }).add(Ui.img("delete"))))
              .adds(addAnn(ann)))))
    ;
  }

  /**
   * @private
   * @param {Array<?>} ann
   */
  async removeAnn (ann) { // ---------------------------------------- RemoveAnn
    if (confirm(_args(_("Delete %0?"), JSON.stringify(ann)))) {
      const rq = {
        "module": "sys",
        "source": "Annotations",
        "rq": "remove",
        "id": ann[1],
        "date": ann[2]
      };
      await Main.client.send(rq);
      this.show();
    }
  }

  /**
   * @private
   * @param {string} type
   * @param {string} date
   * @param {string} nick
   * @param {number} i
   * @param {number} db
   * @param {string} str
   * @return {Promise<?>}
   */
  async sendAnn (type, date, nick, i, db, str) { // ------------------- sendAnn
    const ddm =
      this._lang === "es" ? DateDm.fromIso(date) : DateDm.fromEn(date);
    const d = ddm.toBase();
    const ann =
      type === "se" || type === "bu" ? [type, 0, d, nick, i, db]
        : type === "in" || type === "wi" ? [type, 0, d, db]
          : type === "pr" || type === "fe" ||
            type === "pd" || type === "nd" ? [type, 0, d, db, str]
            : [type, 0, d];
    const rq = {
      "module": "sys",
      "source": "Annotations",
      "rq": "new",
      "ann": ann
    };
    await Main.client.send(rq);
    this.show();
  }

}

