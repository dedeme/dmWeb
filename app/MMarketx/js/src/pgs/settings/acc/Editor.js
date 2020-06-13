// Copyright 19-May-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/**
    Accounting/All the investors page.
**/

import Maybe from "../../../dmjs/Maybe.js";
import It from "../../../dmjs/It.js";
import DateDm from "../../../dmjs/DateDm.js";
import Ui from "../../../dmjs/Ui.js";
import DatePicker from "../../../dmjs/DatePicker.js";
import Domo from "../../../dmjs/Domo.js";  //eslint-disable-line
import Dec from "../../../dmjs/Dec.js";
import {_, _args} from "../../../I18n.js";
import Cts from "../../../data/Cts.js";
import Annotations from "./wgs/Annotations.js";

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

const controlBuyPrice = (lang, id, value) => {
  if (
    (lang === "es" &&
      (
        !Dec.isNumberIso(value) || Dec.newIso(value, 4).value < 0 ||
        value.indexOf(".") !== -1
      )
    ) ||
    (lang === "en" &&
      (
        !Dec.isNumberEn(value) || Dec.newEn(value, 4).value < 0 ||
        value.indexOf(",") !== -1
      )
    )
  ) {
    return _args(_("'%0' is not a cero or positive number"), id);
  }
  return "";
};


/**
    Accounting/All the investors page.
**/
export default class Editor {
  /**
    @param {!Domo} wg
    @param {string } year
    @param {number} investorId
    @param {!Array<?>} anns Annotations. Each annotations is an Array:
            [0 - number] -> identifier
            [1 - date] -> date in format YYYYMMDD
            [2 - string] -> Type. One of se - bu - st - in - wi - pr - fe -
                                         pd - nd.
            ... -> More fields depending on 'Type'.
    @param {number} cash
  **/
  constructor (wg, year, investorId, anns, cash) {
    this._wg = wg;
    this._year = year;
    this._investorId = investorId;
    this._anns = anns;
    this._cash = cash;

    this._body = $("div");
    this._editor = $("div");
    this.view();
  }

  // View ----------------------------------------------------------------------

  datePicker (txf) {
    const dt = new DatePicker();
    dt.action = () => {};
    dt.lang = "es";
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
    Ui.changePoint(prf);
    const f = () => {
      let r = controlBase(_("Date"), v(dtf));
      if (r === "") r = controlBase(_("Nicks"), v(nkf));
      if (r === "") r = controlBase(_("Stocks"), v(stf));
      if (r === "") r = controlInt(_("Stocks"), v(stf));
      if (r === "") r = controlBase(_("Price"), v(prf));
      if (r === "") r = controlBuyPrice("es", _("Price"), v(prf));
      if (r === "") {
        this.sendAnn(
          "se", v(dtf), v(nkf), Number(v(stf)),
          Dec.newIso(v(prf), 4).value,
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
    Ui.changePoint(prf);
    const f = () => {
      let r = controlBase(_("Date"), v(dtf));
      if (r === "") r = controlBase(_("Nicks"), v(nkf));
      if (r === "") r = controlBase(_("Stocks"), v(stf));
      if (r === "") r = controlInt(_("Stocks"), v(stf));
      if (r === "") r = controlBase(_("Price"), v(prf));
      if (r === "") r = controlBuyPrice("es", _("Price"), v(prf));
      if (r === "") {
        this.sendAnn(
          "bu", v(dtf), v(nkf), Number(v(stf)),
          Dec.newIso(v(prf), 4).value,
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
    Ui.changePoint(amf);
    const f = () => {
      let r = controlBase(_("Date"), v(dtf));
      if (r === "") r = controlBase(_("Ammount"), v(amf));
      if (r === "") r = controlDouble("es", _("Ammount"), v(amf));
      if (r === "") {
        this.sendAnn(
          "in", v(dtf), "", 0,
          Dec.newIso(v(amf), 4).value,
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
    Ui.changePoint(amf);
    const f = () => {
      let r = controlBase(_("Date"), v(dtf));
      if (r === "") r = controlBase(_("Ammount"), v(amf));
      if (r === "") r = controlDouble("es", _("Ammount"), v(amf));
      if (r === "") {
        this.sendAnn(
          "wi", v(dtf), "", 0,
          Dec.newIso(v(amf), 4).value,
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
    Ui.changePoint(amf);
    const def = mkin();
    const f = () => {
      let r = controlBase(_("Date"), v(dtf));
      if (r === "") r = controlBase(_("Ammount"), v(amf));
      if (r === "") r = controlDouble("es", _("Ammount"), v(amf));
      if (r === "") r = controlBase(_("Description"), v(def));
      if (r === "") {
        this.sendAnn(
          "pr", v(dtf), "", 0,
          Dec.newIso(v(amf), 4).value,
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
    Ui.changePoint(amf);
    const def = mkin();
    const f = () => {
      let r = controlBase(_("Date"), v(dtf));
      if (r === "") r = controlBase(_("Ammount"), v(amf));
      if (r === "") r = controlDouble("es", _("Ammount"), v(amf));
      if (r === "") r = controlBase(_("Description"), v(def));
      if (r === "") {
        this.sendAnn(
          "fe", v(dtf), "", 0,
          Dec.newIso(v(amf), 4).value,
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
    Ui.changePoint(amf);
    const def = mkin();
    const f = () => {
      let r = controlBase(_("Date"), v(dtf));
      if (r === "") r = controlBase(_("Ammount"), v(amf));
      if (r === "") r = controlDouble("es", _("Ammount"), v(amf));
      if (r === "") r = controlBase(_("Description"), v(def));
      if (r === "") {
        this.sendAnn(
          "pd", v(dtf), "", 0,
          Dec.newIso(v(amf), 4).value,
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
    Ui.changePoint(amf);
    const def = mkin();
    const f = () => {
      let r = controlBase(_("Date"), v(dtf));
      if (r === "") r = controlBase(_("Ammount"), v(amf));
      if (r === "") r = controlDouble("es", _("Ammount"), v(amf));
      if (r === "") r = controlBase(_("Description"), v(def));
      if (r === "") {
        this.sendAnn(
          "nd", v(dtf), "", 0,
          Dec.newIso(v(amf), 4).value,
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
    ;
  }

  /**
    @private
    @return void
  **/
  view () {
    const annsWg = $("div");
    // eslint-disable-next-line
    new Annotations(annsWg, this._anns, Maybe.just(i => { this.del(i) }));

    this._editor = this.selector();

    this._wg
      .removeAll()
      .add($("div")
        .add($("div")
          .klass("head")
          .html(_("Annotations")))
        .add($("table")
          .att("align", "center")
          .klass("frame3")
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
                  .add($("td")
                    .klass("rlabel")
                    .add($("span")
                      .html(_("Cash:"))))
                  .add($("td")
                    .klass("number")
                    .text(new Dec(this._cash, 2).toIso()))
                  .add($("td"))))))
          .add($("tr")
            .add($("td").klass("frame")
              .add(annsWg)))))
    ;
  }

  // Control -------------------------------------------------------------------

  /**
    @private
    @param {number} annId
    @return !Promise<void>
  **/
  async del (annId) {
    const ann = this._anns.find(a => a[0] === annId);
    if (ann === undefined) {
      alert(_args(_("Annotation con id '%0' not found"), String(annId)));
      return;
    }
    if (ann[2] === "st") {
      alert(_("Intial stockage can not be deleted"));
      return;
    }
    if (confirm(_args(_("Delete %0?"), JSON.stringify(ann)))) {
      const rp = await Cts.client.ssend({
        "module": "settings",
        "source": "acc/editor",
        "rq": "del",
        "investorId": this._investorId,
        "annId": annId
      });
      this._anns = rp["anns"];
      this._cash = rp["cash"];
      this.view();
    }
  }

  /**
      @private
      @param {string} type
      @param {string} date
      @param {string} nick
      @param {number} i
      @param {number} db
      @param {string} str
      @return {Promise<?>}
  **/
  async sendAnn (type, date, nick, i, db, str) { // ------------------- sendAnn
    const ddm = DateDm.fromIso(date);
    const d = ddm.toBase();
    const ann =
      type === "se" || type === "bu" ? [0, d, type, nick, i, db]
        : type === "in" || type === "wi" ? [0, d, type, db]
          : [0, d, type, db, str]
    ;
    const rp = await Cts.client.ssend({
      "module": "settings",
      "source": "acc/editor",
      "rq": "new",
      "investorId": this._investorId,
      "ann": ann
    });
    this._anns = rp["anns"];
    this._cash = rp["cash"];
    this.view();
  }

  // Static --------------------------------------------------------------------

  /**
      @param {!Domo} wg
      @param {string} year
      @param {number} investorId
      @return !Promise<!Editor>
  **/
  static async mk (wg, year, investorId) {
    const rp = await Cts.client.ssend({
      "module": "settings",
      "source": "acc/editor",
      "rq": "idata",
      "year": year,
      "investorId": investorId
    });
    const /** !Array<?> */ anns = rp["anns"];
    const /** number */ cash = rp["cash"];
    return new Editor(wg, year, investorId, anns, cash);
  }
}
