// Copyright 19-06-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import Main from "../Main.js";
import SysMain from "./SysMain.js"; //eslint-disable-line
import {_, _args} from "../I18n.js";
import Domo from "../dmjs/Domo.js";  //eslint-disable-line
import Ui from "../dmjs/Ui.js";
import It from "../dmjs/It.js";
import Dec from "../dmjs/Dec.js";
import Tp from "../dmjs/Tp.js";
import NumberField from "../dmjs/NumberField.js";
//eslint-disable-next-line
import {Manager, ManagerEntry, ModelMxMn, ManagerFormat}
  from "../data/Manager.js";

const $ = e => Ui.$(e);

/** Models page. */
export default class Models {

  /**
   * @param {!SysMain} sysMain Main
   */
  constructor (sysMain) {
    this._sysMain = sysMain;

    /** @type {!Array<!ManagerEntry>} */
    this._models = [];

    // VIEW --------
    // TTTTTTTTTTTTT

    this._updateDiv = $("div");
    this._bodyDiv = $("div");
  }

  // VIEW ----------------------------------------
  // TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT

  /**
   * @private
   * @param {string} model
   * @param {function(string):void} action
   * @return {!Domo}
   */
  modelSelect (model, action) {
    const ls = this._models.map(m => m.model);
    const wg = $("select");
    ls.forEach(m => {
      const op = $("option").text(m);
      if (m === model) {
        op.att("selected", true);
      }
      wg.e.add(op.e);
    });
    wg.on("change", () => action(ls[wg.e.selectedIndex]));
    return wg;
  }

  /**
   * @private
   * @param {number} ix
   * @param {number} value
   * @param {!ModelMxMn} cf
   * @param {!ManagerFormat} fmt
   * @return {!Tp<!NumberField, !Domo>}
   */
  entry (ix, value, cf, fmt) {
    const dec = fmt.decimals + Math.log(fmt.multiplicator) / Math.log(10);
    const nf = new NumberField(false, "update-" + String(ix + 1))
      .setValue(new Dec(value, dec))
    ;

    const wg = $("table").klass("white")
      .add($("tr").add($("td").style("text-align:center").text(cf.name)))
      .add($("tr").add($("td").add($("hr"))))
      .add($("tr").add($("td").klass("example").text(String(cf.max))))
      .add($("tr").add($("td").add(nf.input.att("id", "update-" + String(ix)))))
      .add($("tr").add($("td").klass("example").text(String(cf.min))))
    ;
    return new Tp(nf, wg);
  }

  /**
   * @private
   * @param {string} nick
   * @param {!ManagerEntry} entry
   * @return {!Domo}
   */
  editDiv (nick, entry) {
    const /** !Array<!NumberField> */ inputs = [];
    const wgs = [];
    It.range(entry.params.length).each(ix => {
      const iw = this.entry(
        ix, entry.params[ix], entry.paramCfs[ix], entry.paramFmts[ix]
      );
      inputs.push(iw.e1);
      wgs.push(iw.e2);
    });
    const decs = entry.paramFmts.map(fmt =>
      fmt.decimals + Math.log(fmt.multiplicator) / Math.log(10)
    );

    return $("div")
      .add($("div").klass("head").text(
        nick === "" ? _("Default Model") : _args(_("%0 Model"), nick)
      ))
      .add($("div").style("text-align:center")
        .add(this.modelSelect(entry.model, m => {
          for (const model of this._models) {
            if (m === model.model) {
              this._updateDiv.removeAll().add(this.editDiv(nick, model));
            }
          }
        })))
      .add($("table").att("align", "center").add($("tr")
        .adds(wgs.map(w => $("td").add(w)))))
      .add($("div").style("text-align:center")
        .add($("button").text(_("Cancel")).on("click", () => this.cancel()))
        .add($("span").html("&nbsp;&nbsp;"))
        .add($("button").text(_("Accept")).on("click", () => this.accept(
          nick, entry.model, entry.paramCfs,
          inputs.map((inp, ix) => {
            const d = inp.value(decs[ix]);
            return d === null ? null : d.value;
          })))))
      .add($("hr"))
    ;
  }

  /**
   * @private
   * @param {!ManagerEntry} e
   * @return {!Domo}
   */
  defaultTable (e) {
    return $("div").style("text-align:center")
      .add($("div").klass("head").html(_("Default Model")))
      .add($("table").klass("white").att("align", "center")
        .add($("tr")
          .add($("td").klass("header").html(_("Model")))
          .adds(e.paramCfs.map(cf =>
            $("td").klass("header").html(cf.name))))
        .add($("tr")
          .add($("td")
            .add(Ui.link(() => this.edit("", e)).klass("link")
              .html(e.model)))
          .adds([...It.range(e.params.length)].map(ix => {
            const p = e.params[ix];
            const f = e.paramFmts[ix];
            return $("td").klass("number").text(
              f.prefix +
              new Dec(p * f.multiplicator, f.decimals).toIso() +
              f.suffix
            );
          }))))
    ;
  }

  /**
   * @private
   * @param {!ManagerEntry} current
   * @param {Map<string, !ManagerEntry>} es
   * @return {!Domo}
   */
  nicksTable (current, es) {
    const keys = [...es.keys()];
    keys.sort();
    const rows = keys.map(k => {
      const e = es.get(k);
      return $("tr")
        .add($("td").klass("nick border").text(k))
        .add($("td").klass("border")
          .add(Ui.link(() => this.edit(k, e)).klass("link")
            .html(e.model)))
        .adds([...It.range(4)].map(ix => {
          if (ix < e.params.length) {
            const p = e.params[ix];
            const f = e.paramFmts[ix];
            return $("td").klass("number").text(
              f.prefix +
              new Dec(p * f.multiplicator, f.decimals).toIso() +
              f.suffix
            ).att("title", e.paramCfs[ix].name);
          }
          return $("td").klass("header").text("");
        }))
        .add($("td").klass("border")
          .add(Ui.img(e.eqParams(current) ? "blank" : "warning")));
    });
    return $("div").style("text-align:center")
      .add($("div").klass("head").html(_("Nick models")))
      .add($("table").klass("white").att("align", "center")
        .add($("tr")
          .add($("td").klass("header").html(_("Nick")))
          .add($("td").klass("header").html(_("Model")))
          .adds([...It.range(4)].map((i) =>
            $("td").klass("header").html("P. " + String(i + 1))))
          .add($("td").klass("header").text("·")))
        .adds(rows))
    ;
  }

  /**
   * @return {void}
   */
  show () {
    this._sysMain.view.removeAll()
      .add($("div").style("text-align:center")
        .add(Ui.link(() => this.regularize()).klass("link frame3")
          .text(_("Regularize"))))
      .add(this._updateDiv)
      .add(this._bodyDiv)
    ;

    this.update();
  }

  // CONTROL -------------------------------------
  // TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT

  /** @private */
  async update () {
    const rq = {
      "module": "sys",
      "source": "Models",
      "rq": "idata"
    };
    const rp = await Main.client.send(rq);
    const /** !Manager */ mg = Manager.fromJs(rp["manager"]);
    const current = mg.current;
    const entries = mg.entries;
    // entries.forEach((v, k) => { console.log(v.paramCfs[0].name) });
    this._models.splice(0, this._models.length);
    rp["models"].forEach(m => this._models.push(ManagerEntry.fromJs(m)));
    this._models.sort((m1, m2) => m1.model > m2.model ? 1 : -1);
    // this._models.forEach((m) => { console.log(m.model) });

    this._updateDiv.removeAll();
    this._bodyDiv.removeAll()
      .add(this.defaultTable(current))
      .add(this.nicksTable(current, entries))
    ;
  }

  /**
   * @private
   * @param {string} nick
   * @param {!ManagerEntry} e
   */
  edit (nick, e) {
    this._updateDiv.removeAll().add(this.editDiv(nick, e));
    this._updateDiv.style("visibility:visible");
    location.assign("#");
  }

  /** @private */
  cancel () {
    this._updateDiv.removeAll();
  }

  /**
   * @private
   * @param {string} nick
   * @param {string} model
   * @param {!Array<!ModelMxMn>} cfs
   * @param {!Array<number|null>} values
   * @return {!Promise}
   */
  async accept (nick, model, cfs, values) {
    function check (name, max, min, value) {
      if (value === null) {
        return _args(_("'%0' is missing"), name);
      }
      if (value > max) {
        return _args(_("'%0' is greater than %1"), name, String(max));
      }
      if (value < min) {
        return _args(_("'%0' is less than %1"), name, String(min));
      }
      return "";
    }

    let ok = true;
    for (let ix = 0; ix < values.length; ++ix) {
      const cf = cfs[ix];
      const c = check(cf.name, cf.max, cf.min, values[ix]);
      if (c !== "") {
        alert(c);
        ok = false;
      }
    }
    if (ok) {
      const rq = {
        "module": "sys",
        "source": "Models",
        "rq": "update",
        "nick": nick,
        "model": model,
        "params": values
      };
      const rp = await Main.client.send(rq);
      const ok = rp["ok"];
      if (ok) {
        this.update();
      } else {
        alert(_("error_update_models"));
      }
    }
  }

  /**
   * @private
   * @return {!Promise}
   */
  async regularize () {
    const rq = {
      "module": "sys",
      "source": "Models",
      "rq": "regularize"
    };
    await Main.client.send(rq);
    alert(_("Models have been regularized"));
    this.update();
  }

}

