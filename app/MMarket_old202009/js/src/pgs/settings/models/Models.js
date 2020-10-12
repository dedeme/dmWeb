// Copyright 04-Jun-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import It from "../../../dmjs/It.js";
import Ui from "../../../dmjs/Ui.js";
import Domo from "../../../dmjs/Domo.js";  //eslint-disable-line
import {Menu} from "../../../dmjs/Menu.js";
import Cts from "../../../data/Cts.js";
import {_, _args} from "../../../I18n.js";
import Manager from "../../../data/Manager.js";
import Fmodel from "../../../data/flea/Fmodel.js";
import Params from "../../../wgs/Params.js";

const $ = e => Ui.$(e);

/**
    Investors models management.
**/
export default class Models {

  /**
      @param {!Domo} wg
      @param {number} investors
      @param {number} investor
      @param {!Array<!Fmodel>} models
      @param {!Manager} manager
  **/
  constructor (wg, investors, investor, models, manager) {
    this._wg = wg;
    this._investors = investors;
    this._investor = investor;
    this._sinvestor = `${_("Inv")}-${investor}`;
    models.sort((m1, m2) => m1.id > m2.id ? 1 : -1);
    this._models = models;
    this._manager = manager;

    this._editorDiv = $("div");
    this._paramsDiv = $("div");
    this.view();
  }

  // View ----------------------------------------------------------------------

  /**
    @private
    @param {string} nick
    @param {!Fmodel} model
    @param {!Array<number>=} values
    @return void
  **/
  paramsView (nick, model, values) {
    const sel = $("select");
    this._models.forEach(m => {
      const op = $("option").text(m.id);
      if (m.id === model.id) {
        op.att("selected", true);
      }
      sel.e.add(op.e);
    });
    sel.on(
      "change",
      () => this.paramsView(nick, this._models[sel.e.selectedIndex])
    );


    const params = values === undefined
      ? new Params(
        model.parNames, model.parMaxs, model.parMins, "ps", "accept"
      )
      : new Params(
        model.parNames, model.parMaxs, model.parMins, "ps", "accept", values
      )
    ;

    this._paramsDiv
      .removeAll()
      .add($("table")
        .add($("tr")
          .add($("td")
            .add(sel))
          .add($("td")
            .style("text-align:right")
            .add(Ui.link(() => {
              this.paramsView(
                nick,
                this._manager.base.model,
                this._manager.base.params
              );
            }).klass("link")
              .text(_("Default Model")))))
        .add($("tr")
          .add($("td")
            .att("colspan", 2)
            .add(params.wg)))
        .add($("tr")
          .add($("td")
            .att("colspan", 2)
            .style("text-align:center")
            .add($("button")
              .text(_("Cancel"))
              .on("click", () => { this.cancel() }))
            .add($("span").text(" "))
            .add($("button")
              .att("id", "accept")
              .text(_("Accept"))
              .on("click", () => {
                this.accept(nick, model, params.value);
              })))))
    ;
  }

  /**
      @private
      @param {string} nick
      @return void
  **/
  editorView (nick) {
    const v = nick === "" ? this._manager.base : this._manager.nicks[nick];

    this.paramsView(nick, v.model, v.params);

    this._editorDiv
      .removeAll()
      .add($("div")
        .klass("head")
        .text(nick === "" ? _("Default Model") : _args(_("%0 Model"), nick)))
      .add($("table")
        .att("align", "center")
        .add($("tr")
          .add($("td")
            .att("colspan", 2)
            .add(this._paramsDiv))))
      .add($("hr"))
    ;
  }

  /**
      @private
  **/
  view () {
    const base = this._manager.base;
    const nicks = this._manager.nicks;
    const knicks = Object.keys(nicks);
    knicks.sort();

    const lb = `${_("Inv")}-0`;
    const lopts = [Menu.toption(lb, lb, () => { this.selInvestor(0) })];
    for (let i = 1; i < this._investors; ++i) {
      const lb = `${_("Inv")}-${i}`;
      lopts.push(Menu.separator());
      lopts.push(Menu.toption(lb, lb, () => { this.selInvestor(i) }));
    }
    const menu = new Menu(lopts, [], this._sinvestor);

    const nPar = Object.values(this._manager.nicks).reduce(
      (r, v) => v.params.length > r ? v.params.length : r, 0
    );
    this._wg
      .removeAll()
      .add(menu.wg)
      .add(this._editorDiv)
      .add($("div")
        .klass("head")
        .html(_("Default Model")))
      .add($("table")
        .klass("white")
        .att("align", "center")
        .add($("tr")
          .add($("td")
            .klass("header")
            .html(_("Model")))
          .adds(base.model.parNames.map(n =>
            $("td")
              .klass("header")
              .html(n))))
        .add($("tr")
          .add($("td")
            .klass("border")
            .add(Ui.link(() => this.edit(""))
              .klass("link")
              .html(base.model.id)))
          .adds([...It.range(base.params.length)].map(ix =>
            $("td")
              .klass("number")
              .text(Cts.nformat(base.params[ix], base.model.parDecs[ix]))))))
      .add($("div")
        .klass("head")
        .html(_("Nick models")))
      .add($("table")
        .klass("white")
        .att("align", "center")
        .add($("tr")
          .add($("td")
            .klass("header")
            .html(_("Nick")))
          .add($("td")
            .klass("header")
            .html(_("Model")))
          .adds([...It.range(nPar)].map(n =>
            $("td")
              .klass("header")
              .html("P. " + String(n + 1))))
          .add($("td")
            .klass("header")
            .text("·")))
        .adds(knicks.map(k => {
          const v = nicks[k];
          return $("tr")
            .add($("td")
              .klass("border")
              .text(k))
            .add($("td")
              .klass("border")
              .add(Ui.link(() => this.edit(k))
                .klass("link")
                .text(v.model.id)))
            .adds([...It.range(nPar)].map(ix =>
              ix >= v.params.length
                ? $("td")
                  .klass("border")
                : $("td")
                  .klass("number")
                  .text(Cts.nformat(v.params[ix], v.model.parDecs[ix]))
            ))
            .add($("td")
              .klass("border")
              .add(Ui.img(v.eqParams(base) ? "blank" : "warning")))
          ;
        })))

    ;
  }

  // Controls ------------------------------------------------------------------

  /**
      @private
      @param {number} investor
  **/
  selInvestor (investor) {
    Models.mk(this._wg, investor);
  }

  /**
      @private
      @param {string} nick
      @return void
  **/
  edit (nick) {
    this.editorView(nick);
    window.scroll(0, 0);
  }

  /**
      @private
  **/
  cancel () {
    this._editorDiv.removeAll();
  }

  /**
      @private
      @param {string} nick
      @param {!Fmodel} model
      @param {!Array<number>} params
  **/
  async accept (nick, model, params) {
    await Cts.client.ssend({
      "module": "settings",
      "source": "models",
      "rq": "update",
      "investor": this._investor,
      "nickName": nick,
      "modelId": model.id,
      "params": params
    });
    Models.mk(this._wg, this._investor);
  }

  // Static --------------------------------------------------------------------

  /**
      @param {!Domo} wg
      @param {number} investor
      @return !Models
  **/
  static async mk (wg, investor) {
    const rp = await Cts.client.ssend({
      "module": "settings",
      "source": "models",
      "rq": "idata",
      "investor": investor
    });
    const /** !Array<!Fmodel> */ models =
      rp["models"].map(e => Fmodel.fromJs(e));
    const /** !Manager */ manager = Manager.fromJs(rp["manager"]);
    const /** number */ investors = rp["investors"];

    return new Models(wg, investors, investor, models, manager);
  }
}
