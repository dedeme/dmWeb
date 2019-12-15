// Copyright 28-Nov-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// eslint-disable-next-line
import Client from "../dmjs/Client.js";
import Domo from "../dmjs/Domo.js"; //eslint-disable-line
import Ui from "../dmjs/Ui.js";
import Dec from "../dmjs/Dec.js";
import Main from "../Main.js";  //eslint-disable-line
import Menu from "../wgs/Menu.js";
import Wcharts from "../fleas/wgs/Wcharts.js";
import {_} from "../I18n.js";

const $ = e => Ui.$(e);

/** Ranking Main page. */
export default class RankingMain {
  /**
   * @param {!Main} main
   */
  constructor (main) {
    this._main = main;

    this._menu = new Menu(false);

    this._menuDiv = $("div");
    this._listTd = $("div").klass("frame");
    this._infoTd = $("div");
  }

  /** @return {!Main} */
  get main () {
    return this._main;
  }

  // VIEW ----------------------------------------
  // TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT

  /**
    @private
    @param {!Array<string>} dates
    @return {void}
  **/
  mkMenu (dates) {
    const menu = this._menu;
    menu.reset();

    let first = true;
    for (const d of dates) {
      if (first) first = false;
      else menu.addLeft(Menu.separator());
      menu.addLeft(Menu.mkOption(
        "_" + d, d.substring(6) + "/" + d.substring(4, 6),
        () => this.newSel(d, 0)
      ).klass("link"));
    }

    menu.addRight(Menu.mkOption(
      "_management_", _("Management"),
      () => location.assign(Main.urlBase)
    ).klass("link"));
    menu.addRight(Menu.separator());
    menu.addRight(Menu.mkOption(
      "_update_", _("Update"),
      () => this.dataUpdate()
    ).klass("link"));
  }

  /**
    @private
    @param {string} date
    @param {number} sel
    @param {!Array<?>} ls
    @return {void}
  **/
  mkList (date, sel, ls) {
    let ix = 0;
    const rows = ls.map(e => {
      const i = ix++;
      const /** boolean */ isNew = e[0];
      const /** number */ variation = e[1]; // -2 -1 0 1 2
      const img = isNew ? "rk-new"
        : variation === -2 ? "rk-down2"
          : variation === -1 ? "rk-down"
            : variation === 1 ? "rk-up"
              : variation === 2 ? "rk-up2"
                : "rk-eq"
      ;
      const /** string */ points = "  " + e[2];
      let /** string */ assets = String(e[3]);
      if (assets.length > 3)
        assets = "      " + assets.substring(0, assets.length - 3) +
          "." + assets.substring(assets.length - 3);
      const /** string */ model = e[4];
      const /** string */ flea = e[5];
      const span = i === sel
        ? Ui.link(() => this.newSel(date, i))
          .html(` <i>${model}-${flea}</i>`)
        : Ui.link(() => this.newSel(date, i)).klass("link")
          .html(` ${model}-${flea}`)
      ;
      return $("tr").add($("td")
        .add(Ui.link(() => this.charts(date, i))
          .add(Ui.img(img).style("vertical-align:top")))
        .add($("span").style("font-family:monospace")
          .html(" | " + points.substring(points.length - 3)))
        .add($("span").style("font-family:monospace")
          .html(" | " + assets.substring(assets.length - 7) + " | "))
        .add(span));
    });
    this._listTd.removeAll().add($("table")
      .adds(rows));
  }

  /**
    @private
    @param {string} date
    @param {number} sel
    @param {!Array<?>} ls
    @return {void}
  **/
  mkChartList (date, sel, ls) {
    let ix = 0;
    const rows = ls.map(e => {
      const i = ix++;
      const /** boolean */ isNew = e[0];
      const /** number */ variation = e[1]; // -2 -1 0 1 2
      const img = isNew ? "rk-new"
        : variation === -2 ? "rk-down2"
          : variation === -1 ? "rk-down"
            : variation === 1 ? "rk-up"
              : variation === 2 ? "rk-up2"
                : "rk-eq"
      ;
      const /** string */ model = e[4];
      const /** string */ flea = e[5];
      const span = i === sel
        ? Ui.link(() => this.newSel(date, i))
          .html(` <i>${model}-${flea}</i>`)
        : Ui.link(() => this.newSel(date, i)).klass("link")
          .html(` ${model}-${flea}`)
      ;
      return $("tr").add($("td")
        .add(Ui.link(() => this.charts(date, i))
          .add(Ui.img(img).style("vertical-align:top")))
        .add(span));
    });
    this._listTd.removeAll().add($("table")
      .adds(rows));
  }

  /** @private */
  paramsTable (rs, selParamNames, selParamFmts) {
    const model = rs[0];
    const id = rs[1][0][0][0] + "-" + rs[1][0][0][1] + "-" + rs[1][0][0][2];
    const params = rs[1][1];
    const head = [
      $("td").klass("header").html(_("Model")),
      $("td").klass("header").html(_("Id"))
    ];
    selParamNames.forEach(n => head.push($("td").klass("header").html(n)));
    const body = [
      $("td").klass("menu").html(model),
      $("td").klass("menu").html(id)
    ];
    selParamFmts.forEach((fmt, i) => {
      const prefix = fmt[0];
      const multiplicator = fmt[1];
      const decimals = fmt[2];
      const suffix = fmt[3];
      body.push($("td").klass("fnumber").html(
        prefix +
        new Dec(params[i] * multiplicator, decimals).toIso() +
        suffix
      ));
    });
    return $("table").att("align", "center").klass("white")
      .add($("tr").adds(head))
      .add($("tr").adds(body))
    ;
  }

  /** @private */
  resultsTable (rs, assets) {
    const buys = rs[1][0][1][1];
    const sells = rs[1][0][1][2];
    const avg = rs[1][0][2][0];
    const mdv = rs[1][0][2][1];
    const sel = rs[1][0][2][2];
    const head = [
      $("td").klass("header").html(_("Assets")),
      $("td").klass("header").html(_("Pf. Avg")),
      $("td").klass("header").html(_("Pf. MDV")),
      $("td").klass("header").html(_("Pf. Sel")),
      $("td").klass("header").html(_("Buys")),
      $("td").klass("header").html(_("Sells"))
    ];
    const body = [
      $("td").klass("fnumber").html(new Dec(assets, 2).toIso()),
      $("td").klass("fnumber").html(new Dec(avg * 100, 2).toIso() + "%"),
      $("td").klass("fnumber").html(new Dec(mdv * 100, 2).toIso() + "%"),
      $("td").klass("fnumber").html(new Dec(sel * 100, 2).toIso() + "%"),
      $("td").klass("fnumber").html(new Dec(buys, 0).toIso()),
      $("td").klass("fnumber").html(new Dec(sells, 0).toIso()),
    ];
    return $("table").att("align", "center").klass("white")
      .add($("tr").adds(head))
      .add($("tr").adds(body))
    ;
  }

  /** @private */
  mkChart (isPos, data) {
    if (isPos) {
      data = data.map(d => [d[0], 39 - d[1]]);
    }
    const dataSize = data.length;

    const backFrame = "#d9d9d9";

    const marginFrame = 5;
    const widthNumbers = 60;
    const heightNumbers = 25;

    const numberFont = "12px sans";
    const numberBaseLine = 5;
    const numberHeight = 12;

    const widthFrame = dataSize + 5 + 2 * marginFrame + widthNumbers;
    const heightFrame = 200 + 2 * marginFrame + heightNumbers;

    const x0Gr = marginFrame + widthNumbers + 0.5;
    const xnGr = widthFrame - marginFrame + 0.5;
    const widthGr = xnGr - x0Gr;

    const y0Gr = marginFrame + 0.5;
    const ynGr = heightFrame - heightNumbers + marginFrame + 0.5;
    const heightGr = ynGr - y0Gr;

    let max = 39;
    let min = 0;
    if (!isPos) {
      max = data[0][1];
      min = max;
      data.forEach(d => {
        if (d[1] > max) max = d[1];
        if (d[1] < min) min = d[1];
      });
    }

    const gap = max / 100;
    const base = Math.floor((min / gap) - 1) * gap;
    const top = Math.ceil(((max - min) / gap) + 2) * gap;
    const step = top / 4;

    // Rectangles -------------------------------------------------------------

    const cv = $("canvas")
      .att("width", widthFrame)
      .att("height", heightFrame)
      .klass("GrFrame")
      .style("background:" + backFrame);
    const ctx = cv.e.getContext("2d");

    ctx.fillStyle = "rgba(255, 255, 255)";
    ctx.fillRect(x0Gr, y0Gr, widthGr, heightGr);
    ctx.fillStyle = "rgba(0, 0, 0)";

    // Left numbers -----------------------------------------------------------

    ctx.font = numberFont;
    for (let i = 0; i < 5; ++i) {
      const tx = new Dec(
        isPos ? 40 - step * i : base + step * i,
        0
      ).toIso();
      const text = ctx.measureText(tx);
      ctx.fillText(
        tx,
        (x0Gr - text.width - 4),
        numberBaseLine + ynGr - Math.round(heightGr / 4) * i
      );

      ctx.setLineDash([4, 2]);
      if (i > 0 && i < 4) {
        ctx.beginPath();
        ctx.moveTo(x0Gr, ynGr - Math.round(heightGr / 4) * i);
        ctx.lineTo(xnGr, ynGr - Math.round(heightGr / 4) * i);
        ctx.stroke();
        ctx.closePath();
      }
      ctx.setLineDash([]);
    }

    // Closes -----------------------------------------------------------------

    const xstep = 1;
    let x = x0Gr;
    ctx.beginPath();
    ctx.moveTo(x, ynGr - (data[0][1] - base) * heightGr / top);
    data.forEach(d => {
      const v = d[1];
      const y = ynGr - (v - base) * heightGr / top;
      ctx.lineTo(x, y);
      x += xstep;
    });
    ctx.stroke();
    ctx.closePath();

    // Dates ------------------------------------------------------------------

    x = x0Gr - 0.5;
    let dPr = null;
    ctx.setLineDash([4, 2]);
    data.forEach(e => {
      const d = e[0];
      if (dPr === null) {
        dPr = d;
        return;
      }

      if (d.substring(0, 6) !== dPr.substring(0, 6)) {
        dPr = d;
        if (Number(d.substring(0, 6)) % 2 === 1) {
          ctx.beginPath();
          ctx.moveTo(x - 0.5, y0Gr);
          ctx.lineTo(x - 0.5, ynGr);
          ctx.stroke();
          ctx.closePath();

          const tx = d.substring(4, 6);
          const text = ctx.measureText(tx);
          ctx.fillText(
            tx,
            (x - text.width / 2),
            (ynGr + numberHeight)
          );
        }
      }

      x += xstep;
    });
    ctx.setLineDash([]);

    // Last rectangle ---------------------------------------------------------

    ctx.lineWidth = 1;
    ctx.strokeRect(x0Gr, y0Gr, widthGr, heightGr);

    return cv;
  }

  /** @private */
  chart1 (data) {
    return $("div").style("text-align:center").add(this.mkChart(false, data));
  }

  /** @private */
  chart2 (data) {
    return $("div").style("text-align:center").add(this.mkChart(true, data));
  }

  /** @private */
  mkInfo (selData, selParamNames, selParamFmts) {
    if (selData !== null) {
      const rs = selData[0];
      const name = rs[0] + "-" + rs[1][0][0][0];
      const assets = selData[1];
      const positions = selData[2];
      const lastAssets = assets[assets.length - 1][1];
      this._infoTd.removeAll()
        .add($("div").klass("head").html(name))
        .add(this.paramsTable(rs, selParamNames, selParamFmts))
        .add($("div").style("height:5px"))
        .add(this.resultsTable(rs, lastAssets))
        .add($("div").klass("head").html(_("Assets")))
        .add(this.chart1(assets))
        .add($("div").klass("head").html(_("Positions")))
        .add(this.chart2(positions))
      ;
    } else {
      this._infoTd.removeAll()
        .add($("div").klass("head").html("No data available"));
    }
  }

  /** private **/
  mkCharts (nicks, model, flea, selData, selParamNames, selParamFmts) {
    if (selData !== null) {
      const rs = selData[0];
      const name = rs[0] + "-" + rs[1][0][0][0];
      const assets = selData[1];
      const lastAssets = assets[assets.length - 1][1];
      const wcharts = new Wcharts(
        Wcharts.CHAMPIONS,
        model,
        nicks,
        selParamNames.length,
        flea
      );
      this._infoTd.removeAll()
        .add($("div").klass("head").html(name))
        .add(this.paramsTable(rs, selParamNames, selParamFmts))
        .add($("div").style("height:5px"))
        .add(this.resultsTable(rs, lastAssets))
        .add($("div").klass("head").html(_("Charts")))
        .add(wcharts.wg)
      ;
    } else {
      this._infoTd.removeAll()
        .add($("div").klass("head").html("No data available"));
    }
  }

  mkWgs (date, sel, list, selData, selParamNames, selParamFmts) {
    this.mkList(date, sel, list);
    this.mkInfo(selData, selParamNames, selParamFmts);
  }

  mkChartWgs (nicks, date, sel, list, selData, selParamNames, selParamFmts) {
    this.mkChartList(date, sel, list);

    const e = list[sel];
    const /** string */ model = e[4];
    const /** string */ flea = e[5];
    this.mkCharts(nicks, model, flea, selData, selParamNames, selParamFmts);
  }

  /** @return {void} */
  show () {
    this.main.view.removeAll()
      .add(this._menuDiv)
      .add($("table").klass("main")
        .add($("tr")
          .add($("td").style(
            "width: 5px;white-space: nowrap;vertical-align:top"
          ).add(this._listTd))
          .add($("td").style("vertical-align:top").add(this._infoTd))))
      .add(Ui.upTop("up"))
    ;

    this.update();
  }

  // CONTROL -------------------------------------
  // TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT

  /**
   * @private
   * @return {!Promise}
   */
  async update () {
    const rq = {
      "module": "ranking",
      "rq": "idata"
    };

    const /** !Object<string, ?> */ rp = await Main.client.rq(rq);
    const dates = rp["dates"];
    const date = rp["date"];
    const sel = 0;
    const list = rp["list"];
    const selData = rp["selData"];
    const selParamNames = rp["selParamNames"];
    const selParamFmts = rp["selParamFmts"];

    this.mkMenu(dates);
    this._menuDiv.removeAll().add(this._menu.wg);
    this._menu.setSelected("_" + date);

    this.mkWgs(date, sel, list, selData, selParamNames, selParamFmts);
  }

  /**
   * @private
   * @return {!Promise}
   */
  async dataUpdate () {
    const rq = {
      "module": "ranking",
      "rq": "dataUpdate"
    };
    await Main.client.rq(rq);
    this.update();
  }

  /**
   @private
   @param {string} date
   @param {number} sel
   @return {!Promise}
  ***/
  async newSel (date, sel) {
    const rq = {
      "module": "ranking",
      "rq": "newSel",
      "date": date,
      "sel": sel
    };
    const /** !Object<string, ?> */ rp = await Main.client.rq(rq);
    const list = rp["list"];
    const selData = rp["selData"];
    const selParamNames = rp["selParamNames"];
    const selParamFmts = rp["selParamFmts"];

    this._menu.setSelected("_" + date);
    this.mkWgs(date, sel, list, selData, selParamNames, selParamFmts);
  }

  /**
   @private
   @param {string} date
   @param {number} sel
   @return {!Promise}
  ***/
  async charts (date, sel) {
    const rq = {
      "module": "ranking",
      "rq": "charts",
      "date": date,
      "sel": sel
    };
    const /** !Object<string, ?> */ rp = await Main.client.rq(rq);
    const list = rp["list"];
    const selData = rp["selData"];
    const selParamNames = rp["selParamNames"];
    const selParamFmts = rp["selParamFmts"];
    const nicks = rp["nicks"];

    this._menu.setSelected("_" + date);
    this.mkChartWgs(
      nicks, date, sel, list, selData, selParamNames, selParamFmts
    );
  }
}
