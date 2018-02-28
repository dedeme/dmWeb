// Copyright 8-Dec-2017 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

goog.provide("view_Bests");

goog.require("Flea");

view_Bests = class {
  /**
   * @param {!Main} control
   * @param {!Array<!Array<?>>} bests
   */
  constructor (control, bests) {
    /** @private */
    this._control = control;

    /**
     * @private
     * @type {!Object<string,!Array<Flea>>}
     */
    this._bests = {};
    It.from(bests).each(cycleBests => {
        const cycle = /** @type {string} */ (cycleBests[0]);
        const fsSerial =
          /** @type {!Array<!Array<*>>} */ (cycleBests[1]);
        const fleas = It.from(fsSerial).map(s => Flea.restore(s)).to();
        this._bests[cycle] = fleas;
    });

    /**
     * @private
     * @type {!Array<string>}
     */
    this._keys = It.keys(this._bests).sortf((a, b) => +a < +b ? 1 : -1).to();

    this._interval = setInterval(function () {
      if (control.conf().page() !== "bests") {
        clearInterval(this._interval);
      } else {
        control.updateBests(() => {
          clearInterval(this._interval);
          control.run2();
        });
      }
    }, 10000)

  }

  /**
   * @return {void}
   */
  show () {
    const self = this;
    const control = self._control;
    const conf = control.conf();

    const bests = self._bests;
    const keys = self._keys;

    let selected = conf.subpage();
    if (selected === "last" || !It.from(keys).contains(selected)) {
      selected = this._keys[0];
    }

    function evalCont(fid, post) {
      return post === null
        ? "blank"
        : It.from(post).containsf(e => e != null && e.id() === fid)
          ? "well"
          : "cross"
      ;
    }

    function evalUpDown(fid, ix, pre) {
      if (pre === null) {
        return "blank";
      }
      const preIx = It.from(pre).indexf(e => e != null && e.id() === fid);
      if (preIx === -1) {
        return "flag";
      }
      const dif = preIx - ix;
      return dif > 10 ? "up2"
        : dif > 5 ? "up"
        : dif < -10 ? "down2"
        : dif < -5 ? "down"
        : "blank"
    }

    function th() {
      return $("td").klass("frame4").style("font-family:monospace;");
    }
    function thl() {
      return $("td").klass("frame4")
        .style("text-align:right;font-family:monospace;");
    }
    function td() {
      return $("td").klass("frame");
    }
    function tdl() {
      return $("td").klass("frame").style("text-align:right");
    }

    let span = 1;
    It.from(bests[selected]).each(f => {
      if (f != null && f.extra().fields() > span) {
        span = f.extra().fields();
      }
    })
    const thSpecialData = $("td").klass("frame4").att("colspan", span)
      .style("font-family:monospace;").html(_("Special data"));

    function toTrs(key) {
      function intFormat (n) {
        return conf.fDec(new Dec(n, 0))
      }
      function floatFormat (n) {
        return conf.fDec(new Dec(n, 2))
      }
      /**
       * @param {Flea} f
       * @param {number} span
       */
      function specialCols (f, span) {
        if (f === null) {
          return It.from([td().att("colspan", span)]);
        }
        return f.extra().bests()(intFormat, floatFormat, span);
      }

      const bs = bests[key];
      const postBests = key === keys[0]
        ? null
        : bests[keys[It.from(keys).index(key) - 1]];
      const preBests = key === keys[keys.length - 1]
        ? null
        : bests[keys[It.from(keys).index(key) + 1]];
      const ls = [];
      const rs = [];

      let i = 1;
      return It.from(bs).map(f => {
        return f === null
        ? $("tr")
            .add(td().html(""))
            .add(td().html(""))
            .add(td().html(""))
            .add(td().html(""))
            .add(td().html(""))
            .add(td().html(""))
            .add(td().html(""))
            .add(td().html(""))
            .addIt(specialCols(null, 0))
            .add(td().html(""))
            .add(td().html(""))
            .add(td().html(""))
        : $("tr")
            .add(td().add(Ui.img(evalCont(f.id(), postBests))))
            .add(td().add(Ui.img(evalUpDown(f.id(), i - 1, preBests))))
            .add(tdl().html(i++))
            .add(tdl().html(intFormat(f.id())))
            .add(td().html(Flea.familyNames(f.family())))
            .add(tdl().html(intFormat(f.cycle())))
            .add(tdl().html(intFormat(5000 + f.bet() * 1000)))
            .add(td().html(
                f.ibex() === 0 ? _("Out")
                : f.ibex() === 1 ? _("In") : _("Mix")
              ))
            .addIt(specialCols(f, span))
            .add(tdl().html(floatFormat(f.stats().cash())))
            .add(tdl().html(intFormat(f.stats().buys())))
            .add(tdl().html(intFormat(f.stats().sells())))
      });
    }

    const viewer = $("table").att("align", "center")
      .style("border-collapse:collapse;border:0px;")
      .add($("tr")
        .add(th())
        .add(th())
        .add(thl().html(_("Ix")))
        .add(thl().html(_("Id")))
        .add(th().html(_("Type")))
        .add(thl().html(_("Cycle")))
        .add(thl().html(_("Bet")))
        .add(th().html(_("Ibex")))
        .add(thSpecialData)
        .add(thl().html(_("Incomes")))
        .add(thl().html(_("Buys")))
        .add(thl().html(_("Sells"))))
      .addIt(toTrs(selected))
    ;

    const left = $("td").style("width:5px;vertical-align:top;")
      .add($("table").klass("frame")
        .add($("tr").add($("td").html(_("CYCLES"))))
        .add($("tr").add($("td").html("<hr>")))
        .addIt(It.from(keys).map(k =>
            $("tr").add($("td").style("text-align:right")
              .add(Ui.link(ev => {
                  control.setBestsId(k === keys[0] ? "last" : k);
                }).klass("link")
                .html(k === selected ? "<b>" + k + "</b>" : k))
          ))))
    ;

    const right = $("td").style("text-align:center;vertical-align:top")
      .add($("h2").html(_("Bests")))
      .add(viewer)
    ;

    control.dom().show(
      "bests",
      $("table").klass("main").add($("tr").add(left).add(right))
    );
  }

}

