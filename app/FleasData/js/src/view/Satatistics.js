// Copyright 8-Dec-2017 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

goog.provide("view_Statistics");

goog.require("view_FleasTable");

view_Statistics = class {
  /**
   * @param {!Main} control
   */
  constructor (control) {
    /** @private */
    this._control = control;

    this._interval = setInterval(function () {
      if (control.conf().page() !== "statistics") {
        clearInterval(this._interval);
      } else {
        control.updateStatistics(() => {
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
    const client = control.client();
    const conf = control.conf();
    const subpage = conf.subpage();

    const generalDataDiv = $("div");

    function intFormat (n) {
      return conf.fDec(new Dec(n, 0))
    }
    function floatFormat (n) {
      return conf.fDec(new Dec(n, 2))
    }

    function left() {
      function entry(id, text) {
        return $("tr").add($("td").style("text-align:left")
          .add(id === subpage
            ? $("span").html("<b>" + text + "</b>")
            : Ui.link(ev => {
                control.setStatisticsSelection(id);
              }).klass("link").html(text)
          ));
      }

      return $("table")
        .add($("tr").add(entry("", _("All"))))
        .addIt(It.range(Flea.familyNumber())
          .map(fn => new Tp("" + fn, Flea.familyNames(fn)))
          .sortf((tp1, tp2) => tp1.e2() > tp2.e2() ? 1 : - 1)
          .map(tp => entry(tp.e1(), tp.e2())))
      ;
    }

    function generalData () {
      const ft = subpage === "" ? f => true : f => f.family() === +subpage;
      let n = 0;
      const familyNumber = {};
      const fleas = [];
      const highValue = 99999e99;

      let cycleAvg = 0;
      let cycleMax = 0;
      let cycleMin = highValue;

      let betAvg = 0;
      let betMax = 0;
      let betMin = highValue;

      let ibexAvg = 0;
      let ibexMax = 0;
      let ibexMin = 1;

      let incomesAvg = 0;
      let incomesMax = 0;
      let incomesMin = highValue;

      function showGeneralData() {
        const fleasTableDiv = $("div");
        function th() {
          return $("td").klass("frame4")
            .style("font-family:monospace;text-align:right");
        }
        function td() {
          return $("td").klass("frame")
            .style("font-family:monospace;text-align:right");
        }

        fleas.sort((f1, f2) => f2.stats().cash() - f1.stats().cash());

        generalDataDiv.removeAll()
          .add($("table").att("align", "center")
            .addIt(It.keys(familyNumber)
              .sortf((f1, f2) =>
                Flea.familyNames(+f1) > Flea.familyNames(+f2) ? 1 : -1
              )
              .map(fn =>
                $("tr")
                  .add($("td").klass("frame4").style("text-align:left")
                    .html(Flea.familyNames(+fn)))
                  .add($("td").klass("frame").style("text-align:left")
                    .html(familyNumber[fn]))
              )))
          .add($("p").html(" "))
          .add($("table").klass("summary").att("align", "center")
            .add($("tr")
              .add($("td"))
              .add(th().html("Cycle"))
              .add(th().html("Bet"))
              .add(th().html("Ibex"))
              .add(th().html("Incomes"))
            ).add($("tr")
              .add(th().style("text-align:left").html(_("Average")))
              .add(td().html(floatFormat(cycleAvg / n)))
              .add(td().html(floatFormat(betAvg / n)))
              .add(td().html(floatFormat(ibexAvg / n)))
              .add(td().html(floatFormat(incomesAvg / n)))
            ).add($("tr")
              .add(th().style("text-align:left").html(_("Maximum")))
              .add(td().html(floatFormat(cycleMax)))
              .add(td().html(floatFormat(betMax)))
              .add(td().html(floatFormat(ibexMax)))
              .add(td().html(floatFormat(incomesMax)))
            ).add($("tr")
              .add(th().style("text-align:left").html(_("Minimum")))
              .add(td().html(floatFormat(cycleMin)))
              .add(td().html(floatFormat(betMin)))
              .add(td().html(floatFormat(ibexMin)))
              .add(td().html(floatFormat(incomesMin)))
            )
          )
          .add($("p").html(" "))
          .add($("table").att("align", "center")
            .addIt(It.range(Math.floor(fleas.length / 1000) + 1)
              .map(i => $("tr")
                .addIt(It.range(10).map(j => {
                    const min = i * 1000 + j *100;
                    if (min > fleas.length) {
                      return $("td");
                    }
                    let max = min + 100;
                    if (max > fleas.length) {
                      max = fleas.length;
                    }
                    const n = "000" + (i * 10 + j + 1);
                    return td().add(Ui.link(ev => {
                        fleasTableDiv.removeAll().add(
                          new view_FleasTable(
                            control,
                            It.from(fleas)
                              .take(max)
                              .drop(min)
                              .to()
                          ).make()
                        );
                      }).klass("link").html(n.substring(n.length - 4)));
                  })))))
          .add($("p").html(" "))
          .add(fleasTableDiv)
        ;
      }

      function averages(ix) {
        generalDataDiv.removeAll()
          .add($("img").att("src", "img/wait.gif"))
          .add($("span").html(" " + ix + " "))
          .add($("img").att("src", "img/wait.gif"))
        ;
        const data = {"rq": "readFleas", "ix": "" + ix};
        client.send(data, rp => {
          const fsSerial =
            /** @type {!Array<!Array<*>>} */ (JSON.parse(rp["fleas"]));

          if (fsSerial.length > 0) {
            It.from(fsSerial)
              .map(fserial => Flea.restore(fserial))
              .filter(flea => flea.stats().cash() > 1000)
              .filter(ft)
              .each(flea => {
                fleas.push(flea);

                const ffm = flea.family();
                const ffmN = familyNumber[ffm];
                if (ffmN) {
                  familyNumber[ffm] = ffmN + 1;
                } else {
                  familyNumber[ffm] = 1;
                }

                const cycle = flea.cycle();
                cycleAvg += cycle;
                if (cycle > cycleMax) {
                  cycleMax = cycle;
                }
                if (cycle < cycleMin) {
                  cycleMin = cycle;
                }

                const bet = 5000 + flea.bet() * 1000;
                betAvg += bet;
                if (bet > betMax) {
                  betMax = bet;
                }
                if (bet < betMin) {
                  betMin = bet;
                }

                const ibex = flea.ibex() === 2 ? 0.5 : flea.ibex();
                ibexAvg += ibex;
                if (ibex > ibexMax) {
                  ibexMax = ibex;
                }
                if (ibex < ibexMin) {
                  ibexMin = ibex;
                }

                const incomes = flea.stats().cash();
                incomesAvg += incomes;
                if (incomes > incomesMax) {
                  incomesMax = incomes;
                }
                if (incomes < incomesMin) {
                  incomesMin = incomes;
                }

                ++n;
              })
            averages(ix + 1);
          } else {
            if (cycleMin === highValue) {
              cycleMin = 0;
            }
            if (betMin === highValue) {
              betMin = 0;
            }
            if (ibexMin === highValue) {
              ibexMin = 0;
            }
            if (incomesMin === highValue) {
              incomesMin = 0;
            }

            showGeneralData();
          }
        });
      }

      averages(0);
    }

    function right() {
      return $("div")
        .add($("div").style("text-align:center")
          .add($("span").klass("frame").html(
            subpage === "" ? _("All The Fleas"): Flea.familyNames(+subpage)
          )))
        .add($("div").style("text-align:center")
          .add($("div").style("height:20px").html(""))
          .add(generalDataDiv.add($("img").att("src", "img/wait.gif")))
        );
    }

    control.dom().show("statistics", $("div").style("text-align:center")
      .add($("h2").html(_("Statistics")))
      .add($("hr"))
      .add($("table").klass("main")
        .add($("tr")
          .add($("td").klass("frame").style("vertical-align:top;width:5px")
            .add(left()))
          .add($("td").style("vertical-align:top;").add(right())))
      )
    );

    generalData();
  }
}

