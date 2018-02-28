// Copyright 23-Oct-2017 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

goog.provide("view_Summaries");

goog.require("db_Dentry");

view_Summaries = class {
  /**
   * @param {!Main} control
   */
  constructor (control) {
    /** @private */
    this._control = control;
  }

  /**
   * @return {void}
   */
  show () {
    const self = this;
    const control = self._control;
    const conf = control.conf();
    const lang = conf.language();
    const summary = conf.summary();
    const db = control.db();
    const dom = control.dom();
    const dataDiv = $("div");

    const stats = {};  // {id : [des,val,{id : [des,val,{id : [des,val]}]}]}
    // {id : [des,val,{id : [des,val,{id : [des,val,{id : [des,val]}]}]}]}
    const bals = {};
    It.from(db_Balance.entries()).each(e => {
      const g = e[0].substring(0, 2);
      let group = bals[g];
      if (group === undefined) {
        group = [db_Balance.groupsGet(g), 0, {}];
        bals[g] = group;
      }
      group[2][e[0]] = [e[1], 0, {}];
    });
    // {id : [des,val,{id : [des,val,{id : [des,val,{id : [des,val]}]}]}]}
    const pls = {};
    pls["D"] = [db_PyG.groupsGet("D"), 0, {}];
    It.from(db_PyG.entries()).each(e => {
      const g = db_PyG.groupId(e[0]);
      let group = pls[g];
      if (group === undefined) {
        group = [db_PyG.groupsGet(g), 0, {}];
        pls[g] = group;
      }
      group[2][e[0]] = [e[1], 0, {}];
    });

    It.from(db.subaccounts()).each(s => {
      const g = s[0].charAt(0);
      let stat = stats[g];
      if (stat === undefined) {
        stat = [Db.groupsGet(g), 0, {}];
        stats[g] = stat;
      }
      let accs = stat[2];
      const a = s[0].substring(0, 3);
      let acc = accs[a];
      if (acc === undefined) {
        const accData = db.accountsGet(a);
        const accDes = accData[0];
        const accSum = accData[1];
        acc = [accDes, 0, {}];
        accs[a] = acc;
        if (accSum.charAt(0) === "P") {
          pls[db_PyG.groupId(accSum.substring(1))][2]
            [accSum.substring(1)][2][a] = acc;
        } else {
          bals[accSum.substring(1, 3)][2][accSum.substring(1)][2][a] = acc;
        }
      }
      acc[2][s[0]] = [s[1], 0];
    });

    It.from(db.diary()).each(e => {
      function ann(acc, v) {
        const g = stats[acc.charAt(0)];
        g[1] += v;
        const a = g[2][acc.substring(0, 3)];
        a[1] += v;
        a[2][acc][1] += v;
      }
      It.from(e.debits()).each(d => {
        ann(d.e1(), d.e2().value());
      });
      It.from(e.credits()).each(c => {
        ann(c.e1(), -c.e2().value());
      });
    });

    It.keys(pls).each(k0 => {
      let v0 = pls[k0];
      let pls1 = v0[2];
      let sum0 = 0;
      It.keys(pls1).each(k1 => {
        let v1 = pls1[k1];
        let accs = v1[2];
        let sum1 = 0;
        It.keys(accs).each(ka => {
          sum1 += accs[ka][1];
        });
        v1[1] = sum1;
        sum0 += sum1;
      });
      v0[1] = sum0;
    });
    pls["D"][1] = pls["A"][1] + pls["B"][1] + pls["C"][1];
    pls["C"][1] = pls["D"][1] - pls["C"][1];

    bals["PA"][2]["PAVII"][2]["120"][2]["12000"][1] = pls["D"][1];
    bals["PA"][2]["PAVII"][2]["120"][1] = pls["D"][1];
    It.keys(bals).each(k0 => {
      let v0 = bals[k0];
      let bals1 = v0[2];
      let sum0 = 0;
      It.keys(bals1).each(k1 => {
        let v1 = bals1[k1];
        let accs = v1[2];
        let sum1 = 0;
        It.keys(accs).each(ka => {
          sum1 += accs[ka][1];
        });
        v1[1] = sum1;
        sum0 += sum1;
      });
      v0[1] = sum0;
    });

    function menuTd(op) {
      return summary.charAt(0) === op || summary.charAt(1) === op
        ? $("td").klass("frame").style("width:100px")
        : $("td").style("width:100px")
        ;
    }

    function separator() {
      return $("td").html(" · ");
    }

    function statementsSumary() {
      return $("div").style("text-align:center")
        .add($("h2").html(_("Statements") + "<br><i>" + _("Summary") + "</i>"))
        .add($("table").klass("summary").att("align", "center")
          .addIt(It.keys(stats).sort().map(k => {
            const v = stats[k];
            const desc = v[0];
            const sum = v[1];
            return $("tr")
              .add($("td").klass("summary0cp")
                .add(Ui.link(ev => { control.goAcc(k)}).klass("link")
                  .html(k + ". " + desc)))
              .add($("td").klass("summary0d")
                .html(sum > 0 ? dom.decToStr(new Dec(sum, 2)) : ""))
              .add($("td").klass("summary0c")
                .html(sum < 0 ? dom.decToStr(new Dec(-sum, 2)) : ""));
          })));
    }

    function statementsAccounts() {
      return $("div").style("text-align:center")
        .add($("h2").html(_("Statements") + "<br><i>" + _("Accounts") + "</i>"))
        .add($("table").klass("summary").att("align", "center")
          .addIt(It.keys(stats).sort().reduce(It.empty(), (it, kG) => {
            const vG = stats[kG];
            const descG = vG[0];
            const sumG = vG[1];
            const accs = vG[2];
            return it.add(
              $("tr")
                .add($("td").att("colspan", 2).klass("summary0cp")
                  .add(Ui.link(ev => { control.goAcc(kG)}).klass("link")
                    .html("<b>" + kG + ". " + descG + "</b>")))
                .add($("td").klass("summary0d")
                  .html("<b>" +
                    (sumG > 0 ? dom.decToStr(new Dec(sumG, 2)) : "") +
                    "</b>"))
                .add($("td").klass("summary0c")
                  .html("<b>" +
                    (sumG < 0 ? dom.decToStr(new Dec(-sumG, 2)) : "") +
                    "</b>"))
            ).addIt(It.keys(accs).sort().map(kA => {
              const vA = accs[kA];
              const descA = vA[0];
              const sumA = vA[1];
              return $("tr")
                .add($("td").style("width:40px"))
                .add($("td").klass("summary0cp")
                  .add(Ui.link(ev => { control.goAcc(kA)}).klass("link")
                    .html(kA + ". " + descA)))
                .add($("td").klass("summary0d")
                  .html(sumA > 0 ? dom.decToStr(new Dec(sumA, 2)) : ""))
                .add($("td").klass("summary0c")
                  .html(sumA < 0 ? dom.decToStr(new Dec(-sumA, 2)) : ""));
            }));
          })));
    }

    function statementsSubaccounts() {
      return $("div").style("text-align:center")
        .add($("h2")
          .html(_("Statements") + "<br><i>" + _("Subaccounts") + "</i>"))
        .add($("table").klass("summary").att("align", "center")
          .addIt(It.keys(stats).sort().reduce(It.empty(), (it, kG) => {
            const vG = stats[kG];
            const descG = vG[0];
            const sumG = vG[1];
            const accs = vG[2];
            return it.add(
              $("tr")
                .add($("td").att("colspan", 3).klass("summary0cp")
                  .style("background-color: #cccccc")
                  .add(Ui.link(ev => { control.goAcc(kG)}).klass("link")
                    .html("<b><i>" + kG + ". " + descG + "</i></b>")))
                .add($("td").klass("summary0d")
                  .html("<b><i>" +
                    (sumG > 0 ? dom.decToStr(new Dec(sumG, 2)) : "") +
                    "</i></b>"))
                .add($("td").klass("summary0c")
                  .html("<b><i>" +
                    (sumG < 0 ? dom.decToStr(new Dec(-sumG, 2)) : "") +
                    "</i></b>"))
            ).addIt(It.keys(accs).sort().reduce(It.empty(), (it, kA) => {
              const vA = accs[kA];
              const descA = vA[0];
              const sumA = vA[1];
              const subs = vA[2];
              return it.add($("tr")
                .add($("td").style("width:40px"))
                .add($("td").att("colspan", 2).klass("summary0cp")
                  .add(Ui.link(ev => { control.goAcc(kA)}).klass("link")
                    .html("<b>" + kA + ". " + descA + "</b>")))
                .add($("td").klass("summary0d")
                  .html("<b>" +
                    (sumA > 0 ? dom.decToStr(new Dec(sumA, 2)) : "") +
                    "</b>"))
                .add($("td").klass("summary0c")
                  .html("<b>" +
                    (sumA < 0 ? dom.decToStr(new Dec(-sumA, 2)) : "") +
                    "</b>"))
              ).addIt(It.keys(subs).sort().map(kS => {
                const vS = subs[kS];
                const descS = vS[0];
                const sumS = vS[1];
                return $("tr")
                  .add($("td").style("width:40px"))
                  .add($("td").style("width:40px"))
                  .add($("td").klass("summary0cp")
                    .add(Ui.link(ev => { control.goAcc(kS)}).klass("link")
                      .html(Dom.accFormat(kS) + ". " + descS)))
                  .add($("td").klass("summary0d")
                    .html(sumS > 0 ? dom.decToStr(new Dec(sumS, 2)) : ""))
                  .add($("td").klass("summary0c")
                    .html(sumS < 0 ? dom.decToStr(new Dec(-sumS, 2)) : ""));
              }));
            }));
          })));
    }

    function plSumary() {
      return $("div").style("text-align:center")
        .add($("h2").html(_("P & L") + "<br><i>" + _("Summary") + "</i>"))
        .add($("table").klass("summary").att("align", "center")
          .addIt(It.keys(pls).sort().reduce(It.empty(), (it, kGG) => {
            const vGG = pls[kGG];
            const descGG = vGG[0];
            const sumGG = vGG[1];
            const groups = vGG[2];
            return it.add(
              $("tr")
                .add($("td").att("colspan", 2).klass("summary0cp")
                  .add(Ui.link(ev => { control.goAcc(kGG)}).klass("link")
                    .html("<b>" + kGG + ". " + descGG + "</b>")))
                .add($("td").klass("summary0d")
                  .html("<b>" +
                    (sumGG > 0 ? dom.decToStr(new Dec(sumGG, 2)) : "") +
                    "</b>"))
                .add($("td").klass("summary0c")
                  .html("<b>" +
                    (sumGG < 0 ? dom.decToStr(new Dec(-sumGG, 2)) : "") +
                    "</b>"))
            ).addIt(It.keys(groups).sort().map(kG => {
              const vG = groups[kG];
              const descG = vG[0];
              const sumG = vG[1];
              return $("tr")
                .add($("td").style("width:40px"))
                .add($("td").klass("summary0cp")
                  .add(Ui.link(ev => { control.goAcc(kG)}).klass("link")
                    .html(kG + ". " + descG)))
                .add($("td").klass("summary0d")
                  .html(sumG > 0 ? dom.decToStr(new Dec(sumG, 2)) : ""))
                .add($("td").klass("summary0c")
                  .html(sumG < 0 ? dom.decToStr(new Dec(-sumG, 2)) : ""));
            }));
          })));
    }

    function plAccounts() {
      return $("div").style("text-align:center")
        .add($("h2").html(_("P & L") + "<br><i>" + _("Accounts") + "</i>"))
        .add($("table").klass("summary").att("align", "center")
          .addIt(It.keys(pls).sort().reduce(It.empty(), (it, kGG) => {
            const vGG = pls[kGG];
            const descGG = vGG[0];
            const sumGG = vGG[1];
            const groups = vGG[2];
            return it.add(
              $("tr")
                .add($("td").att("colspan", 3).klass("summary0cp")
                  .style("background-color: #cccccc")
                  .add(Ui.link(ev => { control.goAcc(kGG)}).klass("link")
                    .html("<b>" + kGG + ". " + descGG + "</b>")))
                .add($("td").klass("summary0d")
                  .html("<b>" +
                    (sumGG > 0 ? dom.decToStr(new Dec(sumGG, 2)) : "") +
                    "</b>"))
                .add($("td").klass("summary0c")
                  .html("<b>" +
                    (sumGG < 0 ? dom.decToStr(new Dec(-sumGG, 2)) : "") +
                    "</b>"))
            ).addIt(It.keys(groups).sort().reduce(It.empty(), (it, kG) => {
              const vG = groups[kG];
              const descG = vG[0];
              const sumG = vG[1];
              const accs = vG[2];
              return it.add($("tr")
                .add($("td").style("width:40px"))
                .add($("td").att("colspan", 2).klass("summary0cp")
                  .add(Ui.link(ev => { control.goAcc(kG)}).klass("link")
                    .html("<b>" + kG + ". " + descG + "</b>")))
                .add($("td").klass("summary0d")
                  .html("<b>" +
                    (sumG > 0 ? dom.decToStr(new Dec(sumG, 2)) : "") +
                    "</b>"))
                .add($("td").klass("summary0c")
                  .html("<b>" +
                    (sumG < 0 ? dom.decToStr(new Dec(-sumG, 2)) : "") +
                    "</b>"))
              ).addIt(It.keys(accs).sort().map(kA => {
                const vA = accs[kA];
                const descA = vA[0];
                const sumA = vA[1];
                return $("tr")
                  .add($("td").style("width:40px"))
                  .add($("td").style("width:40px"))
                  .add($("td").klass("summary0cp")
                    .add(Ui.link(ev => { control.goAcc(kA)}).klass("link")
                      .html(kA + ". " + descA)))
                  .add($("td").klass("summary0d")
                    .html(sumA > 0 ? dom.decToStr(new Dec(sumA, 2)) : ""))
                  .add($("td").klass("summary0c")
                    .html(sumA < 0 ? dom.decToStr(new Dec(-sumA, 2)) : ""));
              }));
            }));
          })));
    }

    function plSubaccounts() {
      return $("div").style("text-align:center")
        .add($("h2").html(_("P & L") + "<br><i>" + _("Subaccounts") + "</i>"))
        .add($("table").klass("summary").att("align", "center")
          .addIt(It.keys(pls).sort().reduce(It.empty(), (it, kGG) => {
            const vGG = pls[kGG];
            const descGG = vGG[0];
            const sumGG = vGG[1];
            const groups = vGG[2];
            return it.add(
              $("tr")
                .add($("td").att("colspan", 4).klass("summary0cp")
                  .style("background-color: #cccccc")
                  .add(Ui.link(ev => { control.goAcc(kGG)}).klass("link")
                    .html("<b><i><u>" + kGG + ". " + descGG + "</u></i></b>")))
                .add($("td").klass("summary0d")
                  .html("<b><i><u>" +
                    (sumGG > 0 ? dom.decToStr(new Dec(sumGG, 2)) : "") +
                    "</u></i></b>"))
                .add($("td").klass("summary0c")
                  .html("<b><i><u>" +
                    (sumGG < 0 ? dom.decToStr(new Dec(-sumGG, 2)) : "") +
                    "</u></i></b>"))
            ).addIt(It.keys(groups).sort().reduce(It.empty(), (it, kG) => {
              const vG = groups[kG];
              const descG = vG[0];
              const sumG = vG[1];
              const accs = vG[2];
              return it.add($("tr")
                .add($("td").style("width:40px"))
                .add($("td").att("colspan", 3).klass("summary0cp")
                  .add(Ui.link(ev => { control.goAcc(kG)}).klass("link")
                    .html("<b><i>" + kG + ". " + descG + "</i></b>")))
                .add($("td").klass("summary0d")
                  .html("<b><i>" +
                    (sumG > 0 ? dom.decToStr(new Dec(sumG, 2)) : "") +
                    "</i></b>"))
                .add($("td").klass("summary0c")
                  .html("<b><i>" +
                    (sumG < 0 ? dom.decToStr(new Dec(-sumG, 2)) : "") +
                    "</i></b>"))
              ).addIt(It.keys(accs).sort().reduce(It.empty(), (it, kA) => {
                const vA = accs[kA];
                const descA = vA[0];
                const sumA = vA[1];
                const subs = vA[2];
                return it.add($("tr")
                  .add($("td").style("width:40px"))
                  .add($("td").style("width:40px"))
                  .add($("td").att("colspan", 2).klass("summary0cp")
                    .add(Ui.link(ev => { control.goAcc(kA)}).klass("link")
                      .html("<b>" + kA + ". " + descA + "</b>")))
                  .add($("td").klass("summary0d")
                    .html("<b>" +
                      (sumA > 0 ? dom.decToStr(new Dec(sumA, 2)) : "") +
                      "</b>"))
                  .add($("td").klass("summary0c")
                    .html("<b>" +
                      (sumA < 0 ? dom.decToStr(new Dec(-sumA, 2)) : "") +
                      "</b>"))
                ).addIt(It.keys(subs).sort().map(kS => {
                  const vS = subs[kS];
                  const descS = vS[0];
                  const sumS = vS[1];
                  return $("tr")
                    .add($("td").style("width:40px"))
                    .add($("td").style("width:40px"))
                    .add($("td").style("width:40px"))
                    .add($("td").klass("summary0cp")
                      .add(Ui.link(ev => { control.goAcc(kS)}).klass("link")
                        .html(Dom.accFormat(kS) + ". " + descS)))
                    .add($("td").klass("summary0d")
                      .html(sumS > 0 ? dom.decToStr(new Dec(sumS, 2)) : ""))
                    .add($("td").klass("summary0c")
                      .html(sumS < 0 ? dom.decToStr(new Dec(-sumS, 2)) : ""));
                }));
              }));
            }));
          })));
    }

    function balanceSumary() {
      return $("div").style("text-align:center")
        .add($("h2").html(_("Balance") + "<br><i>" + _("Summary") + "</i>"))
        .add($("table").klass("summary").att("align", "center")
          .addIt(It.keys(bals).sort().reduce(It.empty(), (it, kGG) => {
            const vGG = bals[kGG];
            const descGG = vGG[0];
            const sumGG = vGG[1];
            const groups = vGG[2];
            return it.add(
              $("tr")
                .add($("td").att("colspan", 2).klass("summary0cp")
                  .add(Ui.link(ev => { control.goAcc(kGG)}).klass("link")
                    .html("<b>" + descGG + "</b>")))
                .add($("td").klass("summary0d")
                  .html("<b>" +
                    (sumGG > 0 ? dom.decToStr(new Dec(sumGG, 2)) : "") +
                    "</b>"))
                .add($("td").klass("summary0c")
                  .html("<b>" +
                    (sumGG < 0 ? dom.decToStr(new Dec(-sumGG, 2)) : "") +
                    "</b>"))
            ).addIt(It.keys(groups).sort().map(kG => {
              const vG = groups[kG];
              const descG = vG[0];
              const sumG = vG[1];
              return $("tr")
                .add($("td").style("width:40px"))
                .add($("td").klass("summary0cp")
                  .add(Ui.link(ev => { control.goAcc(kG)}).klass("link")
                    .html(kG + ". " + descG)))
                .add($("td").klass("summary0d")
                  .html(sumG > 0 ? dom.decToStr(new Dec(sumG, 2)) : ""))
                .add($("td").klass("summary0c")
                  .html(sumG < 0 ? dom.decToStr(new Dec(-sumG, 2)) : ""));
            }));
          })));
    }

    function balanceAccounts() {
      return $("div").style("text-align:center")
        .add($("h2").html(_("Balance") + "<br><i>" + _("Accounts") + "</i>"))
        .add($("table").klass("summary").att("align", "center")
          .addIt(It.keys(bals).sort().reduce(It.empty(), (it, kGG) => {
            const vGG = bals[kGG];
            const descGG = vGG[0];
            const sumGG = vGG[1];
            const groups = vGG[2];
            return it.add(
              $("tr")
                .add($("td").att("colspan", 3).klass("summary0cp")
                  .style("background-color: #cccccc")
                  .add(Ui.link(ev => { control.goAcc(kGG)}).klass("link")
                    .html("<b>" + descGG + "</b>")))
                .add($("td").klass("summary0d")
                  .html("<b>" +
                    (sumGG > 0 ? dom.decToStr(new Dec(sumGG, 2)) : "") +
                    "</b>"))
                .add($("td").klass("summary0c")
                  .html("<b>" +
                    (sumGG < 0 ? dom.decToStr(new Dec(-sumGG, 2)) : "") +
                    "</b>"))
            ).addIt(It.keys(groups).sort().reduce(It.empty(), (it, kG) => {
              const vG = groups[kG];
              const descG = vG[0];
              const sumG = vG[1];
              const accs = vG[2];
              return it.add($("tr")
                .add($("td").style("width:40px"))
                .add($("td").att("colspan", 2).klass("summary0cp")
                  .add(Ui.link(ev => { control.goAcc(kG)}).klass("link")
                    .html("<b>" + kG + ". " + descG + "</b>")))
                .add($("td").klass("summary0d")
                  .html("<b>" +
                    (sumG > 0 ? dom.decToStr(new Dec(sumG, 2)) : "") +
                    "</b>"))
                .add($("td").klass("summary0c")
                  .html("<b>" +
                    (sumG < 0 ? dom.decToStr(new Dec(-sumG, 2)) : "") +
                    "</b>"))
              ).addIt(It.keys(accs).sort().map(kA => {
                const vA = accs[kA];
                const descA = vA[0];
                const sumA = vA[1];
                return $("tr")
                  .add($("td").style("width:40px"))
                  .add($("td").style("width:40px"))
                  .add($("td").klass("summary0cp")
                    .add(Ui.link(ev => { control.goAcc(kA)}).klass("link")
                      .html(kA + ". " + descA)))
                  .add($("td").klass("summary0d")
                    .html(sumA > 0 ? dom.decToStr(new Dec(sumA, 2)) : ""))
                  .add($("td").klass("summary0c")
                    .html(sumA < 0 ? dom.decToStr(new Dec(-sumA, 2)) : ""));
              }));
            }));
          })));
    }

    function balanceSubaccounts() {
      return $("div").style("text-align:center")
        .add($("h2").html(_("Balance") + "<br><i>" + _("Subaccounts") + "</i>"))
        .add($("table").klass("summary").att("align", "center")
          .addIt(It.keys(bals).sort().reduce(It.empty(), (it, kGG) => {
            const vGG = bals[kGG];
            const descGG = vGG[0];
            const sumGG = vGG[1];
            const groups = vGG[2];
            return it.add(
              $("tr")
                .add($("td").att("colspan", 4).klass("summary0cp")
                  .style("background-color: #cccccc")
                  .add(Ui.link(ev => { control.goAcc(kGG)}).klass("link")
                    .html("<b><i><u>"  + descGG + "</u></i></b>")))
                .add($("td").klass("summary0d")
                  .html("<b><i><u>" +
                    (sumGG > 0 ? dom.decToStr(new Dec(sumGG, 2)) : "") +
                    "</u></i></b>"))
                .add($("td").klass("summary0c")
                  .html("<b><i><u>" +
                    (sumGG < 0 ? dom.decToStr(new Dec(-sumGG, 2)) : "") +
                    "</u></i></b>"))
            ).addIt(It.keys(groups).sort().reduce(It.empty(), (it, kG) => {
              const vG = groups[kG];
              const descG = vG[0];
              const sumG = vG[1];
              const accs = vG[2];
              return it.add($("tr")
                .add($("td").style("width:40px"))
                .add($("td").att("colspan", 3).klass("summary0cp")
                  .add(Ui.link(ev => { control.goAcc(kG)}).klass("link")
                    .html("<b><i>" + kG + ". " + descG + "</i></b>")))
                .add($("td").klass("summary0d")
                  .html("<b><i>" +
                    (sumG > 0 ? dom.decToStr(new Dec(sumG, 2)) : "") +
                    "</i></b>"))
                .add($("td").klass("summary0c")
                  .html("<b><i>" +
                    (sumG < 0 ? dom.decToStr(new Dec(-sumG, 2)) : "") +
                    "</i></b>"))
              ).addIt(It.keys(accs).sort().reduce(It.empty(), (it, kA) => {
                const vA = accs[kA];
                const descA = vA[0];
                const sumA = vA[1];
                const subs = vA[2];
                return it.add($("tr")
                  .add($("td").style("width:40px"))
                  .add($("td").style("width:40px"))
                  .add($("td").att("colspan", 2).klass("summary0cp")
                    .add(Ui.link(ev => { control.goAcc(kA)}).klass("link")
                      .html("<b>" + kA + ". " + descA + "</b>")))
                  .add($("td").klass("summary0d")
                    .html("<b>" +
                      (sumA > 0 ? dom.decToStr(new Dec(sumA, 2)) : "") +
                      "</b>"))
                  .add($("td").klass("summary0c")
                    .html("<b>" +
                      (sumA < 0 ? dom.decToStr(new Dec(-sumA, 2)) : "") +
                      "</b>"))
                ).addIt(It.keys(subs).sort().map(kS => {
                  const vS = subs[kS];
                  const descS = vS[0];
                  const sumS = vS[1];
                  return $("tr")
                    .add($("td").style("width:40px"))
                    .add($("td").style("width:40px"))
                    .add($("td").style("width:40px"))
                    .add($("td").klass("summary0cp")
                      .add(Ui.link(ev => { control.goAcc(kS)}).klass("link")
                        .html(Dom.accFormat(kS) + ". " + descS)))
                    .add($("td").klass("summary0d")
                      .html(sumS > 0 ? dom.decToStr(new Dec(sumS, 2)) : ""))
                    .add($("td").klass("summary0c")
                      .html(sumS < 0 ? dom.decToStr(new Dec(-sumS, 2)) : ""));
                }));
              }));
            }));
          })));
    }

    const submenu = $("table").att("align", "center").add($("tr")
      .add(menuTd("0").add(Ui.link(ev => {
          control.setSummary("0" + summary.charAt(1));
        }).klass("link").html(_("Statements"))))
      .add(separator())
      .add(menuTd("1").add(Ui.link(ev => {
          control.setSummary("1" + summary.charAt(1));
        }).klass("link").html(_("P & L"))))
      .add(separator())
      .add(menuTd("2").add(Ui.link(ev => {
          control.setSummary("2" + summary.charAt(1));
        }).klass("link").html(_("Balance"))))
      .add($("td").html("||"))
      .add(menuTd("A").add(Ui.link(ev => {
          control.setSummary(summary.charAt(0) + "A");
        }).klass("link").html(_("Summary"))))
      .add(separator())
      .add(menuTd("B").add(Ui.link(ev => {
          control.setSummary(summary.charAt(0) + "B");
        }).klass("link").html(_("Accounts"))))
      .add(separator())
      .add(menuTd("C").add(Ui.link(ev => {
          control.setSummary(summary.charAt(0) + "C");
        }).klass("link").html(_("Subaccounts"))))
    );

    control.dom().show("summaries", $("div").style("text-align:center")
      .add($("h2").html(_("Summaries")))
      .add(submenu)
      .add($("hr"))
      .add(dataDiv)
    );

    dataDiv.add(
        summary === "0A" ? statementsSumary()
      : summary === "0B" ? statementsAccounts()
      : summary === "0C" ? statementsSubaccounts()
      : summary === "1A" ? plSumary()
      : summary === "1B" ? plAccounts()
      : summary === "1C" ? plSubaccounts()
      : summary === "2A" ? balanceSumary()
      : summary === "2B" ? balanceAccounts()
      : summary === "2C" ? balanceSubaccounts()
      : $("p").html("Bad value of summary")
    );
  }

}
