// Copyright 13-Oct-2017 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

goog.provide("view_Cash");

goog.require("github_dedeme.DatePicker");
goog.require("github_dedeme.NumberField");
goog.require("db_Dentry");

{
  const cashAcc = "57200";
  const helpWidth = 250;
  const DatePicker = github_dedeme.DatePicker/**/;
  const NumberField = github_dedeme.NumberField/**/;

view_Cash = class {
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
    const db = control.db();
    const dom = control.dom();

    const shortCuts = control.shortCuts();
    const mostUsed = db.mostUsed();
    let ix = 1;
    let sum = 0;
    const cashData = It.from(db.diary()).map(e => {
        const amm = It.from(e.debits()).filter(tp => tp.e1() === cashAcc)
            .reduce(0, (s, tp) => s += tp.e2().value()) -
          It.from(e.credits()).filter(tp => tp.e1() === cashAcc)
            .reduce(0, (s, tp) => s += tp.e2().value());
          sum += amm;
        return [
          ix++,
          e,
          new Dec(amm, 2),
          new Dec(sum, 2)
        ]
      }).to();

    const planDiv = $("div");
    const listDiv = $("div").style("width:100%");
    const dateField = $("input").att("type", "text").klass("frame")
      .style("width:80px;color:#000000;text-align:center;");
    const datePicker = new DatePicker();
    datePicker.setLang(lang);
    datePicker.setAction(d => {});
    datePicker.setDate(cashData.length > 0
      ? cashData[cashData.length - 1][1].date()
      : DateDm.now()
    );
    const acc = $("input").att("type", "text").klass("frame")
      .style("width:45px;color:#000000;text-align:center;")
      .disabled(true);
    const description = Ui.field("debit").att("id", "description")
      .style("width:270px")
      .on("focus", ev => { description.e().select(); });
    const amm = new NumberField(lang === "en", "accept");
    amm.input().att("id", "debit").style("width:65px")
      .on("focus", ev => { amm.input().e().select(); });
    const accept = $("button").html(_("Accept")).att("id", "accept")
      .on("click", acceptEntry);
    if (!conf.isLastYear()) {
      accept.disabled(true);
    }

    let ixTmp = conf.diaryConf().ix();
    let cashIx = ixTmp === 0 || ixTmp > db.diary().length
      ? db.diary().length
      : conf.cashConf().ix();
    let stepList = conf.cashConf().listLen();

    // Control ---------------------------------------------

    // Left menu -----------------------

    /**
     * @param {string} acc
     * @return {void}
     */
    function changeTo (acc) {
      const lg = acc.length
      if (acc.length < 3) {
        changeTo(db.subOf(acc).next()[0]);
      } else {
        control.setCashId(
          acc,
          () => { planDiv.removeAll().add(planHelpf()); }
        );
      }
    }

    /**
     * @param {string} id
     * @param {string} descr
     * @return {void}
     */
    function helpAccountClick (id, descr) {
      acc.value(Dom.accFormat(id)).att("title", descr);
      const descVal = shortCuts[id];
      description.value(descVal[0]);
      amm.input().value(descVal[1]);
      description.e().focus();
    }

    // Entry ---------------------------

    function acceptEntry () {
      const date = datePicker.date();
      if (date === null) {
        alert(_("Date is missing"));
        return;
      }
      let a = acc.value();
      if (a === "") {
        alert(_("Account is missing"));
        return;
      }
      a = a.replace(".", "");
      const d = description.value().trim();
      if (d === "") {
        alert(_("Description is missing"));
        return;
      }
      const v = amm.value(2);
      if (v === null) {
        alert(_("Ammount is missing"));
        return;
      }
      if (v.eqValue(new Dec())) {
        alert(_("Ammount is 0"));
        return;
      }
      /** @const {!Array<!Tp<string,!Dec>>} */
      const debits = [];
      /** @const {!Array<!Tp<string,!Dec>>} */
      const credits = [];
      if (v.value() < 0) {
        const v2 = new Dec(-v.value(), 2);
        debits.push(new Tp(a, v2));
        credits.push(new Tp(cashAcc, v2));
      } else {
        debits.push(new Tp(cashAcc, v));
        credits.push(new Tp(a, v));
      }

      control.addDentry2(new db_Dentry(
        date,
        d,
        debits,
        credits
      ));
    }

    // Center menu -----------------------

    /**
     * @return {number} next annotation index of cash or -1
     */
    function nextCash (cashIx) {
      const lg = db.diary().length;
      ++cashIx;
      while (
        cashIx < lg &&
        !cashData[cashIx - 1][1].containsAccount(cashAcc)
      ) {
        ++cashIx;
      }
      return cashIx > lg ? -1 : cashIx;
    }

    /**
     * @return {number} previous annotation index of cash or -1
     */
    function previousCash (cashIx) {
      --cashIx;
      while (cashIx > 0 && !cashData[cashIx - 1][1].containsAccount(cashAcc)) {
        --cashIx;
      }
      return cashIx <= 0 ? -1 : cashIx;
    }

    /** @return {void} */
    function upClick () {
      const nextIx = nextCash(cashIx);
      if (nextIx !== -1) {
        cashIx = nextIx;
        control.setCashIx(cashIx, () => {
          listDiv.removeAll().add(list());
        })
      }
    }

    /** @return {void} */
    function downClick () {
      const previousIx = previousCash(cashIx);
      if (previousIx !== -1) {
        cashIx = previousIx;
        control.setCashIx(cashIx, () => {
          listDiv.removeAll().add(list());
        })
      }
    }

    /** @return {void} */
    function dupClick () {
      It.range(stepList).each(i => {
        const nextIx = nextCash(cashIx);
        if (nextIx !== -1) {
          cashIx = nextIx;
        }
      });
      control.setCashIx(cashIx, () => {
          listDiv.removeAll().add(list());
      });
    }

    /** @return {void} */
    function ddownClick () {
      It.range(stepList).each(i => {
        const previousIx = previousCash(cashIx);
        if (previousIx !== -1) {
          cashIx = previousIx;
        }
      });
      control.setCashIx(cashIx, () => {
          listDiv.removeAll().add(list());
      });
    }

    /** @return {void} */
    function plusClick () {
      stepList++;
      control.setCashListLen(
        stepList,
        () => { listDiv.removeAll().add(list()); }
      );
    }

    /** @return {void} */
    function minusClick () {
      if (stepList > 3) {
        stepList--;
        control.setCashListLen(
          stepList,
          () => { listDiv.removeAll().add(list()); }
        );
      }
    }

    /** @return {void} */
    function monthClick (i) {
      let ix =
        It.from(db.diary()).takeUntil(e => e.date().month() > i).size() + 1;
      ix = previousCash(ix);

      cashIx = ix === -1 ? 0 : ix;
      control.setCashIx(cashIx, () => {
        listDiv.removeAll().add(list());
      })
    }

    // View ------------------------------------------------

    const planHelpf = () => {
      const account = conf.cashConf().id();
      return $("ul").style("list-style:none;padding-left:0px;")
        .addIt(It.range(1, 4).map(lg =>
          $("li")
            .html("<a href='#' onclick='return false;'>" +
              Dom.textAdjust(
                db.description(account.substring(0, lg)), helpWidth - 4
              ) + "</a>")
            .add($("ul").att("id", "hlist")
              .style("list-style:none;padding-left:10px;")
              .addIt(db.subOf(account.substring(0, lg - 1))
                .sortf((e1, e2) => e1[0] > e2[0] ? 1 : -1)
                .map(e =>
                  $("li").add(Ui.link(ev => {
                      changeTo(e[0]);
                    }).klass("link").att("title", e[0])
                    .html(Dom.textAdjust(e[1], helpWidth - 16))))))))
        .add($("li").add($("hr")))
        .addIt(db.sub(account)
          .sortf((e1, e2) => e1[0] > e2[0] ? 1 : -1)
          .map(e =>
            $("li").add(Ui.link(ev => { helpAccountClick(e[0], e[1]); })
              .klass("link").att("title", Dom.accFormat(e[0]))
              .html(Dom.textAdjust(e[1], helpWidth - 16)))));
    }

    const list = () => {
      const td = () => $("td").klass("frame").style("vertical-align:top;");
      const tdr = () => td().addStyle("text-align:right");
      const tdl = () => td().addStyle("text-align:left");

      const data = It.from(cashData)
        .take(cashIx)
        .filter(arr => arr[2].value() !== 0)
        .to();
      return $("table").att("align", "center")
        .addIt(It.from(data)
          .drop(data.length - stepList)
          .reverse()
          .map(arr =>
            $("tr")
              .add(tdr().html("" + arr[0]))
              .add(td().html(arr[1].date().format("%D/%M")))
              .add(tdl().add(Ui.link(ev => {
                  control.goDiary(arr[0]);
                }).klass("link").html(arr[1].description())))
              .add(tdr().html(dom.decToStr(arr[2])))
              .add(tdr().html(dom.decToStr(arr[3])))
          ));
    }

    const left = $("td").klass("frame")
      .style("width:" + helpWidth + "px;vertical-align:top;white-space:nowrap")
      .add($("p").html("<b>" + _("Most used accounts") + "</b>"))
      .add($("ul").style("list-style:none;padding-left:0px;")
        .addIt(
            It.from(mostUsed).drop(1).map(acc =>
              $("li").add(Ui.link(ev => {
                  helpAccountClick(
                    acc,
                    It.from(db.subaccounts()).findFirst(s => s[0] === acc)[1]
                  )
                }).add($("span").klass("link")
                  .att("title", Dom.accFormat(acc)).html(
                    Dom.textAdjust(
                      It.from(db.subaccounts()).findFirst(s => s[0] === acc)[1],
                      helpWidth - 4
                    )
                ))))
          ))
      .add($("p").html("<b>" + _("Plan") + "</b>"))
      .add(planDiv)
    ;

    const right =  $("td").style("text-align:center;vertical-align:top;")
      .add($("h2").html(_("Cash")))
      .add($("table").att("align", "center").add($("tr")
        .add($("td").add(datePicker.makeText(dateField)))
        .add($("td").add(acc))
        .add($("td").add(description))
        .add($("td").add(amm.input()))
        .add($("td").add(accept))
      )).add($("hr"))
      .add($("table").att("align", "center")
        .add($("tr")
          .add($("td").att("colspan", 3))
          .add($("td").klass("diary").add(Ui.link(ev => {
              upClick();
            }).addStyle("font-family:monospace").html("&nbsp;\u2191&nbsp;")))
          .add($("td").klass("diary").add(Ui.link(ev => {
              downClick();
            }).addStyle("font-family:monospace").html("&nbsp;\u2193&nbsp;")))
          .add($("td").klass("diary").add(Ui.link(ev => {
              dupClick();
            }).addStyle("font-family:monospace").html("\u2191\u2191")))
          .add($("td").klass("diary").add(Ui.link(ev => {
              ddownClick();
            }).addStyle("font-family:monospace").html("\u2193\u2193")))
          .add($("td").klass("diary").add(Ui.link(ev => {
              plusClick();
            }).html("&nbsp;+&nbsp;")))
          .add($("td").klass("diary").add(Ui.link(ev => {
              minusClick();
            }).html("&nbsp;-&nbsp;")))
          .add($("td").att("colspan", 3)))
        .add($("tr")
          .add($("td").klass("diary").add(Ui.link(ev => {
              monthClick(1);
            }).html("&nbsp;1&nbsp;")))
          .add($("td").klass("diary").add(Ui.link(ev => {
              monthClick(2);
            }).html("&nbsp;2&nbsp;")))
          .add($("td").klass("diary").add(Ui.link(ev => {
              monthClick(3);
            }).html("&nbsp;3&nbsp;")))
          .add($("td").klass("diary").add(Ui.link(ev => {
              monthClick(4);
            }).html("&nbsp;4&nbsp;")))
          .add($("td").klass("diary").add(Ui.link(ev => {
              monthClick(5);
            }).html("&nbsp;5&nbsp;")))
          .add($("td").klass("diary").add(Ui.link(ev => {
              monthClick(6);
            }).html("&nbsp;6&nbsp;")))
          .add($("td").klass("diary").add(Ui.link(ev => {
              monthClick(7);
            }).html("&nbsp;7&nbsp;")))
          .add($("td").klass("diary").add(Ui.link(ev => {
              monthClick(8);
            }).html("&nbsp;8&nbsp;")))
          .add($("td").klass("diary").add(Ui.link(ev => {
              monthClick(9);
            }).html("&nbsp;9&nbsp;")))
          .add($("td").klass("diary").add(Ui.link(ev => {
              monthClick(10);
            }).html("10")))
          .add($("td").klass("diary").add(Ui.link(ev => {
              monthClick(11);
            }).html("11")))
          .add($("td").klass("diary").add(Ui.link(ev => {
              monthClick(12);
            }).html("12")))
        ))
      .add($("hr"))
      .add(listDiv)
    ;

    const table = $("table").klass("main").add($("tr")
      .add(left)
      .add(right));
    control.dom().show("cash", table);

    planDiv.add(planHelpf());
    listDiv.add(list());
  }
}}
