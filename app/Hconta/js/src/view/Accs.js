// Copyright 23-Oct-2017 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

goog.provide("view_Accs");

goog.require("db_Dentry");

{
  const helpWidth = 250;

view_Accs = class {
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
    const account = conf.accsConf().id();

    let ix = 1;
    let sum = 0;
    const accsData = It.from(db.diary()).map(e => {
        const ammD = It.from(e.debits())
            .filter(tp => tp.e1().startsWith(account))
            .reduce(0, (s, tp) => s += tp.e2().value());
        const ammH = It.from(e.credits())
            .filter(tp => tp.e1().startsWith(account))
            .reduce(0, (s, tp) => s += tp.e2().value());
        sum += ammD - ammH;
        return [
          ix++,
          e,
          new Dec(ammD, 2),
          new Dec(ammH, 2),
          new Dec(sum, 2)
        ]
      }).to();

    const planDiv = $("div");
    const listDiv = $("div").style("width:100%");
    let ixTmp = conf.accsConf().ix();
    let accsIx = ixTmp === 0 || ixTmp > db.diary().length
      ? db.diary().length
      : conf.accsConf().ix();
    let stepList = conf.accsConf().listLen();

    // Control ---------------------------------------------

    // Left menu -----------------------

    /**
     * @param {string} acc
     * @return {void}
     */
    function changeTo (acc) {
      control.setAccsId(acc);
    }

    // Center menu -----------------------

    /**
     * @return {number} next annotation index of cash or -1
     */
    function nextAnn (accsIx) {
      const lg = db.diary().length;
      ++accsIx;
      while (
        accsIx < lg &&
        !accsData[accsIx - 1][1].containsAccountOrGroup(account)
      ) {
        ++accsIx;
      }
      return accsIx > lg ? -1 : accsIx;
    }

    /**
     * @return {number} previous annotation index of cash or -1
     */
    function previousAnn (accsIx) {
      --accsIx;
      while (
        accsIx > 0 &&
        !accsData[accsIx - 1][1].containsAccountOrGroup(account)
      ) {
        --accsIx;
      }
      return accsIx <= 0 ? -1 : accsIx;
    }

    /** @return {void} */
    function upClick () {
      const nextIx = nextAnn(accsIx);
      if (nextIx !== -1) {
        accsIx = nextIx;
        control.setAccsIx(accsIx, () => {
          listDiv.removeAll().add(list());
        })
      }
    }

    /** @return {void} */
    function downClick () {
      const previousIx = previousAnn(accsIx);
      if (previousIx !== -1) {
        accsIx = previousIx;
        control.setAccsIx(accsIx, () => {
          listDiv.removeAll().add(list());
        })
      }
    }

    /** @return {void} */
    function dupClick () {
      It.range(stepList).each(i => {
        const nextIx = nextAnn(accsIx);
        if (nextIx !== -1) {
          accsIx = nextIx;
        }
      });
      control.setAccsIx(accsIx, () => {
          listDiv.removeAll().add(list());
      });
    }

    /** @return {void} */
    function ddownClick () {
      It.range(stepList).each(i => {
        const previousIx = previousAnn(accsIx);
        if (previousIx !== -1) {
          accsIx = previousIx;
        }
      });
      control.setAccsIx(accsIx, () => {
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
      ix = previousAnn(ix);

      accsIx = ix === -1 ? 0 : ix;
      control.setAccsIx(accsIx, () => {
        listDiv.removeAll().add(list());
      })
    }

    // View ------------------------------------------------

    const planHelpf = () => {
      const upAcc = account.length === 5
        ? account.substring(0, 3)
        : account.substring(0, account.length - 1);
      return $("ul").style("list-style:none;padding-left:0px;")
        .addIt(It.range(1, account.length + 1).map(lg =>
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
        .addIt(account.length === 5
          ? It.empty()
          : db.sub(account)
            .sortf((e1, e2) => e1[0] > e2[0] ? 1 : -1)
            .map(e =>
              $("li").add(Ui.link(ev => { changeTo(e[0]); })
                .klass("link").att("title", Dom.accFormat(e[0]))
                .html(Dom.textAdjust(e[1], helpWidth - 16)))));
    }

    const mkSubmenu = () => {
      const entry = (id, target) =>
        Ui.link(ev => { changeTo(target); })
          .klass("link").html(id);
      const separator = () => $("span").html("|");
      const blank = () => $("span").html(" ");
      const lg = account.length;

      const r = $("p").add(separator())
        .add(entry(" * ", ""));

      let group = "";
      if (lg > 0) {
        group = account.substring(0, 1);
        r.add(separator()).add(entry(" " + group + " ", group));
      }
      let subgroup = "";
      if (lg > 1) {
        subgroup = account.substring(1, 2);
        r.add(separator()).add(entry(" " + subgroup + " ", group + subgroup));
      }
      let acc = "";
      if (lg > 2) {
        acc = account.substring(2, 3);
        r.add(separator()).add(
          entry(" " + acc + " ", group + subgroup + acc));
      }
      if (lg > 3) {
        const subacc = account.substring(3, 5);
        r.add(separator()).add(
          entry(" " + subacc + " ", group + subgroup + acc + subacc));
      }

      r.add(separator());
      return $("div").add(r)
        .add($("p")
          .add($("span").klass("frame").html(db.description(account))));
    }

    const list = () => {
      const td = () => $("td").klass("frame").style("vertical-align:top;");
      const tdr = () => td().addStyle("text-align:right");
      const tdl = () => td().addStyle("text-align:left");

      const data = It.from(accsData)
        .take(accsIx)
        .filter(arr => arr[2].value() + arr[3].value() !== 0)
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
              .add(tdr().addStyle("background-color:#f0f0ff;")
                .html(arr[2].value() === 0 ? "" : dom.decToStr(arr[2])))
              .add(tdr().addStyle("background-color:#fff0f0;")
                .html(arr[3].value() === 0 ? "" : dom.decToStr(arr[3])))
              .add(tdr().html(dom.decToStr(arr[4])))
          ));
    }

    const left = $("td").klass("frame")
      .style("width:" + helpWidth + "px;vertical-align:top;white-space:nowrap")
      .add(planDiv)
    ;

    const right =  $("td").style("text-align:center;vertical-align:top;")
      .add($("h2").html(_("Accs")))
      .add(mkSubmenu())
      .add($("hr"))
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
    control.dom().show("accs", table);

    planDiv.add(planHelpf());
    listDiv.add(list());
  }
}}
