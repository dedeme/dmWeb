// Copyright 24-Sep-2017 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

goog.provide("view_Diary");

goog.require("github_dedeme.DatePicker");
goog.require("github_dedeme.NumberField");
goog.require("db_Dentry");

{
  const helpWidth = 250;
  const DatePicker = github_dedeme.DatePicker/**/;
  const NumberField = github_dedeme.NumberField/**/;

view_Diary = class {
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

    const mostUsed = db.mostUsed();

    const planDiv = $("div");
    const editDiv = $("div");
    const listDiv = $("div").style("width:100%");
    const dateField = $("input").att("type", "text").klass("frame")
      .style("width:80px;color:#000000;text-align:center;");
    const datePicker = new DatePicker();
    datePicker.setLang(lang);
    datePicker.setAction(d => {});
    const number =  $("input").att("type", "text").klass("frame")
      .style("width:50px;background-color:#f0f0ff;color:#000000;" +
        "text-align:center;")
      .disabled(true);
    const description = Ui.field("debit").att("id", "description")
      .style("width:270px");
    /** @const {!Array<!Array<!Domo>>} */
    const entryRows = [];

    let ixTmp = conf.diaryConf().ix();
    let diaryIx = ixTmp === 0 || ixTmp > db.diary().length
      ? db.diary().length
      : conf.diaryConf().ix();
    let stepList = conf.diaryConf().listLen();
    let fieldFocussed = 0;

    function rowCol (fFocussed) {
      return [Math.floor(fFocussed / 2), 3 * (fFocussed % 2)];
    }

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
        control.setDiaryId(
          acc,
          () => { planDiv.removeAll().add(planHelpf()); }
        );
      }
    }

    /**
     * @param {string} id
     * @param {string} description
     * @return {void}
     */
    function helpAccountClick (id, description) {
      const [row, col] = rowCol(fieldFocussed);
      if (entryRows[row]) {
        entryRows[row][col].html(Dom.accFormat(id)).att("title", description);
      }
    }

    // Entry ---------------------------

    /**
     * @return {void}
     */
    function autoSum () {
      /** @const {!Array<!Tp<string,!Dec>>} */
      const debits = [];
      let sdeb = 0;
      /** @type {!Array<!Tp<string,!Dec>>} */
      const credits = [];
      let scred = 0;
      let err = "";
      It.from(entryRows).each(dm => {
        if (err === "") {
          const db = dm[1].value().trim();
          const cr = dm[2].value().trim();
          if (db !=="" && !dom.isNumber(db)) {
            err = _args(_("'%0' is not a number"), db);
          }
          if (cr !== "" && !dom.isNumber(cr)) {
            err = _args(_("'%0' is not a number"), cr);
          }
          if (err === "") {
            sdeb += dom.toDec(db).value();
            scred += dom.toDec(cr).value();
          }
        }
      });
      if (err !== "") {
        alert(err);
        return;
      }
      if (new Dec(sdeb, 2).eqValue(new Dec(scred, 2))) {
        alert(_("Sums of Debits and Credits are equals"));
        return;
      }
      const len1 = entryRows.length - 1;
      if (sdeb > scred) {
        const diff = new Dec(sdeb - scred, 2);
        const diffs = lang === "es" ? diff.toEu() : diff.toEn();
        if (entryRows[len1][2].value().trim() !== "") {
          alert(_("Result is a credit,\nbut last credit field is not empty"));
          return;
        }
        entryRows[len1][2].value(diffs);
        return;
      }
      const diff = new Dec(scred - sdeb, 2);
      const diffs = lang === "es" ? diff.toEu() : diff.toEn();
      if (entryRows[len1][1].value().trim() !== "") {
        alert(_("Result is a debit,\nbut last debit field is not empty"));
        return;
      }
      entryRows[len1][1].value(diffs);
      return;
    }

    /** @return {void} */
    function addEntryRow () {
      const ix = entryRows.length * 2;
      entryRows.push([
        mkAccEntry(),
        mkAmmountEntry(ix),
        mkAmmountEntry(ix + 1),
        mkAccEntry()
      ]);
      editDiv.removeAll().add(entry());
      description.e().focus();
    }

    /** @return {void} */
    function removeEntryRow () {
      const len1 = entryRows.length - 1;
      if (entryRows.length === 1) {
        alert (_("Single row can not be removed"));
        return;
      }
      if (
        entryRows[len1][1].value().trim() !== "" ||
        entryRows[len1][2].value().trim() !== ""
      ) {
        alert(_("Last row has data"));
        return;
      }
      const [row, col] = rowCol(fieldFocussed);
      if (row === len1) {
        fieldFocussed = 0;
        fieldFocus(0);
      }
      entryRows.splice(len1, 1);
      editDiv.removeAll().add(entry());
      description.e().focus();
    }

    /**
     * @param {number} ix
     * @return {void}
     */
    function ammountFocus (ix) {
      fieldUnfocus(fieldFocussed);
      fieldFocussed = ix;
      fieldFocus(fieldFocussed);
    }

    /**
     * @param {number} ix
     * @return {void}
     */
    function ammountDblclick (ix) {
      const [row, col] = rowCol(ix);
      entryRows[row][col].value("");
    }

    /**
     * @return {void}
     */
    function cancelEntry () {
      entryRows.splice(0, entryRows.length);
      editDiv.removeAll();
    }

    /**
     * @param {number=} lix Index of annotations if it is modification
     * @return {void}
     */
    function acceptEntry (lix) {
      function ctrl(acc, amm) {
        if (acc === "" && amm === "") {
          return "";
        }
        if (acc === "") {
          return (_args(_("Account for ammount %0 is missing"), amm));
        }
        if (!dom.isNumber(amm)) {
          return (_args(_("'%0' is not a number"), amm));
        }
        return "";
      }
      const date = datePicker.date();
      if (date === null) {
        alert(_("Date is missing"));
        return;
      }
      const desc = description.value().trim();
      if (desc === "") {
        alert(_("Description is missing"));
        return;
      }
      /** @const {!Array<!Tp<string,!Dec>>} */
      const debits = [];
      let sdeb = 0;
      /** @type {!Array<!Tp<string,!Dec>>} */
      const credits = [];
      let scred = 0;
      let err = "";
      It.from(entryRows).each(dm => {
        /** @const !Array<string>} */
        const e = [
          dm[0].html().trim(),
          dm[1].value().trim(),
          dm[2].value().trim(),
          dm[3].html().trim()
        ]
        if (err === "") {
          err = ctrl(e[0], e[1]);
          if (err === "") {
            err = ctrl(e[3], e[2]);
            if (err === "") {
              if (e[1] !== "") {
                const dec = dom.toDec(e[1]);
                sdeb += dec.value();
                debits.push(new Tp(e[0].replace(".", ""), dec));
              }
              if (e[2] !== "") {
                const dec = dom.toDec(e[2]);
                scred += dec.value();
                credits.push(new Tp(e[3].replace(".", ""), dec));
              }
            }
          }
        }
      });
      if (err === "") {
        const accs = It.from(debits).addIt(It.from(credits))
          .map(tp => tp.e1()).to();

        err = It.range(accs.length - 1).reduce("", (s, i) =>
            s !== "" ? s
            : It.range(i + 1, accs.length).reduce("", (ss, j) =>
                ss !== "" ? ss
                : (accs[i] !== accs[j]) ? ""
                : _args(_("Account %0 is repeated"), Dom.accFormat(accs[i]))
              )
          );
      }
      if (err !== "") {
        alert(err);
        return;
      }
      if (sdeb !== scred) {
        alert(_("Debits sum is different from Credits sum"));
        return;
      }
      if (sdeb === 0) {
        alert(_("Sums of Debits and Credits are zero"));
        return;
      }

      if (lix) {
        control.modifyDentry(
          lix,
          new db_Dentry(
            date,
            desc,
            debits,
            credits
          )
        );
      } else {
        control.addDentry(new db_Dentry(
          date,
          desc,
          debits,
          credits
        ));
      }
    }

    // Center menu -----------------------

    /** return {void} */
    function newClick () {
      datePicker.setDate(
        db.diary().length > 1
          ? db.diary()[db.diary().length - 1].date()
          : DateDm.now()
      );
      number.value(db.diary().length + 1);
      description.value("");
      entryRows.splice(0, entryRows.length);
      entryRows.push([
        mkAccEntry(),
        mkAmmountEntry(0),
        mkAmmountEntry(1),
        mkAccEntry()
      ]);
      fieldFocussed = 0;
      fieldFocus(0);
      editDiv.removeAll().add(entry());
      description.e().focus();
    }

    /** @return {void} */
    function upClick () {
      if (diaryIx < db.diary().length) {
        diaryIx++;
        control.setDiaryIx(diaryIx, () => {
          listDiv.removeAll().add(list());
        })
      }
    }

    /** @return {void} */
    function downClick () {
      if (diaryIx > 1) {
        diaryIx--;
        control.setDiaryIx(diaryIx, () => {
          listDiv.removeAll().add(list());
        })
      }
    }

    /** @return {void} */
    function dupClick () {
      if (diaryIx < db.diary().length) {
        const ix = diaryIx + stepList
        diaryIx =  ix > db.diary().length ? db.diary().length : ix;
        control.setDiaryIx(diaryIx, () => {
          listDiv.removeAll().add(list());
        })
      }
    }

    /** @return {void} */
    function ddownClick () {
      if (diaryIx > 1) {
        const ix = diaryIx - stepList;
        diaryIx = ix <  1 ? 1 : ix;
        control.setDiaryIx(diaryIx, () => {
          listDiv.removeAll().add(list());
        })
      }
    }

    /** @return {void} */
    function plusClick () {
      stepList++;
      control.setDiaryListLen(
        stepList,
        () => { listDiv.removeAll().add(list()); }
      );
    }

    /** @return {void} */
    function minusClick () {
      if (stepList > 3) {
        stepList--;
        control.setDiaryListLen(
          stepList,
          () => { listDiv.removeAll().add(list()); }
        );
      }
    }

    /** @return {void} */
    function monthClick (i) {
      diaryIx = It.from(db.diary()).takeUntil(e => e.date().month() > i).size();
      control.setDiaryIx(diaryIx, () => {
        listDiv.removeAll().add(list());
      })
    }

    // List --------------------

    function deleteClick (i) {
      if (confirm(_args(
        _("Delete annotation %0:\n%1?"),
        i,
        db.diary()[i - 1].description()
      ))) {
        const len = db.diary().length;
        control.delDentry(
          i,
          () => {
              if (diaryIx === len) {
                downClick();
              } else {
                listDiv.removeAll().add(list());
              }
            }
        );
      }
    }

    function modifyClick (i) {
      const e = db.diary()[i - 1];
      const n = e.debits().length > e.credits().length
        ? e.debits().length
        : e.credits().length;

      datePicker.setDate(e.date());
      number.value(i);
      description.value(e.description());

      entryRows.splice(0, entryRows.length);
      It.range(n).each(i => {
        entryRows.push([
          mkAccEntry().html(
            i < e.debits().length ? Dom.accFormat(e.debits()[i].e1()) : ""
          ),
          mkAmmountEntry(i * 2).value(
            i < e.debits().length ? dom.decToStr(e.debits()[i].e2()) : ""
          ),
          mkAmmountEntry(i * 2 + 1).value(
            i < e.credits().length ? dom.decToStr(e.credits()[i].e2()) : ""
          ),
          mkAccEntry().html(
            i < e.credits().length ? Dom.accFormat(e.credits()[i].e1()) : ""
          )
        ]);
      });
      fieldFocussed = 0;
      fieldFocus(0);
      editDiv.removeAll().add(entry(i, e.date()));
      description.e().focus();
    }

    // View ------------------------------------------------

    const planHelpf = () => {
      const account = conf.diaryConf().id();
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

    const mkAccEntry = () => {
      const i = $("div");
      i.klass("frame")
      .style(
        "width:48px;color:#000000;" +
        "text-align:center;vertical-align:middle;"
      ).on("dblclick", ev => { i.html(""); });
      return i;
    }

    const mkAmmountEntry = (ix) => {
      const amm = new NumberField(lang === "en", "accept");
      return amm.input().att("type", "text")
      .style("width:65px").on("focus", ev => { ammountFocus(ix); });
    }

    const fieldFocus = (ix) => {
      const [row, col] = rowCol(ix);
      entryRows[row][col].style(
        "width:48px;color:#000000;" +
        "background-color:#fffff0;text-align:center;vertical-align:text-bottom;"
      );
    }

    const fieldUnfocus = (ix) => {
      const [row, col] = rowCol(ix);
      entryRows[row][col].style(
        "width:48px;color:#000000;" +
        "background-color:#ffffff;text-align:center;vertical-align:middle;")
    }

    /** @const {function(number=, !DateDm=):!Domo} */
    const entry = (lix, date) => {
      return $("table").att("align", "center").klass("frame")
        .add($("tr")
          .add($("td").att("colspan", 2).style("text-align:left")
            .add(datePicker.makeText(dateField)))
          .add($("td").att("colspan", 3)
            .add(Ui.link(ev => { autoSum(); }).klass("diary")
              .html("&nbsp;&nbsp;S&nbsp;&nbsp;"))
            .add($("span").html(" · "))
            .add(Ui.link(ev => { addEntryRow(); }).klass("diary").html("+"))
            .add($("span").html(" "))
            .add(Ui.link(ev => { removeEntryRow(); }).klass("diary").html("-")))
          .add($("td").att("colspan", 2).style("text-align:right;")
            .add(number)))
        .add($("tr")
          .add($("td").att("colspan", 7).add(description)))
        .addIt(It.from(entryRows).map(e =>
          $("tr")
            .add($("td").style("text-align:left;width:5px").add(e[0]))
            .add($("td").att("colspan", 2).style("text-align:left").add(e[1]))
            .add($("td").html("||"))
            .add($("td").att("colspan", 2).style("text-align:right").add(e[2]))
            .add($("td").style("text-align:right").add(e[3])))
          )
        .add($("tr").add($("td").att("colspan", 7).add($("hr"))))
        .add($("tr")
          .add($("td").att("colspan", 7).style("text-align:right")
            .add($("button").text(_("Cancel")).style("width:100px")
              .on("click", (ev) => { cancelEntry(); }))
            .add($("span").html("&nbsp;&nbsp;"))
            .add($("button").text(_("Accept")).style("width:100px")
              .att("id", "accept")
              .on("click", (ev) => {
                  const newDate = datePicker.date();
                  if (lix && newDate !== null && date) {
                    if (newDate.eq(date)) {
                      acceptEntry(lix);
                    } else {
                      control.delDentry(lix, () => { acceptEntry(); });
                    }
                  } else {
                    acceptEntry();
                  }
                }))))
    }

    const list = () => {
      const td = () => $("td").klass("frame").style("vertical-align:top;");
      const tdr = () => td().addStyle("text-align:right");
      const tdl = () => td().addStyle("text-align:left");
      let ix = diaryIx;
      return $("table").att("align", "center")
        .addIt(It.from(db.diary())
          .take(diaryIx)
          .drop(diaryIx - stepList)
          .reverse()
          .map(e => {
            const n = e.debits().length > e.credits().length
                    ? e.debits().length
                    : e.credits().length;
            const descDentry = $("table").att("align", "center")
              .addIt(It.range(n).map(i =>
                $("tr")
                  .add(td().add(i < e.debits().length
                    ? Ui.link(ev => {
                        control.goToAcc(e.debits()[i].e1(), lix)
                      }).klass("link")
                        .html(Dom.accFormat(e.debits()[i].e1()))
                    : $("span")))
                  .add(tdr().add(i < e.debits().length
                    ? $("span").html(dom.decToStr(e.debits()[i].e2()))
                    : $("span")))
                  .add($("td").html("||"))
                  .add(tdr().add(i < e.credits().length
                    ? $("span").html(dom.decToStr(e.credits()[i].e2()))
                    : $("span")))
                  .add(td().add(i < e.credits().length
                    ? Ui.link(ev => {
                        control.goToAcc(e.credits()[i].e1(), lix)
                      }).klass("link")
                        .html(Dom.accFormat(e.credits()[i].e1()))
                    : $("span")))
              ));
            descDentry.e().style/**/.display/**/ = "none";
            const lix = ix--;
            const desc = $("div")
              .add(Ui.link(ev => {
                  descDentry.e().style/**/.display/**/ =
                    descDentry.e().style/**/.display/**/ === "none"
                      ? "block" : "none";
                }).klass("link").html(e.description().toString()))
              .add(descDentry);
            return $("tr")
              .add($("td").add(
                lix === 1
                  ? $("span").add(Ui.lightImg("delete"))
                  : Ui.link(ev => {
                      deleteClick(lix);
                    }).add(Ui.img("delete"))))
              .add(tdr().html("" + lix))
              .add(td().add(
                conf.isLastYear()
                  ? Ui.link(ev => {modifyClick(lix);}).klass("link")
                    .html(e.date().format("%D/%M"))
                  : $("span").style("color:#800000;")
                    .html(e.date().format("%D/%M"))))
              .add(tdl().add(desc))
              .add(tdr().html(dom.decToStr(new Dec(
                It.from(e.debits())
                  .reduce(0, (s, tp) => s += tp.e2().value()),
                2))))
          })
        );
    }

    const left = $("td").klass("frame")
      .style("width:" + helpWidth + "px;vertical-align:top;white-space:nowrap")
      .add($("p").html("<b>" + _("Most used accounts") + "</b>"))
      .add($("ul").style("list-style:none;padding-left:0px;")
        .addIt(
            It.from(mostUsed).map(acc =>
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
      .add(planDiv);

    const right =  $("td").style("text-align:center;vertical-align:top;")
      .add($("h2").html(_("Diary")))
      .add(editDiv)
      .add($("hr"))
      .add($("table").att("align", "center")
        .add($("tr")
          .add($("td").att("colspan", 2))
          .add($("td").att("colspan", 2).klass("diary").add(
            conf.isLastYear()
              ? Ui.link(ev => {newClick();}).html(_("New"))
              : $("span").style("color: #800000;").html(_("New"))))
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
          .add($("td").att("colspan", 2)))
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
      .add(listDiv);
    ;

    const table = $("table").klass("main").add($("tr")
      .add(left)
      .add(right));
    control.dom().show("diary", table);

    planDiv.add(planHelpf());
    listDiv.add(list());
  }
}}
