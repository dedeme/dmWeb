// Copyright 24-Sep-2017 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

goog.provide("view_Plan");

goog.require("db_Balance");
goog.require("db_PyG");

view_Plan = class {
  /**
   * @param {!Main} control
   */
  constructor (control) {
    /** @private */
    this._control = control;
    /**
     * @private
     * @type {string}
     */
    this._keyModify = "";
  }

  /**
   * @return {void}
   */
  show () {
    const self = this;
    const control = self._control;
    const db = control.db();
    const conf = control.conf();

    let item = conf.planId();
    let lg = item.length;
    if (
      (lg === 2 && !It.from(db.subgroups()).containsf(s => s[0] === item)) ||
      (lg === 3 && !It.from(db.accounts()).containsf(a => a[0] === item))
    ) {
      item = "";
      lg = 0;
    }
    let group = "";
    let subgroup = "";
    let account = "";

    let title = _("Groups");
    let data = Db.groups();
    if (lg > 0) {
      group = item.charAt(0);
      title = _("Subgroups");
      data = It.from(db.subgroups()).filter(s => s[0].charAt(0) === item).to();
    }
    if (lg > 1) {
      subgroup = item.charAt(1);
      title = _("Accounts");
      data = It.from(db.accounts()).filter(a =>
        a[0].substring(0, 2) === item
      ).to();
    }
    if (lg > 2) {
      account = item.charAt(2);
      title = _("Subaccounts");
      data = It.from(db.subaccounts()).filter(s =>
        s[0].substring(0, 3) === item
      ).to();
    }
    data.sort((e1, e2) => e1[0] > e2[0] ? 1 : -1);

    const isAccount = account === "" && subgroup !== "";
    const isSubaccount = account !== "";
    const isEditable = lg !== 0 && conf.isLastYear();
    let keyModify = this._keyModify;

    const enterName = Ui.field("newBt");

    const groupField = $("input").att("type", "text").klass("frame")
      .style("width:45px;color:#000000;text-align:center;")
      .disabled(true);

    /** @type {function(string):string} */
    const formatAcc = acc => {
      const l = acc.length;
      return l == 5 ? acc.substring(3) : acc.substring(l - 1);
    }

    const mkEmptyTd = () => {
      return $("td").style("width:0px;");
    }

    const mkLeftHelp = () => {
      function mkBalance() {
        return It.from(db_Balance.groups()).map(gr => {
          const gkey = gr[0];
          const gname = gr[1];
          return $("li")
            .html("<a href='#' onclick='return false;'>" + gname + "</a>")
            .add($("ul").att("id", "hlist")
              .style("list-style:none;padding-left:10px;")
              .addIt(It.from(db_Balance.entries()).filter(e =>
                  db_Balance.groupId(e[0]) === gkey
                ).map(e => {
                  const ekey = e[0];
                  const ename = e[1];
                  const ename2 = Dom.textAdjust(ename, 340);
                  return $("li")
                    .add(Ui.link(ev => { groupField.value("B" + ekey); })
                      .klass("link").att("title", "B" + ekey).html(ename2));
                }))
            );
        });
      }

      const mkPyg = () => {
        return It.from(db_PyG.groups()).map(gr => {
          const gkey = gr[0];
          const gname = gr[1];
          return $("li")
            .html("<a href='#' onclick='return false;'>" + gname + "</a>")
            .add($("ul").att("id", "hlist")
              .style("list-style:none;padding-left:10px;")
              .addIt(It.from(db_PyG.entries()).filter(e =>
                  db_PyG.groupId(e[0]) === gkey
                ).map(e => {
                  const ekey = e[0];
                  const ename = e[1];
                  const ename2 = Dom.textAdjust(ename, 337);
                  return $("li")
                    .add(Ui.link(ev => { groupField.value("P" + ekey); })
                      .klass("link").att("title", "P" + ekey).html(ename2));
                }))
            );
        });
      }

      return  $("td").klass("frame").style("width:350px;vertical-align:top;")
        .add($("p").html("<b>Balance</b>"))
        .add($("ul").style("list-style:none;padding-left:0px;")
          .addIt(mkBalance()))
        .add($("p").html("<b>PyG</b>"))
        .add($("ul").style("list-style:none;padding-left:0px;")
          .addIt(mkPyg()))
      ;
    }

    const mkSubmenu = () => {
      const entry = (id, target) =>
        Ui.link(ev => { control.planGo(target); })
          .klass("link").html(id);
      const separator = () => $("span").html("|");
      const blank = () => $("span").html(" ");

      var r = $("p").add(separator())
        .add(entry(" * ", ""));
      if (group != "") {
        r.add(separator()).add(entry(" " + group + " ", group));
      }
      if (subgroup != "") {
        r.add(separator()).add(entry(" " + subgroup + " ", group + subgroup));
      }
      if (account != "") {
        r.add(separator()).add(
          entry(" " + account + " ", group + subgroup + account));
      }

      return r.add(separator());
    }

    const createdKeys = It.from(data).map(e =>
      isSubaccount
      ? e[0].substring(e[0].length - 2)
      : e[0].substring(e[0].length - 1)
    ).to();
    const left = isAccount ? mkLeftHelp() : mkEmptyTd();

    let cols = 2;
    if (isEditable) cols += 2;
    if (isAccount) ++cols;

    const selectKey = Ui.select("enterKey",
      (isSubaccount
        ? It.range(26).map(n => {
            const r = "0" + n;
            return r.substring(r.length - 2);
          })
        : It.range(10).map(n => "" + n)
      ).filter(n =>
        (
          !It.from(createdKeys).contains(n) &&
          keyModify !== "57" &&
          keyModify !== "572" &&
          keyModify !== "57201"
        ) ||
        (keyModify !== "" && formatAcc(keyModify) === n)
      ).map(n =>
        (keyModify !== "" && formatAcc(keyModify) === n) ? "+" + n : n
      ).to()).on("change", ev => { enterName.e().focus(); });

    let tds = [];
    let rows = [];

    // Edition row
    if (isEditable) {
      tds = [];
      tds.push($("td").att("colspan", 2).add($("button")
        .att("style",
          "border:1px solid #a0a9ae;padding:0px;width:32px;")
        .att("id", "newBt")
        .add(Ui.img("enter")
          .att("style", "padding:0px;margin:0px;vertical-align:-20%"))
        .on("click", ev => {
            const key = conf.planId() + selectKey.value();
            let name = enterName.value();
            const group = groupField.value();
            if (name == "") {
              alert(_("Description is missing"));
              return;
            }
            if (group == "" && isAccount) {
              alert(_("Group is missing"));
              return;
            }
            name = name.length > 35
              ? name.substring(0, 32) + "..."
              : name;
            if (keyModify !== "") {
              const panns = db.planAnnotations(keyModify);
              if (
                panns != 0 &&
                !confirm(_args(_("change of count %0"), panns))
              ) {
                this._keyModify = "";
                self.show();
                return;
              }
              control.planMod(keyModify, key, name, group);
            } else {
              control.planAdd(key, name, group);
            }
          })));
      tds.push($("td").add(selectKey));
      tds.push($("td").add(enterName));
      if (isAccount) {
        tds.push($("td").add(groupField));
      }
      rows.push($("tr").addIt(It.from(tds)));
      rows.push($("tr")
        .add($("td").att("colspan", cols)
          .add($("hr").att("height", "1px"))));
    }

    // Title row
    tds = [];
    if (isEditable) {
      const td = $("td").att("colspan", 2);
      if (keyModify !== "") {
        td.add(Ui.link(ev => {
          this._keyModify = "";
          self.show();
        }).add(Ui.img("cancel")))
      }
      tds.push(td);
    }
    tds.push($("td").html(_("Nº")));
    tds.push($("td").style("text-align:left;").html(_("Description")));
    if (isAccount) {
      tds.push($("td").html(_("Group")));
    }
    rows.push($("tr").addIt(It.from(tds)));
    rows.push($("tr")
      .add($("td").att("colspan", cols)
        .add($("hr").att("height", "1px"))));

    // Data row
    It.from(data).each(r => {
      const mkLink = text =>
        Ui.link(ev => { control.planGo(r[0]); })
          .klass("link").html(text);

      tds = [];
      if (isEditable) {
        tds.push($("td")
          .add(Ui.link(ev => {
            enterName.value(r[1]);
            if (isAccount) {
              groupField.value(r[2]);
            }
            this._keyModify = r[0];
            self.show();
          }).add(Ui.img("edit"))));
        tds.push($("td")
          .add(Ui.link(ev => {
            if (r[0] === "57" || r[0] === "572" || r[0] === "57200") {
              alert(_args(_("Group/account %0 can not be deleted"), r[0]));
              return;
            }
            if (!confirm(
              _args(_("Delete '%0'?"), r[0] + " - " + r[1])
            )) {
              return;
            }
            const panns = db.planAnnotations(r[0]);
            if (panns > 0) {
              alert(_args(
                _("%0 can not be deleted because %1"), r[0], panns));
              return
            }
            control.planDel(r[0]);
          }).add(Ui.img("delete"))));
      }
      tds.push($("td").style("text-align:right;")
        .html(formatAcc(r[0])));
      if (isSubaccount) {
        tds.push($("td").style("text-align:left;").html(r[1]));
      } else {
        tds.push($("td").style("text-align:left;").add(mkLink(r[1])));
      }
      if (keyModify !== "" && r[0] === keyModify) {
        enterName.value(r[1]);
      }
      if (isAccount) {
        const isPyG = r[2].charAt(0) === "P";
        const idDescription = isPyG
          ? It.from(db_PyG.entries()).findFirst(
            e => e[0] === r[2].substring(1))
          : It.from(db_Balance.entries()).findFirst(
            e => e[0] === r[2].substring(1))

        const id = idDescription[0];
        const description = idDescription[1];
        const group = isPyG
          ? It.from(db_PyG.groups()).find(e => e[0] === db_PyG.groupId(id))[1]
          : It.from(db_Balance.groups()).find(e =>
              e[0] === db_Balance.groupId(id)
            )[1];

        tds.push($("td").style("text-align:center;")
          .add(Ui.link(ev => {
              alert(group + "\n" + description);
            }).att("title", description)
            .text(r[2])));

        if (keyModify !== "" && r[0] === keyModify) {
          groupField.value(r[2]);
        }
      }
      rows.push($("tr").addIt(It.from(tds)));
    });

    const right = $("td").style("text-align:center;vertical-align:top;")
      .add($("h2").html(title))
      .add(mkSubmenu())
      .add($("table").att("align", "center").klass("frame")
        .addIt(It.from(rows)));

    const table = $("table").klass("main").add($("tr")
      .add(left)
      .add(right));
    control.dom().show("plan", table);
    enterName.e().focus();
  }
}


