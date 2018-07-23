// Copyright 11-07-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

goog.provide("Nicks");

{

Nicks = class {

  /**
   * @param {!Main} main
   * @return {!Nicks}
   */
  constructor (main) {
    /**
     * @private
     * @type {!Main}
     */
    this._main = main;
    /**
     * @private
     * @type {!Array<!Array<!Nicks_entry>>}
     */
    this._entries = [];
    /** @type {!Domo} */
    this._inputNew = Ui.field("newBt").style("width:100px");
    /** @type {!Domo} */
    this._newBt = $("button").att("id", "newBt");
  }

  /**
   * @param {function(*):void} f
   * @return {void}
   */
  idata (f) {
    const client = this._main.client();
    const data = {"page" : "nicks", "rq": "idata"};
    client.send(data, rp => f(rp));
  }

// View --------------------------------------------------------------

  /** @private */
  sep () {
    return $("span").style("padding-left:5px");
  }

  /** @private */
  head () {
    const main = this._main;
    const client = main.client();
    const input = this._inputNew;
    const bt = this._newBt.html(_("New nick")).on("click", ev => {
        const nick = input.value().trim();
        if (nick === "") {
          alert(_("Nick name is missing"));
          return;
        }
        const data = {"page" : "nicks", "rq": "new", "nick": nick};
        client.send(data, (rp) => {
          if (rp["ok"]) {
            main.run();
            return;
          }
          alert (_args(_("'%0' already exists"), nick));
          input.e().select();
          input.e().focus();
        });
      });
    return $("table").style("width:100%").add($("tr").add($("td")
      .add(input)
      .add(this.sep())
      .add(bt)
    ));
  }

  /** private */
  mkEntries (list) {
    const ls = this._entries;
    It.range(5).each(() => ls.push([]));
    let li = 0;
    let i = 0;
    while (true) {
      ls[li].push(new Nicks_entry(this._main, list[i]));
      ++li;
      ++i;
      if (li === 5) {
        li = 0;
      }
      if (i >= list.length) {
        break;
      }
    }
  }

  /** @private */
  emptyList () {
    return $("table").att("align", "center").add($("tr").add($("td")
        .add($("div").klass("frame").html(_("Without nicks")))
    ));
  }

  /** @private */
  fullList (model) {
    const ls = this._entries;
    const tb = $("table").style("width:100%").klass("frame");
    It.range(ls[0].length).each(i => {
      const tr = $("tr");
      tb.add(tr);
      It.range(5).each(j => {
        const td = $("td");
        tr.add(td);
        if (i < ls[j].length) {
          td.add(
            ls[j][i].mk()
          );
        }
      });
    });
    return tb;
  }

  /**
   * @param {*} data
   * @return {!Domo}
   */
  mk (data) {
    console.log(data);
    const model = data["model"];
    // elements are Arrays of 4 elements
    // [id:string, name:string, isIbex:bool, isSel:bool]
    const list = data["nicks"];
    // Elements are Arrats of 2 elements
    // [nickId:string, withIssue:boolean]
    const issues = data["issues"];

    if (list !== null) {
      if (issues !== null) { // Add to elements of list a 5th field
                             // [withIssue:boolean]
        It.from(list).each (n => {
          const n_is = It.from(issues).findFirst(i => i[0] === n[0]);
          if (n_is && n_is[1]) {
            n[4] = true;
          } else {
            n[4] = false;
          }
        })
      }
      list.sort((e1, e2) => e1[1].localeCompare(e2[1]));
      this.mkEntries(list);
      It.from(this._entries).each(c => It.from(c).each(e => e.setModel(model)));
    }
    return $("div")
      .add(this.head())
      .add(model !== null ? this.fullList(model) : this.emptyList())
    ;
  }

  /**
   * @return {void}
   */
  initialFocus () {
    this._inputNew.e().focus();
  }
}

// -------------------------------------------------------------------
// CLASS: Nick_entry
// -------------------------------------------------------------------

class Nicks_entry {
  constructor (main, e) {
    this._main = main;
    /** type {string} */
    this._id = e[0];
    /** type {string} */
    this._nick = e[1];
    /** type {boolean} */
    this._isIbex = e[2];
    /** type {boolean} */
    this._isSel = e[3];
    /** type {boolean} */
    this._withIssue = e[4];
    /** @type {!Domo} */
    this._modelDomo = $("div");
    /** @type {!Domo} */
    this._ibexDomo = $("div");
    /** @type {!Domo} */
    this._selDomo = $("div");
    /** @type {!Domo} */
    this._checkDomo = $("div");
    /** @type {!Domo} */
    this._issueDomo = $("div");
  }

  /** @private */
  modifyModel () {
    const client = this._main.client();
    const data = {"page" : "nicks", "rq": "setModel", "id": this._id};
    client.send(data, rp => {
      this._main.run();
    })
  }

  /** @private */
  del () {
    if (!confirm (_args(_("Delete '%0'?"), this._nick))) {
      return;
    }
    const client = this._main.client();
    const data = {"page" : "nicks", "rq": "del", "id": this._id};
    client.send(data, rp => {
      this._main.run();
    })
  }

  /** @private */
  changeIbex () {
    const client = this._main.client();
    const data = {"page" : "nicks", "rq": "changeIbex", "id": this._id};
    client.send(data, rp => {
      this._main.run();
    })
  }

  /** @private */
  changeSel () {
    const client = this._main.client();
    const data = {"page" : "nicks", "rq": "changeSel", "id": this._id};
    client.send(data, rp => {
      this._isSel = !this._isSel;
      this.setSel();
    })
  }

  /** @private */
  check () {
    const client = this._main.client();
    const data = {"page" : "nicks", "rq": "check", "id": this._id};
    client.send(data, rp => {
      this._withIssue = rp["withIssues"];
      this.setIssue();
    })
  }

  /** @private */
  edit () {
    alert("edit");
  }

  /** @private */
  goIssue () {
    alert("goIssue");
  }

// View --------------------------------------------------------------

  /** @private */
  img (id, title) {
    return Ui.img(id).att("title", title);
  }

  /** @private */
  emptyBt (title) {
    return $("div")
      .style(
        "padding:5px;" +
        "border: 1px solid #002040;border-radius: 6px;" +
        "background: #d0ddde;"
      )
      .att("title", title);
  }

  /**
   * @public
   * @param {string} id
   * @return {void}
   */
  setModel (id) {
    this._modelDomo.removeAll().add(id === this._id
      ? this.img("star", _("Model"))
      : Ui.link(e => this.modifyModel())
          .add(Ui.lightImg("star2").att("title", _("Model")))
    );
  }

  /**
   * @private
   * @return {void}
   */
  setIbex () {
    this._ibexDomo.removeAll().add(this._isIbex
      ? Ui.link(e => this.changeIbex()).add(this.img("flag2", "Ibex"))
      : Ui.link(e => this.changeIbex()).add(this.emptyBt("Ibex"))
    );
  }

  /**
   * @private
   * @return {void}
   */
  setSel () {
    this._selDomo.removeAll().add(this._isSel
      ? Ui.link(e => this.changeSel()).add(this.img("flag1", _("Selection")))
      : Ui.link(e => this.changeSel()).add(this.emptyBt(_("Selection")))
    );
  }

  /**
   * @public
   * @return {void}
   */
  setIssue () {
    this._issueDomo.removeAll().add(this._withIssue
      ? Ui.link(e => this.goIssue()).add(this.img("error", _("Problems")))
      : this.img("well", _("Errors"))
    );
  }

  mk () {
    this.setIbex();
    this.setSel();
    this.setIssue();

    return $("table").add($("tr")
      .add($("td").add(this._modelDomo))
      .add($("td")
        .add(Ui.link(e => this.del()).add(this.img("delete", _("Delete")))))
      .add($("td").add(this._ibexDomo))
      .add($("td").add(this._selDomo))
      .add($("td")
        .add(Ui.link(e => this.check()).add(this.img("check", _("Check")))))
      .add($("td").add(Ui.link(e => this.edit()).text(this._nick)))
      .add($("td").add(this._issueDomo))
    );
  }
}

}
