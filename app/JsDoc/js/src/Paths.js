// Copyright 06-Sept-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// eslint-disable-next-line
import Main from "./Main.js";
import {_, _args} from "./I18n.js";
import Chpass from "./core/Chpass.js";
import Ui from "./dmjs/Ui.js";

const $ = Ui.$;

/** Paths page. */
export default class Paths {
  /**
   * @param {!Main} main Main page
   */
  constructor (main) {
    /**
     * @private
     * @type {!Main}
     */
    this._main = main;
  }

  /** @private */
  validate (id, path) {
    const paths = this._main.model.paths;
    const msg = ch => _args(_("Name '%0' contains '%1'"), id, ch);
    const contains = ch => id.indexOf(ch) !== -1;

    if (id === "") {
      return _("Name is missing.");
    }
    if (path === "") {
      return _("Path is missing.");
    }
    if (paths.some(p => p.id === id)) {
      return _args(_("Name '%0' is repeated"), id);
    }
    for (const ch of "=@/&") {
      if (contains(ch)) {
        return msg(ch);
      }
    }
    if (contains(" ")) {
      return _args(_("Name '%0' contains blanks"), id);
    }
    return "";
  }

  /** @private */
  async addPath (id, path) {
    const main = this._main;

    id = id.trim();
    path = path.trim();
    while (path.endsWith("/")) {
      path = path.substring(0, path.length - 1);
    }
    path = path.trim();

    const error = this.validate(id, path);
    if (error !== "") {
      alert(error);
      return;
    }

    const rq = {
      "page": "paths",
      "rq": "addPath",
      "id": id,
      "path": path
    };
    await main.client.send(rq);
    main.run();

  }

  /** @private */
  async changeShowAll () {
    const main = this._main;
    const showAll = !main.model.showAll;
    const rq = {
      "page": "paths",
      "rq": "setShowAll",
      "showAll": showAll
    };
    await main.client.send(rq);
    main.run();
  }

  /** @private */
  async changeLang () {
    const main = this._main;
    const lang = main.model.lang === "es" ? "en" : "es";
    const rq = {
      "page": "paths",
      "rq": "setLang",
      "lang": lang
    };
    await main.client.send(rq);
    main.run();
  }

  /** @private */
  changePass () {
    new Chpass(this._main).show();
  }

  /** @private */
  async setShow (id, value) {
    const main = this._main;
    const rq = {
      "page": "paths",
      "rq": "setShow",
      "id": id,
      "value": value
    };
    await main.client.send(rq);
    main.run();
  }

  /** @private */
  async deletePath (id) {
    if (confirm(_args(_("Delete %0?"), id))) {
      const main = this._main;
      const rq = {
        "page": "paths",
        "rq": "delete",
        "id": id
      };
      await main.client.send(rq);
      main.run();
    }
  }

  async modify (oldId, newId, path) {
    const self = this;
    const main = self._main;

    newId = newId.trim();
    path = path.trim();
    while (path.endsWith("/")) {
      path = path.substring(0, path.length - 1);
    }
    path = path.trim();
    if (path === "") {
      alert(_("Path is missing."));
      self.modifyShow(oldId);
      return;
    }

    if (oldId !== newId) {
      const error = this.validate(newId, path);
      if (error !== "") {
        alert(error);
        self.modifyShow(oldId);
        return;
      }
    }

    const rq = {
      "page": "paths",
      "rq": "modify",
      "oldId": oldId,
      "newId": newId,
      "path": path
    };
    await main.client.send(rq);
    main.run();
  }

  // ____
  // View ------------------------------------------------------------
  // TTTT

  /** @private */
  modifyShow (id) {
    const self = this;
    const main = self._main;
    const model = main.model;
    const paths = model.paths.filter(p => model.showAll || p.show);

    $("#newEnter").removeAll().add(Ui.lightImg("enter"));
    $("#nameIn").value("").disabled(true);
    $("#pathIn").value("").disabled(true);

    $("#titleInOut").removeAll()
      .add(Ui.lightImg(model.showAll ? "out" : "in"));

    for (const p of paths) {
      if (p.id === id) {
        $("#" + id + ":InOut").removeAll().add(Ui.img("blank"));
        $("#" + id + ":Modify").removeAll()
          .add(Ui.link(() => {
            self.show();
          }).add(Ui.img("cancel")));
        $("#" + id + ":Delete").removeAll()
          .add(Ui.link(() => {
            self.modify(id, $("#nameModify").value(), $("#pathModify").value());
          }).add(Ui.img("enter")));
        $("#" + id + ":Name").removeAll()
          .add(Ui.field("pathModify")
            .att("id", "nameModify")
            .att("size", 20)
            .value(id));
        $("#" + id + ":Path").removeAll()
          .add(Ui.field("nameModify")
            .att("id", "pathModify")
            .att("size", 60)
            .value(p.path));
        $("#nameModify").e.select();
      } else {
        $("#" + p.id + ":InOut").removeAll()
          .add(Ui.lightImg(p.show ? "out" : "in"));
        $("#" + p.id + ":Modify").removeAll().add(Ui.lightImg("edit"));
        $("#" + p.id + ":Delete").removeAll().add(Ui.lightImg("delete"));
      }
    }
  }

  /** @private */
  mkRow (path) {
    const self = this;

    const id = path.id;
    const ppath = path.path;
    return $("tr")
      .add($("td").att("id", id + ":InOut")
        .add(Ui.link(() => {
          self.setShow(id, !path.show);
        })
          .add(Ui.img(path.show ? "out" : "in"))))
      .add($("td").att("id", id + ":Modify").style("text-align:center")
        .add(Ui.link(() => {
          self.modifyShow(id);
        })
          .add(Ui.img("edit"))))
      .add($("td").att("id", id + ":Delete").style("text-align:center")
        .add(Ui.link(() => {
          self.deletePath(id);
        })
          .add(Ui.img("delete"))))
      .add($("td")
        .klass("border")
        .att("id", id + ":Name")
        .text(id.length > 20 ? (id.substring(0, 17) + "...") : id))
      .add($("td")
        .klass("border")
        .att("id", id + ":Path")
        .text(ppath.length > 60 ? (ppath.substring(0, 57) + "...") : ppath))
      .add($("td")
        .att("id", id + ":Error")
        .add(Ui.img(path.ok ? "well" : "error")))
    ;
  }

  /**
   * @return {void}
   */
  show () {
    const self = this;
    const main = self._main;
    const model = main.model;
    const paths = model.paths.filter(p => model.showAll || p.show);

    const pg =
      $("div")
        .add($("h2").att("align", "center").html(_("Libraries")))
        .add($("table")
          .att("border", 0)
          .att("align", "center")
          .klass("border")
          .style("background-color: rgb(255, 250, 250)")
          .add($("tr")
            .add($("td")
              .add(Ui.img("new").style("vertical-align:-15%")))
            .add($("td")
              .att("id", "newEnter")
              .att("align", "center")
              .att("colspan", 2)
              .add($("button")
                .att("id", "newEnterBt")
                .on("click", () => {
                  self.addPath($("#nameIn").value(), $("#pathIn").value());
                })
                .add(Ui.img("enter").style("vertical-align:-10%"))))
            .add($("td")
              .add(Ui.field("pathIn").att("id", "nameIn").att("size", 20)))
            .add($("td")
              .add(Ui.field("newEnterBt").att("id", "pathIn").att("size", 60))))
          .add($("tr")
            .add($("td").att("id", "titleInOut").style("width:18px")
              .add(Ui.link(() => {
                self.changeShowAll();
              })
                .add(Ui.img(model.showAll ? "out" : "in"))))
            .add($("td").style("width:18px").add(Ui.img("blank")))
            .add($("td").style("width:18px").add(Ui.img("blank")))
            .add($("td").html("&nbsp;&nbsp;<b>" + _("Name") + "</b>"))
            .add($("td").html("&nbsp;&nbsp;<b>" + _("Path") + "</b>"))
            .add($("td").add(Ui.img("blank"))))
          .adds(paths.length === 0
            ? [$("tr")
              .add($("td")
                .att("colspan", 6)
                .att("align", "center")
                .klass("frame")
                .text(_("There is no library")))]

            : paths.map(p => self.mkRow(p))
          ))
        .add($("p").style("text-align:center")
          .add(Ui.link(() => {
            self.changeLang();
          })
            .klass("link")
            .html(_args(
              _("Change Language to %0"),
              model.lang === "en" ? "ES" : "EN"
            )))
          .add($("span").html("&nbsp;|&nbsp;"))
          .add(Ui.link(() => {
            self.changePass();
          })
            .klass("link")
            .html(_("Change Password"))))

    ;

    main.dom.show(pg);
    $("#nameIn").e.focus();
  }
}
