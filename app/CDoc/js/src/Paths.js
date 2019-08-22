// Copyright 18-Aug-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import Domo from "./dmjs/Domo.js"; //eslint-disable-line
import Ui from "./dmjs/Ui.js";
import Main from "./Main.js";  //eslint-disable-line
import {_, _args} from "./I18n.js";
import Dpath from "./data/Dpath.js";  //eslint-disable-line
import Chpass from "./Chpass.js";

const $ = e => Ui.$(e);

/**
    @param {string} chars
    @param {string} id
    return {string} "" is ok, otherwise is error.
**/
function validateChar (chars, id) {
  for (const i of chars) {
    const ch = chars[i];
    if (id.indexOf(ch) !== -1)
      return _args(_("Name '%0' contains '%1'"), id, ch);
  }
  return "";
}

/**
    @param {!Array<Dpath>} dpaths
    @param {string} id
    @return {string} "" is ok, otherwise is error.
**/
function validateId (dpaths, id) {
  const r = id === ""
    ? _("Name is missing")
    : id.indexOf(" ") !== -1
      ? _args(_("Name '%0' contains blanks"), id)
      : validateChar("=@/?", id)
  ;
  return r === ""
    ? dpaths.some(p => p.id === id)
      ? _args(_("Name '%0' is repeated"), id)
      : ""
    : r
  ;
}

/**
    @param {string} path
    @return {string} "" is ok, otherwise is error.
**/
function validatePath (path) {
  if (!path.startsWith("/"))
    return _args(_("Path '%0' does not start with '/'"), path);
  while (path.length > 1 && path.endsWith("/"))
    path = path.substring(0, path.length - 1);
  return path === "/"
    ? _("Path is '/'")
    : path === ""
      ? _("Path is missing")
      : ""
  ;
}

/**
    @param {!Array<Dpath>} dpaths
    @param {string} id
    @return {string} "" is ok, otherwise is error.
**/
function validateIdPath (dpaths, id, path) {
  const err = validateId(dpaths, id);
  return err !== "" ? err : validatePath(path);
}

/**
    Paths page.
**/
export default class Paths {

  /**
      @param {!Main} main
  **/
  constructor (main) {
    this._main = main;
  }

  // View ----------------------------------------------------------------------

  /**
      @param {!Array<!Dpath>} dpaths
  **/
  show (dpaths) {
    this._main.view.removeAll()
      .add($("h2")
        .att("align", "center")
        .text(_("Libraries")))
      .add(
        $("table")
          .att("class", "border")
          .att("border", "0")
          .att("align", "center")
          .att("style", "background-color: rgb(255, 250, 250)")
          .add($("tr")
            .add($("td")
              .add(Ui.img("new").att("style", "vertical-align:-15%")))
            .add($("td")
              .att("id", "newEnter")
              .att("colspan", "2")
              .att("align", "center")
              .add($("button")
                .style("width:80px")
                .att("id", "newEnterBt")
                .add(Ui.img("enter").att("style", "vertical-align:-10%"))
                .on("click", () =>
                  this.newPath(
                    dpaths, $("#nameIn").value(), $("#pathIn").value()
                  )
                )))
            .add($("td").att("id", "newName")
              .add(Ui.field("pathIn").att("id", "nameIn").att("size", "20")))
            .add($("td").att("id", "newPath")
              .add(Ui.field("newEnterBt")
                .att("id", "pathIn").att("size", "60")))
            .add($("td")))
          .add($("tr")
            .add($("td").att("id", "titleInOut").att("width", "18px")
              .add(
                Ui.link(() => this.changeShowAll())
                  .add(Ui.img(this._main.conf.showAll ? "out" : "in"))
              ))
            .add($("td").add(Ui.img("blank")).att("width", "18px"))
            .add($("td").add(Ui.img("blank")).att("width", "18px"))
            .add($("td").html("&nbsp;&nbsp;<b>" + _("Name") + "</b>"))
            .add($("td").html("&nbsp;&nbsp;<b>" + _("Path") + "</b>"))
            .add($("td").add(Ui.img("blank"))))
          .adds(
            dpaths.length > 0
              ? dpaths.filter(p =>
                p.show || this._main.conf.showAll
              ).sort((p1, p2) =>
                p1.id.toUpperCase() > p2.id.toUpperCase() ? 1 : -1
              ).map(entry => {
                const id = entry.id;
                const path = entry.path;
                const sel = entry.show;
                const error = !entry.valid;

                return $("tr")
                  .add($("td").att("id", id + ":InOut")
                    .add(Ui.link(() => this.showPath(id, error))
                      .add(Ui.img(sel ? "out" : "in"))))
                  .add($("td").att("id", id + ":Modify")
                    .style("text-align:center;")
                    .add(
                      Ui.link(() => this.modifyBegin(dpaths, id))
                        .add(Ui.img("edit"))
                    ))
                  .add($("td").att("id", id + ":Delete")
                    .style("text-align:center;")
                    .add(
                      Ui.link(() => this.deletePath(id))
                        .add(Ui.img("delete"))
                    ))
                  .add(
                    $("td").att("class", "border").att("id", id + ":Name")
                      .text(
                        id.length > 20 ? id.substring(0, 17) + "..." : id
                      )
                  )
                  .add(
                    $("td").att("class", "border").att("id", id + ":Path")
                      .text(
                        path.length > 60
                          ? path.substring(0, 57) + "..."
                          : path
                      )
                  )
                  .add(
                    $("td").att("id", id + ":Error")
                      .add(error ? Ui.img("error") : Ui.img("well"))
                  );
              })
              : [$("tr")
                .add($("td")
                  .att("colspan", "6")
                  .att("align", "center")
                  .att("class", "border")
                  .text(_("There are no libraries")))
              ]))
      .add($("p").att("style", "text-align:center")
        .add(Ui.link(() => this.changeLang())
          .att("class", "link")
          .html(_args(_("Change Language to %0"),
            this._main.conf.lang === "es" ? "EN" : "ES")))
        .add($("span").html("&nbsp;|&nbsp;"))
        .add(Ui.link(() => this.changePassPage())
          .att("class", "link")
          .html(_("Change Password"))));
    $("#nameIn").e.focus();
  }

  /**
    @private
    @param {!Array<!Dpath>} dpaths
    @param {string} id
  **/
  modifyBegin (dpaths, id) {
    $("#newEnter").removeAll().add(
      $("div").style("width:80px").add(
        Ui.lightImg("enter").att("style", ";vertical-align:-12%")));
    $("#nameIn").value("").disabled(true);
    $("#pathIn").value("").disabled(true);

    $("#titleInOut").removeAll().add(
      Ui.lightImg(this._main.conf.showAll ? "out" : "in")
    );

    dpaths.forEach(p => {
      const pId = p.id;
      const path = p.path;
      const show = p.show;

      if (pId === id) {
        $("#" + pId + ":InOut").removeAll().add(Ui.img("blank"));
        $("#" + pId + ":Modify").removeAll()
          .add(Ui.link(() => new Main().show()).add(Ui.img("cancel")));
        $("#" + pId + ":Delete").removeAll()
          .add(Ui.link(() =>
            this.modifyPath(
              dpaths,
              pId,
              $("#nameModify").value(),
              $("#pathModify").value()
            )
          ).add(Ui.img("enter")));
        $("#" + pId + ":Name").removeAll()
          .add(Ui.field("pathModify")
            .att("id", "nameModify")
            .att("size", "20")
            .value(pId));
        $("#" + pId + ":Path").removeAll()
          .add(Ui.field("nameModify")
            .att("id", "pathModify")
            .att("size", "60")
            .value(path));
        $("#nameModify").e.focus();
      } else {
        $("#" + pId + ":InOut").removeAll().add(Ui.lightImg(
          show ? "out" : "in"
        ));
        $("#" + pId + ":Modify").removeAll().add(Ui.lightImg("edit"));
        $("#" + pId + ":Delete").removeAll().add(Ui.lightImg("delete"));
      }
    });
  }

  // Control -------------------------------------------------------------------

  /**
      @private
      @param {!Array<!Dpath>} dpaths
      @param {string} id
      @param {string} path
  **/
  async newPath (dpaths, id, path) {
    const err = validateIdPath(dpaths, id, path);
    if (err !== "") {
      alert(err);
      return;
    }
    const rq = {
      "page": "Paths",
      "rq": "new",
      "id": id,
      "path": path
    };
    await Main.client.send(rq);
    new Main().show();
  }

  /**
      @private
  **/
  async changeShowAll () {
    const rq = {
      "page": "Paths",
      "rq": "showAll"
    };
    await Main.client.send(rq);
    new Main().show();
  }

  /**
      @private
      @param {string} id
      @param {boolean} error
  **/
  async showPath (id, error) {
    if (error) {
      alert(_("This source can not be selected, because it does not exist"));
      return;
    }
    const rq = {
      "page": "Paths",
      "rq": "showPath",
      "id": id
    };
    await Main.client.send(rq);
    new Main().show();
  }

  /**
      @private
      @param {string} id
  **/
  async deletePath (id) {
    if (!confirm(_args(_("Delete %0?"), id)))
      return;

    const rq = {
      "page": "Paths",
      "rq": "del",
      "id": id,
    };
    await Main.client.send(rq);
    new Main().show();
  }

  /**
      @private
  **/
  async changeLang () {
    const rq = {
      "page": "Paths",
      "rq": "lang"
    };
    await Main.client.send(rq);
    new Main().show();
  }

  /**
      @private
  **/
  changePassPage () {
    new Chpass(this._main).show();
  }

  /**
      @private
      @param {!Array<!Dpath>} dpaths
      @param {string} id
      @param {string} newId
      @param {string} dpath
  **/
  async modifyPath (dpaths, id, newId, dpath) {
    const err = id === newId
      ? validatePath(dpath)
      : validateIdPath(dpaths, newId, dpath)
    ;
    if (err !== "") {
      alert(err);
      return;
    }

    const rq = {
      "page": "Paths",
      "rq": "modify",
      "id": id,
      "newId": newId,
      "dpath": dpath
    };
    await Main.client.send(rq);
    new Main().show();
  }

}
