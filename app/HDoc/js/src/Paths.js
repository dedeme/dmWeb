// Copyright 28-Feb-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import Client from "./dmjs/Client.js"; // eslint-disable-line
import {_, _args} from "./I18n.js";
import Ui from "./dmjs/Ui.js";
import Domo from "./dmjs/Domo.js"; // eslint-disable-line
import Maybe from "./dmjs/Maybe.js";
import PathEntry from "./data/PathEntry.js"; //eslint-disable-line
import ChangePass from "./ChangePass.js";
import Msg from "./Msg.js";

const $ = e => Ui.$(e);

/**
    @private
    @param {!Array<string>} names
    @param {string} n
    @return string
**/
function validateName (names, n) {
  if (n === "") return _("Name is missing");
  if (names.some(nm => n === nm)) return _("Name is duplicate");
  let r = "";
  " =@/?<&".split("").forEach(c => { if (n.indexOf(c) !== -1) r = c; });
  if (r !== "") return (_args(_("Name contains '%0'"), r));
  return "";
}

/**
    @private
    @param {string} p
    @return string
**/
function validatePath (p) {
  if (p === "") return _("Path is missing");
  return "";
}

/**
    Paths page.
**/
export default class Paths {

  /**
      @param {string} app
      @param {!Client} client
      @param {string} lang
      @param {boolean} showAll
      @param {!Array<!PathEntry>=} paths
  **/
  constructor (app, client, lang, showAll, paths) {
    this._app = app;
    this._client = client;
    this._lang = lang;
    this._showAll = showAll;
    this._paths = paths;
    this._modifying = Maybe.nothing;

    this._wgNewName = Ui.field("newPath").att("size", 20);
    this._wgNewPath = Ui.field("newBt").att("id", "newPath").att("size", 60);
    this._wgShowAll = $("div");
    this._wgMdName = Ui.field("mdPath").att("id", "mdName").att("size", 20);
    this._wgMdPath = Ui.field("mdName").att("id", "mdPath").att("size", 60);
    this._wg = $("div");
    this.view();
  }

  /**
      @return {!Domo}
  **/
  get wg () {
    return this._wg;
  }

  /**
      @return {!Domo}
  **/
  get wgNewName () {
    return this._wgNewName;
  }

  // View ----------------------------------------------------------------------
  /**
      @private
      @return {void}
  **/
  view () {
    const md = this._modifying;
    const newBt = $("button").att("id", "newBt").style("width:80px")
      .add(Ui.img("enter").style("vertical-align:-10%"));
    const showAllImg = this._showAll ? "out" : "in";

    if (md.isJust()) {
      newBt.disabled(true);
      this._wgNewName.disabled(true);
      this._wgNewPath.disabled(true);
      this._wgShowAll.removeAll().add(Ui.lightImg(showAllImg));
    } else {
      newBt.disabled(false).on("click", () => { this.newPath() });
      this._wgNewName.disabled(false);
      this._wgNewPath.disabled(false);
      this._wgShowAll.removeAll()
        .add(Ui.link(() => { this.changeShowAll() })
          .add(Ui.img(showAllImg)));
    }

    const ls = this._paths
      .filter(p => this._showAll || p.selected)
      .sort((p1, p2) => p1.name.toUpperCase() > p2.name.toUpperCase() ? 1 : -1)
    ;
    const rows = ls.length === 0
      ? [
        $("tr").add($("td").klass("frame").style("text-align:center")
          .att("colspan", 6).html(_("There are no libraries")))
      ]
      : ls.map(p => {
        const showImg = p.selected ? "out" : "in";
        const exImg = p.exists ? "well" : "error";
        const wgSh = md.isJust()
          ? md.fromJust() === p.name
            ? Ui.img("blank")
            : Ui.lightImg(showImg)
          : Ui.link(() => { this.setShow(p.name, !p.selected) })
            .add(Ui.img(showImg))
        ;
        const wgMf = md.isJust()
          ? md.fromJust() === p.name
            ? Ui.link(() => { this.editCancel() })
              .add(Ui.img("cancel"))
            : Ui.lightImg("edit")
          : Ui.link(() => { this.edit(p.name) })
            .add(Ui.img("edit"))
        ;
        const wgRm = md.isJust()
          ? md.fromJust() === p.name
            ? Ui.link(() => { this.editAccept(p.name) })
              .add(Ui.img("enter"))
            : Ui.lightImg("delete")
          : Ui.link(() => { this.remove(p.name) })
            .add(Ui.img("delete"))
        ;
        const wgNm = md.isJust()
          ? md.fromJust() === p.name
            ? this._wgMdName.value(p.name)
            : $("span").style("opacity:0.4").text(p.name)
          : $("span").text(p.name)
        ;
        const wgPt = md.isJust()
          ? md.fromJust() === p.name
            ? this._wgMdPath.value(p.path)
            : $("span").style("opacity:0.4").text(p.path)
          : $("span").text(p.path)
        ;

        return $("tr")
          .add($("td").add(wgSh))
          .add($("td").style("text-align:center").add(wgMf))
          .add($("td").style("text-align:center").add(wgRm))
          .add($("td").klass("border").add(wgNm))
          .add($("td").klass("border").add(wgPt))
          .add($("td").add(Ui.img(exImg)))
        ;
      })
      ;

    this._wg.removeAll()
      .add($("div").style("padding-bottom:20px;").klass("head")
        .text(_("Libraries")))
      .add($("table")
        .klass("border")
        .att("align", "center")
        .style("background-color: rgb(255, 250, 250)")
        .add($("tr")
          .add($("td").add(Ui.img("new").style("vertical-align:-15%")))
          .add($("td").att("colspan", 2).add(newBt))
          .add($("td").add(this._wgNewName))
          .add($("td").add(this._wgNewPath))
          .add($("td")))
        .add($("tr")
          .add($("td").add(this._wgShowAll))
          .add($("td").att("colspan", 2))
          .add($("td").html("&nbsp;&nbsp;<b>" + _("Name") + "</b>"))
          .add($("td").html("&nbsp;&nbsp;<b>" + _("Path") + "</b>"))
          .add($("td")))
        .adds(rows))
      .add($("p").style("text-align:center")
        .add(Ui.link(() => { this.changeLang() }).klass("link")
          .text(_args(
            _("Change Language to %0"), this._lang === "es" ? "EN" : "ES"
          )))
        .add($("span").html("&nbsp;|&nbsp;")).klass("link")
        .add(Ui.link(() => { this.changePass() })
          .text(_("Change Password"))))
    ;
  }

  // Control -------------------------------------------------------------------

  /**
      @private
      @return {!Promise<void>}
  **/
  async newPath () {
    const n = this._wgNewName.value().trim();
    const p = this._wgNewPath.value().trim();
    let v = validateName(this._paths.map(e => e.path), n);
    if (v !== "") {
      alert(v);
      return;
    }
    v = validatePath(p);
    if (v !== "") {
      alert(v);
      return;
    }

    const rp = await this._client.send({
      "source": "Paths",
      "rq": "new",
      "name": n,
      "path": p
    });
    const err = rp["error"];
    if (err === "") location.reload();
    else this.errorPg(err);
  }

  /**
      @private
  **/
  async changeShowAll () {
    await this._client.send({
      "source": "Paths",
      "rq": "changeShowAll",
      "value": !this._showAll
    });
    location.reload();
  }

  /**
      @private
      @param {string} name
      @param {boolean} selected
      @return {!Promise<void>}
  **/
  async setShow (name, selected) {
    await this._client.send({
      "source": "Paths",
      "rq": "setShow",
      "name": name,
      "value": selected
    });
    location.reload();
  }

  /**
      @private
      @param {string} name
      @return {void}
  **/
  edit (name) {
    this._modifying = Maybe.just(name);
    this.view();
  }

  /**
      @private
      @return {void}
  **/
  editCancel () {
    this._modifying = Maybe.nothing;
    this.view();
  }

  /**
      @private
      @param {string} name
      @return {!Promise<void>}
  **/
  async editAccept (name) {
    const n = this._wgMdName.value().trim();
    const p = this._wgMdPath.value().trim();
    let v = n === name ? "" : validateName(this._paths.map(e => e.path), n);
    if (v !== "") {
      alert(v);
      return;
    }
    v = validatePath(p);
    if (v !== "") {
      alert(v);
      return;
    }

    const rp = await this._client.send({
      "source": "Paths",
      "rq": "modify",
      "oldName": name,
      "newName": n,
      "path": p
    });
    const err = rp["error"];
    if (err === "") location.reload();
    else this.errorPg(err);
  }

  /**
      @private
      @param {string} name
      @return {!Promise<void>}
  **/
  async remove (name) {
    if (!confirm(_args(_("Delete '%0'?"), name))) return;
    await this._client.send({
      "source": "Paths",
      "rq": "delete",
      "name": name
    });
    location.reload();
  }

  /**
      @private
      @return {!Promise<void>}
  **/
  async changeLang () {
    await this._client.send({
      "source": "Paths",
      "rq": "changeLang",
      "lang": this._lang === "es" ? "en" : "es"
    });
    location.reload();
  }

  /**
      @private
      @return {void}
  **/
  changePass () {
    const pg = new ChangePass(this._app, this._client);
    this._wg.removeAll().add(pg.wg);
    pg.focus();
  }

  /**
      @private
      @return {void}
  **/
  errorPg (err) {
    const pg = new Msg(this._app, err, true);
    this._wg.removeAll().add(pg.wg);
  }

}
