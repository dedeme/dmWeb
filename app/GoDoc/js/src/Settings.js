// Copyright 03-Apr-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/**
    Page to show documentation
**/

import Domo from "./dmjs/Domo.js"; //eslint-disable-line
import Ui from "./dmjs/Ui.js";
import Client from "./dmjs/Client.js"; //eslint-disable-line
import PLibRow from "./data/PLibRow.js";  //eslint-disable-line
import {_, _args} from "./I18n.js";
import SimpleList from "./wgs/SimpleList.js";
import EditList from "./wgs/EditList.js";
import ChangePass from "./ChangePass.js";
import Msg from "./Msg.js";
import Cts from "./Cts.js";

const $ = e => Ui.$(e);

/**
    @private
    @param {!Client} client
**/
function bye (client) {
  if (!confirm(_("Application exit?"))) {
    return;
  }
  client.send({
    "source": "Main",
    "rq": "bye",
    "sessionId": client.sessionId()
  });

  const pg = new Msg(Cts.app, _args(_("Logout-message"), Cts.app));
  $("@body").removeAll().add(pg.wg);
}

/**
    Page to show documentation
**/
export default class Doc {
  /**
      @param {!Client} client
      @param {string} lang
      @param {!Array<!PLibRow>} plibs
      @param {!Array<!PLibRow>} slibs
  **/
  constructor (client, lang, plibs, slibs) {
    this._client = client;
    this._lang = lang;
    this._plibs = plibs;
    this._slibs = slibs;

    this._wg = $("div");
    this.view();
  }

  /**
      @return {!Domo}
  **/
  get wg () {
    return this._wg;
  }

  // View ----------------------------------------------------------------------

  /**
      @private
  **/
  view () {
    const lg = this._lang === "en" ? "ES" : "EN";
    const plibs = this._plibs;
    const slibs = this._slibs;

    this._wg.removeAll()
      .add($("table").klass("main")
        .add($("tr")
          .add($("td").att("colspan", 5).style("text-align:center")
            .add(Ui.link(() => this.changeLang()).klass("link")
              .text(_args(_("Change Language to %0"), lg)))
            .add($("span").text(" | "))
            .add(Ui.link(() => this.changePass()).klass("link")
              .text(_("Change Password")))
            .add($("span").text(" | "))
            .add(Ui.link(() => { bye(this._client) })
              .add(Ui.img("cross").style("vertical-align:middle")))))
        .add($("tr").add($("td").att("colspan", 5).html("&nbsp;")))
        .add($("tr").klass("frame0")
          .add($("td").att("colspan", 2).style("text-align:center")
            .html("<b>" + _("Personal Libraries") + "</b>"))
          .add($("td").klass("frame"))
          .add($("td").att("colspan", 2).style("text-align:center")
            .html("<b>" + _("Standard Libraries") + "</b>")))
        .add($("tr").klass("frame")
          .add($("td").style("text-align:center;width:20%").text("Most Used"))
          .add($("td").style("text-align:center;width:40%").text("All"))
          .add($("td").klass("frame").style("width:4px"))
          .add($("td").style("text-align:center;width:20%").text("Most Used"))
          .add($("td").style("text-align:center;width:20%").text("All")))
        .add($("tr")
          .add($("td").style("vertical-align:top;text-align:center")
            .add(new SimpleList(true, plibs).wg))
          .add($("td").style("vertical-align:top;text-align:center")
            .add(new EditList(
              this._client, true, plibs,
              ls => { return this.updateLibs(true, ls) }
            ).wg))
          .add($("td"))
          .add($("td").style("vertical-align:top;text-align:center")
            .add(new SimpleList(false, slibs).wg))
          .add($("td").style("vertical-align:top;text-align:center")
            .add(new EditList(
              this._client, false, slibs,
              ls => { return this.updateLibs(false, ls) }
            ).wg))))

      .add($("p").html("&nbsp;"))
      .add($("hr"))
      .add($("table").klass("main")
        .add($("tr")
          .add($("td")
            .add($("a")
              .att("href", "doc/about.html")
              .att("target", "blank")
              .html("<small>" + _("Help & Credits") + "</small>")))
          .add($("td")
            .style("text-align: right;font-size: 10px;" +
              "color:#808080;font-size:x-small;")
            .html(`- © ºDeme. ${Cts.app} (${Cts.version}) -`))))
      .add(Ui.upTop("up"));
  }

  // Control -------------------------------------------------------------------

  /**
      @private
  **/
  async changeLang () {
    await this._client.send({
      "source": "Settings",
      "rq": "setLang",
      "lang": this._lang === "en" ? "es" : "en"
    });
    location.reload();
  }

  /**
      @private
  **/
  changePass () {
    this._wg.removeAll()
      .add(new ChangePass(Cts.app, this._client).wg)
    ;
  }

  /**
      @private
      @param {boolean} isPersonal
      @param {!Array<string>} paths
      @return {!Promise<void>}
  **/
  async updateLibs (isPersonal, paths) {
    if (paths.length === 0) {
      alert(_("List is empty"));
      return;
    }
    await this._client.send({
      "source": "Settings",
      "rq": "updateLibs",
      "isPersonal": isPersonal,
      "paths": paths
    });
    location.reload();
  }
}

