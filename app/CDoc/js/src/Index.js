// Copyright 18-Aug-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import Domo from "./dmjs/Domo.js"; //eslint-disable-line
import Ui from "./dmjs/Ui.js";
import Main from "./Main.js";
import {_} from "./I18n.js";
import IndexTree from "./data/IndexTree.js";

const $ = e => Ui.$(e);

/**
    Index page.
**/
export default class Index {

  /**
      @param {!Main} main
  **/
  constructor (main) {
    this._main = main;
  }

  // View ----------------------------------------------------------------------

  /**
      @param {string} pathId
  **/
  show (pathId) {
    $("@title").text(pathId);
    this.update(pathId);
  }

  /**
      @private
  **/
  mkEmptyTree () {
    this._main.view.removeAll()
      .add($("table").att("align", "center")
        .add($("tr").add($("td").add($("div").klass("frame").html(
          _("Library not found or without '.h' files.") + "<br><br>" +
          _("Click <a href='?@'>here</a> to continue.")
        )))));
  }

  /**
      @private
      @param {string} id
      @param {!IndexTree} tree
      @return {void}
  **/
  mkTree (id, tree) {
    const linkPrefix = "?" + id + "@";

    /**
        @param {!IndexTree} t1
        @param {!IndexTree} t2
    **/
    function fsort (t1, t2) {
      return t1.isPath === t2.isPath
        ? t1.id.toUpperCase() > t2.id.toUpperCase() ? 1 : -1
        : t1.isPath ? 1 : -1
      ;
    }

    /**
        @param {!Array<!Domo>} trs
        @param {!Array<!IndexTree>} trees
        @param {string} path
        @param {number} space
    **/
    function add (trs, trees, path, space) {
      trees.sort(fsort);
      for (const e of trees) {
        if (e.isPath) {
          trs.push($("tr")
            .add($("td").style(`padding-left:${space}px`)
              .html(`<b>${e.id}</b>`))
            .add($("td"))
          );
          const newPath = path === "" ? e.id : path + "/" + e.id;
          add(trs, e.trees, newPath, space + 20);
        } else {
          trs.push($("tr")
            .add($("td").style(`width:10px;padding-left:${space}px`)
              .html(`<a href="${linkPrefix}${path}/${e.id}">${e.id}</a>`))
            .add($("td").style("padding-left:10px").text(e.doc))
          );
        }
      }
    }

    const trs = [];
    add(trs, tree.trees, "", 0);
    this._main.view.removeAll().add($("div").klass("frame")
      .add($("table").klass("main").adds(trs)));
  }

  // Control -------------------------------------------------------------------

  /**
      @private
      @param {string} id
  **/
  async update (id) {
    const rq = {
      "page": "Index",
      "id": id
    };

    const /** !Object<string, !Array<?>> */ rp = await Main.client.rq(rq);
    const /** !IndexTree */ tree = IndexTree.fromJs(rp["tree"]);
    if (tree.trees.length === 0) this.mkEmptyTree();
    else this.mkTree(id, tree);
  }

}
