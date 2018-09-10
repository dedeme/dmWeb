// Copyright 06-Sept-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/** Module page */

// eslint-disable-next-line
import Main from "./Main.js";
import Ui from "./dmjs/Ui.js";
import It from "./dmjs/It.js";

const $ = Ui.$;

const mkTd = () => {
  return $("td").style("width:25%");
};

const mkFLink = (klass, packDoc) => {
  const id = packDoc[0];
  const link = klass === "" ? `#${id}` : `#${klass}.${id}`;
  return mkTd().html(`<a href = "${link}">${id}</a>`);
};

const mkALink = (klass, classAtt) => {
  const id = classAtt[0];
  const link = `#${klass}.${id}`;
  const type = classAtt[1];
  let html = "";
  if (type === "g") {
    html = `<a href = "${link}.get">${id}</a>`;
  } else if (type === "gs") {
    html +=
      `<a href = "${link}.get">${id}</a> (<a href = "${link}.set">set</a>)`;
  } else {
    html += `<a href = "${link}.set">${id} (set)</a>`;
  }
  return mkTd().html(html);
};

const mkTable = (cells) => {
  const len = cells.length;
  const table = $("table").klass("main");
  const nrows = Math.floor((len - 1) / 4) + 1;
  It.range(nrows).each(r => {
    const tr = $("tr");
    It.range(4).each(c => {
      const ix = c * nrows + r;
      if (ix < len) {
        tr.add(cells[ix]);
      } else {
        tr.add($("td"));
      }
    });
    table.add(tr);
  });
  return table;
};

const mkDoc = (tx) => {
  tx = "\n" + tx;

  tx = tx.replace(/\n\*[ ]?/g, "\n");

  let p1 = tx;
  let p2 = "";
  const ix = tx.indexOf("\n@");
  if (ix !== -1) {
    p1 = tx.substring(0, ix);
    p2 = tx.substring(ix + 1);
  }

  while(p1.startsWith("\n")) {
    p1 = p1.substring(1);
  }

  const td = $("td");
  if (p1 !== "") {
    td.add($("pre").klass("doc").text(p1));
  }
  if (p2 !== "") {
    td.add($("pre").klass("doc").text(p2));
  }

  return $("table").add($("tr").add(td));
};

/** Module page. */
export default class Module {
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

  // ____
  // View ------------------------------------------------------------
  // TTTT


  /** @private */
  mkFunctions (functions) {
    if (functions.length === 0) {
      return $("div");
    }
    const cells = functions.map(packDoc => mkFLink("", packDoc));
    return $("div")
      .add($("div").html("&nbsp;"))
      .add($("div").html("<b>functions</b>"))
      .add(mkTable(cells))
    ;
  }

  /** @private */
  mkConstructor (classId, c) {
    if (c[1] === "") {
      return $("div");
    }
    return $("div").html(`<a href="#${classId}."><i>constructor</i></a>`);
  }

  /** @private */
  mkAtts (isStatic, classId, atts) {
    let title = "<i>Instance Attributes</i>";
    if (isStatic) {
      title = "<i>Static Attributes</i>";
      atts = atts.filter(a =>
        a[1] === "g" || a[1] === "gs"
          ? a[2][1].startsWith("static ")
          : a[3][1].startsWith("static ")
      );
    } else {
      atts = atts.filter(a =>
        a[1] === "g" || a[1] === "gs"
          ? !a[2][1].startsWith("static ")
          : !a[3][1].startsWith("static ")
      );
    }
    if (atts.length === 0) {
      return $("div");
    }
    atts.sort((a, b) => a[0].localeCompare(b[0]));
    const cells = atts.map(classAtt => mkALink(classId, classAtt));
    return $("div")
      .add($("div").html(title))
      .add(mkTable(cells));
  }

  mkMethods (isStatic, classId, methods) {
    let title = "<i>Instance Methods</i>";
    if (isStatic) {
      title = "<i>Static Methods</i>";
      methods = methods.filter(m => m[1].startsWith("static "));
    } else {
      methods = methods.filter(m => !m[1].startsWith("static "));
    }
    if (methods.length === 0) {
      return $("div");
    }
    methods.sort((a, b) => a[0].localeCompare(b[0]));
    const cells = methods.map(packDoc => mkFLink(classId, packDoc));
    return $("div")
      .add($("div").html(title))
      .add(mkTable(cells));
  }

  /** @private */
  mkClass (klass) {
    const id = klass[0];
    return $("div")
      .add($("div").html("&nbsp;"))
      .add($("div").html(`<b>class </b><a href="#${id}"><b>${id}</b></a>`))
      .add(this.mkConstructor(id, klass[3]))
      .add(this.mkAtts(false, id, klass[5]))
      .add(this.mkMethods(false, id, klass[4]))
      .add(this.mkAtts(true, id, klass[5]))
      .add(this.mkMethods(true, id, klass[4]))
    ;
  }

  /** @private */
  mkClasses (classes) {
    classes.sort((a, b) => a[0].localeCompare(b[0]));
    return classes.map(klass => this.mkClass(klass));
  }

  /** @private */
  mkHead (mdoc) {
    const model = this._main.model;
    const functions = mdoc[1];
    const classes = mdoc[2];
    return $("div")
      .add($("div").klass("frame2").html("<b>" + model.module + "</b>"))
      .add(this.mkFunctions(functions))
      .adds(this.mkClasses(classes))
    ;
  }

  /** @private */
  mkOverview (mdoc) {
    const model = this._main.model;
    const mod = model.module;
    const sel = model.sel;
    const selMod = `${sel}@${mod}`;
    return $("div")
      .add($("hr"))
      .add($("div").klass("frame").html("<b>Overview</b>"))
      .add($("div").html("&nbsp;"))
      .add($("div").html(`<b>File</b><br><a href="?${selMod}&hp:">${mod}</a>`))
      .add(mkDoc(mdoc[0]))
      .add($("hr"))
      .add($("div").klass("frame").html(""))
    ;
  }

  /** @private */
  bodyEntry (type, classId, packDoc) {
    const model = this._main.model;
    const sel = model.sel;
    const mod = model.module;
    let id = packDoc[0];
    if (type === "Get") {
      id = id + ".get";
    }
    if (type === "Set") {
      id = id + ".set";
    }
    const linkId =
      (classId === ""
        ? ""
        : id === "" && type === "Class" ? classId : `${classId}.`) + id;

    const link = `${sel}@${mod}&hp:${linkId}`;
    const lk = type === "Constructor"
      ? `[<a href="?${link}">code</a>]`
      : `<a href="?${link}">${id}</a>`
    ;
    return $("div")
      .add($("div").att("id", linkId).html("&nbsp;"))
      .add($("div").html(`<b>${type}</b> ${lk}`))
      .add($("pre").klass("def").text(packDoc[1]))
      .add(mkDoc(packDoc[2]))
      .add($("hr"))
    ;
  }

  /** @private */
  mkBody (mdoc) {
    const body = $("div");
    for (const fn of mdoc[1]) {
      body.add(this.bodyEntry("Function", "", fn));
    }
    for (const cl of mdoc[2]) {
      const classId = cl[0];
      body.add(this.bodyEntry("Class", "", cl));
      const construct = cl[3];
      if (construct[1] !== "") {
        body.add(this.bodyEntry("Constructor", classId, construct));
      }
      for (const att of cl[5]) {
        const type = att[1];
        if (type === "g" || type === "gs") {
          body.add(this.bodyEntry("Get", classId, att[2]));
        }
        if (type === "s" || type === "gs") {
          body.add(this.bodyEntry("Set", classId, att[3]));
        }
      }
      for (const meth of cl[4]) {
        body.add(this.bodyEntry("Method", classId, meth));
      }
    }

    return body;
  }

  /** @private */
  mkFoot () {
    return $("div")
      .adds([...It.range(35).map(() => $("div").html("&nbsp"))])
      .add($("div").style("position: fixed;bottom: 0px;right: 20px")
        .add(Ui.link(() => location.assign("#"))
          .add(Ui.img("up"))))
    ;
  }

  /** @private */
  show2 (mdoc) {
    const main = this._main;
    const pg = $("div")
      .add(this.mkHead(mdoc))
      .add(this.mkOverview(mdoc))
      .add(this.mkBody(mdoc))
      .add(this.mkFoot())
    ;
    main.dom.show(pg);
    $("@title").text(main.model.module);
  }

  /**
   * @return {void}
   */
  show () {
    const self = this;
    const main = self._main;
    const sel = main.model.sel;
    const module = main.model.module;
    const path = main.model.paths.find(p => p.id === sel);
    if (path === undefined || module === "") {
      main.go("@");
      return;
    }

    const rq = {
      "page": "module",
      "path": path.path + "/" + module + ".js"
    };
    main.client.send(rq, rp => {
      const mdoc = rp["mdoc"];
      if (mdoc === undefined) {
        main.go("@");
        return;
      }
      self.show2(mdoc);
    });
  }
}
