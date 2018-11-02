// Copyright 10-Mar-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

goog.provide("module_View");

goog.require("module_Module");
goog.require("module_Model");

{
  function addTopLinks () {
    const headers = document.getElementsByTagName('h3');
    for (let i = 0; i < headers.length; ++i) {
      const span = document.createElement('span');
      span.className/**/ = 'navtop';
      const link = document.createElement("a");
      span.appendChild(link);
      link.href/**/ = "#top";
      link.className/**/ ="navtop";
      const textNode = document.createTextNode('[Top]');
      link.appendChild(textNode);
      headers[i].appendChild(link);
    }
  }

module_View = class {

  /** @param {!module_Control} control */
  constructor (control) {
    /** @const {!module_Control} */
    this._control = control;
  }

  /**
   * @param {!Array<!Path>} paths
   * @param {string} selected
   * @param {string} modPath
   * @param {string} text
   * @return {void}
   */
  show (paths, selected, modPath, text) {
    const name = modPath;
    const library = selected;
    /** @type {!module_ModuleHelp} */
    const mod = module_Module.read(modPath, text);

    /**
     * @param {string} bf
     * @param {!module_ModuleHelp} mod
     * @return {string}
     */
    function index(bf, mod) {
      const tab = "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;";
      /**
       * @param {!Array<!module_HelpFinal> | !Array<!module_ModuleHelp>} a
       * @param {string} name
       * @return {string}
       */
      function addBlock (a, name) {
        if (a.length === 0) {
          return bf;
        }
        bf += "<tr><td colspan='3'><i>" + name + "</i></td></tr>";
        const h = Math.floor((a.length - 1) / 3) + 1;
        for (let y = 0; y < h; ++y) {
          bf += "<tr>";
          for (let x = 0; x < 3; ++x) {
            const e = a[x * h + y];
            bf += !e
              ? "<td></td>"
              : "<td><a href='#hp:" + e.name() + "'>" +
                tab + e.name() + "</a></td>"
            ;
          }
          bf += "</tr>";
        }
        return bf;
      }

      bf += "<p class='frame2'><b>" +   mod.name() + "</b></p>";

      bf += "<table border=0 width='100%'>";

      addBlock(mod.defines(), "Defines");
      addBlock(mod.enums(), "Enums");
      addBlock(mod.structs(), "Structs");
      addBlock(mod.unions(), "Unions");
      addBlock(mod.typedefs(), "Typedefs");
      addBlock(mod.functions(), "Functions");
      addBlock(mod.vars(), "Variables");

      bf += "</table><hr>";
      return bf;
    }

    /**
     * @param {string} bf
     * @param {!module_ModuleHelp} mod
     * @return {string}
     */
    function overview(bf, mod) {
      bf += "<p class='frame'><b>Overview</b></p>";
      bf += module_Model.makeHelp(mod.help());
      bf += "<p><b>File</b><br><a href='?"
      + library + "@" + mod.name() + "&hp:'>"
      + name + ".h" + "</a>"
      ;
      bf += " | <a href='?"
      + library + "@" + mod.name() + "&hp::'>"
      + name + ".c" + "</a>"
      ;
      bf += "</p><hr>";
      return bf;
    }

    /**
     * @param {string} bf
     * @param {!module_ModuleHelp} mod
     * @return {string}
     */
    function body(bf, mod) {
      /**
       * @param {string} link
       * @param {string} name
       */
      function makeLink (link, name) {
        return "<a href='?" + library + "@" + mod.name()
        + "&hp:" + link + "'>" + name + "</a>";
      }
      /**
       * @param {string} bf
       * @param {!module_ModuleHelp | !module_HelpFinal} e
       * @return {string}
       */
      function endEntry(bf, e) {
        let isNewLine = true;
        let bf2 = "";
        const code = e.code();
        for (let i = 0; i < code.length; ++i) {
          const ch = code[i];
          if (isNewLine) {
            if (/\s/.test(ch)) {
              bf2 += "&nbsp;";
            } else {
              bf2 += ch;
              isNewLine = false;
            }
          } else {
            if (ch == "\n") {
              bf2 += "<br>";
              isNewLine = true;
            } else {
              bf2 += ch;
            }
          }
        }

        bf += "<p><tt>" + bf2.toString()  + "</tt></p>";
        bf += module_Model.makeHelp (e.help());
        bf += "<hr>";
        return bf;
      }
      /**
       * @param {!Array<!module_ModuleHelp> | !Array<!module_HelpFinal>} a
       * @param {string} name
       * @return {void}
       */
      function addBlock(a, name) {
        It.from(a).each(e => {
          bf += "<h3 id='hp:" + e.name() + "'>" + name + " "
          + makeLink (e.link(), e.name()) + "</h3>"
          ;
          bf = endEntry (bf, e);
        });
      }

      addBlock(mod.defines(), "define");
      addBlock(mod.enums(), "enum");
      addBlock(mod.structs(), "struct");
      addBlock(mod.unions(), "union");
      addBlock(mod.typedefs(), "typedef");

      It.from(mod.functions()).each(e => {
        bf += "<h3 id='hp:" + e.name() + "'>function "
        + makeLink (":" + e.link(), e.name()) + "</h3>"
        ;
        bf = endEntry (bf, e);
      });

      addBlock(mod.vars(), "variable");

      return bf;
    }

    let bf = "";
    bf = index (bf, mod);
    bf = overview (bf, mod);
    bf += "<hr class='frame'>";
    bf = body (bf, mod);

    It.range(22).each (function (e) {
      bf += "<p>&nbsp;</p>";
    });

    bf += "<div style='position: fixed;bottom: 0px;right: 20px'>" +
      "<a href='#'><img border='0' src='img/up.png'></a></div>";

    this._control.control().dom().show(
      paths,
      selected,
      $("div").html(bf)
    );
    addTopLinks();

    const iBar = modPath.lastIndexOf("/");
    $$("title").next().text(iBar == -1 ? modPath : modPath.substring(iBar + 1));

    const lc = location.href/**/;
    const ix = lc.indexOf("#");
    if (ix != -1) {
      let e = $(lc.substring(ix)).e();
      if (e) {
        e.scrollIntoView(true);
      }
    }
  }
}}

