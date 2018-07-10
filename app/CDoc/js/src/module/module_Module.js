// Copyright 10-Mar-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

goog.provide("module_Module");
goog.require("module_C");
goog.require("module_CResult");
goog.require("module_ModuleHelp");
goog.require("module_HelpFinal");

module_Module = class {
  /**
   * @param {string} line
   * @param {number} ix
   * @return {string}
   */
  static readNameBk (line, ix) {
    while (--ix >= 0) {
      if (/\w|\d/.test(line.charAt(ix))) {
        break;
      }
    }
    if (ix === -1) {
      return "???";
    }
    const end = ix + 1;
    while (--ix >= 0) {
      if (!/\w|\d/.test(line.charAt(ix))) {
        break;
      }
    }
    return line.substring (ix + 1, end);
  }

  /**
   * @param {string} line
   * @param {number} ix
   * @return {string}
   */
  static readName (line, ix) {
    const lg = line.length;
    while (++ix < lg) {
      if (/\w|\d/.test(line.charAt (ix))) {
        break;
      }
    }
    if (ix === lg) {
      return "???";
    }
    const begin = ix;
    while (++ix < lg) {
      if (!/\w|\d/.test(line.charAt (ix))) {
        break;
      }
    }
    return line.substring (begin, ix);
  }

  /**
   * @param {string} code
   * @return {string}
   */
  static makeLink (code) {
    let bf = "";
    for (let i = 0; i < code.length; ++i) {
      const ch = code[i];
      if (ch === '#') {
        bf += "_";
      } else if (ch > ' ') {
        bf += ch;
      }
    }

    let ix = bf.indexOf("=");
    const ix2 = bf.indexOf("(");
    if (ix === -1 || (ix2 !== -1 && ix2 < ix)) {
      ix = ix2;
      if (ix !== -1) {
        if (bf.substring(ix).startsWith("(*)")) {
          const ix2 = bf.indexOf("(", ix + 1);
          if (ix2 !== -1) {
            ix = ix2;
          }
        }
      }
    }
    if (ix === -1) {
      ix = bf.indexOf(";");
    }
    if (ix === -1) {
      ix = bf.length;
    }

    return bf.substring(0, ix);
  }

  /**
   * @param {!It<string>} file
   * @param {string} line
   * @return {string}
   */
  static readCode (file, line) {
    let bf = "";
    const l = line.trim() + "\n";
    if (l.startsWith("#ifndef ")) {
      return "overview";
    }
    if (l.startsWith("#define ")) {
      while (true) {
        bf += line;
        if (!line.endsWith("\\\n") || !file.hasNext()) {
          break;
        }
        line = file.next();
      }
      return bf;
    }
    if (l.startsWith("#") || l.startsWith("/")) {
      return "";
    }
    let withBracket = 0;
    while (true) {
      if (withBracket === 0) {
        const smcl = line.indexOf(";");
        let bck = line.indexOf("{");
        if (smcl === -1) {
          if (bck != -1) {
            bck++;
            bf += line.substring(0, bck);
            line = line.substring(bck);
            withBracket = 1;
            continue;
          }
          if (!file.hasNext()) {
            return "";
          }
          bf += line;
          line = file.next();
          continue;
        }
        if (bck === -1 || bck > smcl) {
          bf += line.substring(0, smcl + 1);
          return bf.toString();
        }
        bck++;
        bf += line.substring(0, bck);
        line = line.substring(bck);
        withBracket = 1;
        continue;
      }
      var bckop = line.indexOf("{");
      var bckcl = line.indexOf("}");
      if (bckop === -1) {
        if (bckcl === -1) {
          if (!file.hasNext()) {
            return "";
          }
          bf += line;
          line = file.next();
          continue;
        }
        withBracket--;
        bckcl++;
        bf += line.substring(0, bckcl);
        line = line.substring(bckcl);
        continue;
      }
      if (bckcl === -1) {
        withBracket++;
        bckop++;
        bf += line.substring(0, bckop);
        line = line.substring(bckop);
        continue;
      }
      if (bckop < bckcl) {
        withBracket++;
        bckop++;
        bf += line.substring(0, bckop);
        line = line.substring(bckop);
        continue;
      }
      bckcl++;
      withBracket--;
      bf += line.substring(0, bckcl);
      line = line.substring(bckcl);
      continue;
    }
  }

  /**
   * Reads module model
   * @param {string} path
   * @param {string} code
   * @return {!module_ModuleHelp}
   */
  static read (path, code) {
    const name = path;

    /** @type {!It<string>} */
    const it = It.from(code.split("\n"))
      .map(function (l) { return l + "\n"; });

    /** @type {module_ModuleHelp} */
    let r = null;
    /** @type {module_CResult} */
    let rs = null;
    while (it.hasNext ()) {
      rs = null;
      const l = it.next() + "\n";
      const l2 = l.trim() + "\n";
      if (l2.startsWith ("///")) {
        rs = module_C.readSHelp (it, l);
      } else if (l2.startsWith ("/**")) {
        rs = module_C.readLHelp (it, l);
      }

      if (rs === null) {
        continue;
      }

      const cd = module_Module.readCode(it, rs.line());

      if (r === null) {
        if (cd === "overview") {
          r = new module_ModuleHelp(name, rs.help());
          continue;
        }
        r = new module_ModuleHelp(name, "");
      }

      const cdtrim = cd.trim();

      if (cdtrim.startsWith("#define ")) {
        const name = module_Module.readName(cdtrim, "#define".length);
        r.defines().push(new module_HelpFinal(
          name,
          rs.help(),
          cd,
          module_Module.makeLink(cdtrim)
        ));
        continue;
      }
      if (cdtrim.startsWith("typedef ")) {
        const name = module_Module.readNameBk(cdtrim, cdtrim.length);
        r.typedefs().push(new module_HelpFinal(
          name,
          rs.help(),
          cd,
          module_Module.makeLink(cdtrim)
        ));
        continue;
      }

      if (cdtrim.indexOf("{") != -1) {
        if (cdtrim.startsWith("enum ")) {
          const name = module_Module.readName(cdtrim, "enum".length);
          r.enums().push(new module_HelpFinal(
            name,
            rs.help(),
            cd,
            module_Module.makeLink(cdtrim)
          ));
          continue;
        }
        if (cdtrim.startsWith("struct ")) {
          const name = module_Module.readName(cdtrim, "struct".length);
          r.structs().push(new module_HelpFinal(
            name,
            rs.help(),
            cd,
            module_Module.makeLink(cdtrim)
          ));
          continue;
        }
        if (cdtrim.startsWith("union ")) {
          const name = module_Module.readName(cdtrim, "union".length);
          r.unions().push(new module_HelpFinal(
            name,
            rs.help(),
            cd,
            module_Module.makeLink(cdtrim)
          ));
          continue;
        }
        continue;
      }

      var ix = cdtrim.indexOf("=");
      if (ix != -1) {
        const name = module_Module.readNameBk(cdtrim, ix);
        r.vars().push(new module_HelpFinal(
          name,
          rs.help(),
          cd,
          module_Module.makeLink(cdtrim)
        ));
        continue;
      }

      ix = cdtrim.indexOf("(");
      if (ix != -1) {
        if (cdtrim.substring(ix + 1).trim().startsWith("*")) {
          ix = cdtrim.indexOf(")", ix);
        }
        const name = module_Module.readNameBk(cdtrim, ix);
        r.functions().push(new module_HelpFinal(
          name,
          rs.help(),
          cd,
          module_Module.makeLink(cdtrim)
        ));
        continue;
      }

      if (cdtrim != "") {
        const name = module_Module.readNameBk(cdtrim, cdtrim.length);
        r.vars().push(new module_HelpFinal(
          name,
          rs.help(),
          cd,
          module_Module.makeLink(cdtrim)
        ));
        continue;
      }

    }

    function fsort (e1, e2) {
      return e1.name() > e2.name() ? 1 : e2.name() > e1.name() ? -1 : 0;
    }
    r.enums().sort(fsort);
    r.structs().sort(fsort);
    r.unions().sort(fsort);
    r.typedefs().sort(fsort);
    r.functions().sort(fsort);
    r.vars().sort(fsort);
    r.defines().sort(fsort);

    return r;
  }
}
