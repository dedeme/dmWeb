// Copyright 03-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import {Mod, ModEntry} from "./Mod.js";

/**
    @private
    @param {string} s Must start with [_ | [a-z] | [A-Z]]
    @return {string} Can be ""
**/
function takeName (s) {
  if (s.length > 0 && s.charAt(0) === "(") {
    const ix = s.indexOf(")");
    if (ix > 1) return s.substring(0, ix + 1);
    return "";
  }
  let r = "";
  for (let i = 0; i < s.length; ++i) {
    const ch = s.charAt(i);
    if ((ch >= "a" && ch <= "z") || (ch >= "A" && ch <= "Z") || ch === "_")
      r += ch;
    else if (i > 0 && ((ch >= "0" && ch <= "9") || ch === "." || ch === "'"))
      r += ch;
    else
      break;
  }
  return r;
}

/**
    @private
    @param {string} s
    @return {string}
**/
function point (s) {
  if (s.charAt(0) === "(") return s;
  const ix = s.lastIndexOf(".");
  return ix === -1 ? s : s.substring(ix + 1);
}

/**
    Module constructor.
**/
export default class MkMod {

  /**
      @private
      @param {!Array<string>} exports
      @param {!Mod} mod
      @param {!Array<string>} ls
      @param {string} l
      @param {number} nl
      @return {number}
  **/
  static documentation (exports, mod, ls, l, nl) {
    let d = "";
    for (;;) {
      if ((l + " ").startsWith("--- ")) {
        if (l.length > 3) d += l.substring(4);
        d += "\n";
        if (ls.length > 0) {
          l = ls.shift();
          ++nl;
        } else {
          return nl;
        }
      } else break;
    }
    while (l.trim() === "") {
      if (ls.length > 0) {
        l = ls.shift();
        ++nl;
      } else {
        return nl;
      }
    }
    return MkMod.read(exports, d, mod, ls, l, nl);
  }

  /**
      @private
      @param {!Array<string>} exports
      @param {string} doc
      @param {!Mod} mod
      @param {!Array<string>} ls
      @param {string} l
      @param {number} nl
      @return {number}
  **/
  static module (exports, doc, mod, ls, l, nl) {
    mod.overview.push(new ModEntry(0, "", "", doc, []));
    let code = "";
    for (;;) {
      const ix = l.indexOf("where");
      if (ix === -1) {
        if (ls.length === 0) return nl;
        code += l;
        l = ls.shift();
        ++nl;
      } else {
        code += l.substring(0, ix);
        break;
      }
    }

    const start = code.indexOf("(");
    const end = code.lastIndexOf(")");
    exports.es = [];
    if (start !== -1 && end !== -1) {
      const ids = code.substring(start + 1, end).split(",");
      ids.forEach(i => {
        const id = takeName(i.trim());
        if (id !== "") exports.push(point(id));
      });
    }

    return nl;
  }

  /**
      @private
      @param {number} ix
      @param {!Array<string>} exports
      @param {string} doc
      @param {!Mod} mod
      @param {!Array<string>} ls
      @param {string} l
      @param {number} nl
      @return {number}
  **/
  static reserved (ix, exports, doc, mod, ls, l, nl) {
    const nm = nl;
    let def = l;
    for (;;) {
      if (ls.length === 0) break;
      if (ls[0] !== "" && ls[0].charAt(0) > " ") break;
      def += ("\n" + ls.shift());
      ++nl;
    }
    const id = takeName(def.substring(ix).trim());
    const i = exports.indexOf(id);
    if (i !== -1) {
      mod.entries.push(new ModEntry(nm, id, def, doc, []));
      exports.splice(i, 1);
    }
    return nl;
  }

  /**
      @private
      @param {!Array<string>} exports
      @param {string} doc
      @param {!Mod} mod
      @param {!Array<string>} ls
      @param {string} l
      @param {number} nl
      @return {number}
  **/
  static fun (exports, doc, mod, ls, l, nl) {
    const id = takeName(l.trim());
    const i = exports.indexOf(id);
    if (i !== -1) {
      const nm = nl;
      let def = l;
      for (;;) {
        if (ls.length === 0) break;
        if (ls[0] !== "" && ls[0].charAt(0) > " ") break;
        def += ("\n" + ls.shift());
        ++nl;
      }

      mod.entries.push(new ModEntry(nm, id, def, doc, []));
      exports.splice(i, 1);
    }
    return nl;
  }

  /**
      @private
  **/
  static read (exports, doc, mod, ls, l, nl) {
    const l2 = l + " ";
    if (l2.startsWith("--- "))
      return MkMod.documentation(exports, mod, ls, l, nl);
    if (l2.startsWith("module "))
      return MkMod.module(exports, doc, mod, ls, l, nl);
    if (l2.startsWith("class "))
      return MkMod.reserved(6, exports, doc, mod, ls, l, nl);
    if (l2.startsWith("data "))
      return MkMod.reserved(5, exports, doc, mod, ls, l, nl);
    if (l2.startsWith("type "))
      return MkMod.reserved(5, exports, doc, mod, ls, l, nl);
    if (l2.startsWith("newtype "))
      return MkMod.reserved(8, exports, doc, mod, ls, l, nl);
    return MkMod.fun(exports, doc, mod, ls, l, nl);
  }

  /**
      Makes a module.
      @param {string} code
      @return {!Mod}
  **/
  static run (code) {
    const mod = new Mod();

    const ls = code.split("\n");
    const exports = [];
    let l = "";
    let nl = 0;

    for (;;) {
      if (ls.length === 0) break;
      l = ls.shift();
      nl = MkMod.read(
        exports, "", mod, ls, l, nl + 1
      );
    }

    exports.forEach(e => {
      mod.entries.push(new ModEntry(-1, e, "", "", []));
    });

    return mod;
  }

}
