// Copyright 23-Jun-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

using StringTools;

import dm.Domo;
import dm.Ui.Q;
import dm.Ui;
import dm.It;
import dm.Js;
import dm.Str;
import data.Mod;
import data.Settings;
import haxe.ds.Option;

/// Module page.
class Module {

  // Model ---------------------------------------------------------------------

  static function mkModule (code: String): Mod {

    function afterWord (l: String, word: String): String {
      l = l.substring(l.indexOf(word) + word.length + 1).ltrim();
      var i = 0;
      while (i < l.length) {
        if (!Str.isLetterOrDigit(l.charAt(i))) break;
        ++i;
      }
      return l.substring(0, i);
    }

    function countLines (n: Int, start: Int, end: Int): Int {
      final ix = code.indexOf("\n", start);
      if (ix != -1 && ix < end) return countLines(n + 1, ix + 1, end);
      return n;
    }

    function justify (s: String) {
      final cut = s.length - s.ltrim().length;
      return s.split("\n").map(s -> s.substring(cut)).join("\n");
    }

    function ltype (l: String): String {
      if (l.startsWith("///")) return "doc";
      if (l.startsWith("enum ")) return "enum";
      if (l.startsWith("class ")) return "class";
      var isPublic = false;
      var isStatic = false;
      while (true) {
        if (l.startsWith("private ")) break;
        if (l.startsWith("public ")) {
          isPublic = true;
          l = l.substring(7).ltrim();
        } else if (l.startsWith("override ")) {
          isPublic = true;
          l = l.substring(9).ltrim();
        } else if (l.startsWith("static ")) {
          isStatic = true;
          l = l.substring(7).ltrim();
        } else if (l.startsWith("dynamic ")) {
          l = l.substring(8).ltrim();
        } else if (l.startsWith("inline ")) {
          l = l.substring(7).ltrim();
        } else if (l.startsWith("extern ")) {
          l = l.substring(7).ltrim();
        } else if (l.startsWith("final ")) {
          l = l.substring(6).ltrim();
          if (isPublic) {
            if (l.startsWith("function ")) return isStatic ? "sfun" : "fun";
            return isStatic ? "sfinal" : "final";
          }
          break;
        } else if (isPublic) {
          if (l.startsWith("var ")) return isStatic ? "svar" : "var";
          if (l.startsWith("function ")) return isStatic ? "sfun" : "fun";
          break;
        } else {
          break;
        }
      }
      return "";
    }

    final r = new Mod();
    var currentClass: Option<Klass> = None;
    var currentDoc: Option<String> = None;
    var ilong = -2;
    var prev = 0;
    var nline = 1;

    function getDoc (): String {
      switch (currentDoc) {
      case Some(d):
        currentDoc = None;
        return d;
      case None:
        return "";
      }
    }

    function setDoc (d: String): Void {
      switch (r.overview) { case None: r.overview = Some(d); case _: }
      currentDoc = Some(d);
    }

    function doc (ix: Int, l: String): Void {
      var r = (l + " ").substring(4);
      var ix = code.indexOf("\n", ix);
      while (ix != -1) {
        ++nline;
        ++ix;
        prev = ix;
        final ix2 = code.indexOf("\n", ix);
        if (ix2 == -1) {
          ix = -1;
          break;
        }
        final l2 = code.substring(ix, ix2).ltrim();
        if (!l2.startsWith("///")) {
          break;
        }

        r += "\n" + (l2 + " ").substring(4);
        ix = code.indexOf("\n", prev);
      }
      if (ix == -1) {
        prev = code.length;
        return;
      }
      setDoc(r);
    }

    function klass (ix: Int, l: String): Void {
      switch (currentClass) { case Some(c): r.classes.push(c); case _ : }
      final c = new Klass();
      c.id = afterWord(l, "class");
      c.link = c.id;
      c.line = nline;
      c.doc = getDoc();
      prev = code.indexOf("{", ix);
      c.code = justify(code.substring(ix, prev));
      nline += countLines(0, ix, prev);
      currentClass = Some(c);
    }

    function enm (ix: Int, l: String): Void {
      final e = new Mentry();
      e.id = afterWord(l, "enum");
      e.link = e.id;
      e.line = nline;
      e.doc = getDoc();
      prev = code.indexOf("}", ix) + 1;
      e.code = justify(code.substring(ix, prev));
      nline += countLines(0, ix, prev);
      r.enums.push(e);
    }

    function vvar (isStatic: Bool, type: String, ix: Int, l: String): Void {
      final e = new Mentry();
      e.id = afterWord(l, type);
      e.line = nline;
      e.doc = getDoc();
      prev = code.indexOf("\n", ix);
      if (prev == -1) prev = code.length;
      var ix2 = code.indexOf(";", ix);
      if (ix2 != -1 && ix2 < prev) prev = ix2;
      ix2 = code.indexOf("->", ix);
      if (ix2 != -1 && ix2 < prev) prev = ix2 - 1;
      e.code = code.substring(ix, prev);

      switch (currentClass) {
        case Some (c):
          e.link = '${c.id}.${e.id}';
          if (isStatic) c.svars.push(e); else c.ivars.push(e);
        case None:
      }
    }

    function fun (isStatic: Bool, ix: Int, l: String): Void {
      final e = new Mentry();
      e.id = afterWord(l, "function");
      e.line = nline;
      e.doc = getDoc();
      prev = code.indexOf("{", ix);
      e.code = code.substring(ix, prev);
      nline += countLines(0, ix, prev);

      switch (currentClass) {
        case Some (c):
          e.link = '${c.id}.${e.id}';
          if (isStatic) c.sfuns.push(e);
          else if (e.id == "new") c.constructor = Some(e);
          else c.ifuns.push(e);
        case None:
      }
    }

    while (true) {
      if (ilong == -2) ilong = code.indexOf("/*", prev);
      var newl = code.indexOf("\n", prev);

      if (newl == -1) break;

      if (ilong != -1 && ilong < newl) {
        prev = code.indexOf("*/", ilong + 2);

        if (prev == -1) break;

        nline += countLines(0, ilong, prev);
        ilong = -2;
      } else {
        final l = code.substring(prev, newl).trim();
        switch (ltype(l)) {
        case "doc":
          doc(prev, l);
        case "class":
          klass(prev, l);
        case "enum":
          enm(prev, l);
        case "var":
          vvar(false, "var", prev, l);
        case "final":
          vvar(false, "final", prev, l);
        case "svar":
          vvar(true, "var", prev, l);
        case "sfinal":
          vvar(true, "final", prev, l);
        case "fun":
          fun(false, prev, l);
        case "sfun":
          fun(true, prev, l);
        default:
          ++nline;
          prev = newl + 1;
        }
      }
    }

    switch (currentClass) {case Some(c): r.classes.push(c); case _ :}

    r.sort();
    return r;
  }

  // View ----------------------------------------------------------------------

  /// Constructor.
  ///   wg  : Container.
  ///   sett: Settings data.
  ///   lib : Library name.
  ///   path: Module path.
  public static function mk (
    wg: Domo, sett: Settings, lib: String, path: String
  ): Void {

    var mod: Mod;

    final mkRule = label ->
      Q("table")
        .klass("main")
        .add(Q("tr")
          .add(Q("td")
            .add(Q("br"))))
        .add(Q("tr")
          .add(Q("td")
            .style("width: 50px")
            .add(Q("hr")))
          .add(Q("td")
            .style("width: 5px")
            .text(" " + label + " "))
          .add(Q("td")
            .add(Q("hr"))))
    ;

    final mkIndex = (entries: Array<Mentry>) -> {
      final len = entries.length;
      if (len == 0) return Q("div");
      final tb = Q("table").klass("main");
      final COLS = 4;
      final rows = Std.int((len - 1) / COLS) + 1;
      for (r in 0...rows) {
        final tr = Q("tr");
        for (c in 0...COLS) {
          final ix = c * rows + r;
          tr.add(Q("td")
            .style("width: 25%")
            .html(ix < len
              ? '<a href="#${entries[ix].link}">${entries[ix].id}</a>'
              : "")
          );
        }
        tb.add(tr);
      }
      return tb;
    }

    final mkDoc = d -> {
      return Q("table")
        .add(Q("tr")
          .add(Q("td")
            .add(Q("pre")
              .klass("frame0")
              .text(d))))
      ;
    }

    final mkCode = (id: String, code: String) -> {
      var len = code.length;

      function word (c: Int): Int {
        var i = c;
        while (i < len) {
          if (!Str.isLetterOrDigit(code.charAt(i))) break;
          ++i;
        }
        return i;
      }

      function html (s: String): String {
        return Str.html(s).replace("\n", "<br>").replace(" ", "&nbsp;");
      }

      var start = 0;
      while (true) {
        final end = word(start);
        if (code.substring(start, end) == id) {
          return "<tt>" + html(code.substring(0, start)) +
            "<b>" + code.substring(start, end) + "</b>" +
            html(code.substring(end)) + "</tt>"
          ;
        }
        start = end + 1;
        if (start >= len) break;
      }
      return code;
    }

    final henums = () -> mod.enums.length > 0
      ? Q("div")
        .add(mkRule("enums"))
        .add(mkIndex(mod.enums))
      : Q("div")
    ;

    final hclass = (c: Klass) -> {
      final r = Q("div")
        .html("<hr><b>Class<b> <a href='#" + c.link + "'>" + c.id + "</a>");

      final group = (g, es) ->
        r.add(Q("div")
          .html('<br><i>${g}</i>')
          .add(mkIndex(es)))
      ;

      switch (c.constructor) {
        case Some(v): group("Constructor", [v]);
        case None:
      }
      if (c.ivars.length != 0) group("Variables", c.ivars);
      if (c.ifuns.length != 0) group("Functions", c.ifuns);
      if (c.svars.length != 0) group("<b>Static</b> Variables", c.svars);
      if (c.sfuns.length != 0) group("<b>Static</b> Functions", c.sfuns);

      return r;
    };

    final hclasses = () -> mod.classes.length > 0
      ? Q("table")
          .klass("main")
          .add(Q("tr")
            .add(Q("td")
              .add(mkRule("Classes"))))
          .adds(mod.classes.map(c -> hclass(c)))
      : Q("div")
    ;

    final overview = () ->
      Q("div")
        .add(Q("hr"))
        .add(Q("div")
          .klass("frame")
          .html("<b>Overview</b>"))
        .add(Q("div")
          .html("&nbsp;"))
        .add(Q("div")
          .html('<b>File</b><br><a href="?${lib}@${path}&1">${path}</a>'))
        .add(switch (mod.overview) {
            case Some(o): mkDoc(o);
            case None: Q("div");
          })
        .add(Q("hr"))
        .add(Q("div").klass("frame").html(""))
    ;

    final bentry = e ->
      Q("div")
        .add(Q("div")
          .html(
            '<a name = "${e.link}"></a><hr>' +
            '<a href="?${lib}@${path}&${Std.string(e.line)}">${e.id}</a>'
          ))
        .add(Q("div")
          .style("color: #304060")
          .html(mkCode(e.id, e.code)))
        .add(mkDoc(e.doc))
    ;

    final body = () -> mod.entries().map(e -> bentry(e)).to();

    final view = () -> {
      wg
        .removeAll()
        .add(Q("div")
          .klass("frame2")
          .html("<b>" + path + "</b>"))
        .add(henums())
        .add(hclasses())
        .add(overview())
        .adds(body())
        .adds(It.range(25).to().map(i -> Q("p").html("&nbsp;")))
      ;
    }

    switch (It.from(sett.paths).find(p -> p.lib == lib)) {
    case Some (p):
      Cts.client.send([
        "source" => Js.ws("Module"),
        "path" => Js.ws(p.path + "/" + path + ".hx")
      ], rp -> {
        final code = rp["code"].rs();
        if (code != "") {
          mod = mkModule(code);
        }
        view ();
      });
    case None:
      view ();
    }
  }

}
