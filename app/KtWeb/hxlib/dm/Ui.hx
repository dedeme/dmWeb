// Copyright 16-Jun-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Utilities to access to DOM elements.
package dm;

import haxe.Constraints;
import dm.It;
import dm.Tp;
import dm.Domo;
import dm.Cryp;

/// Utilities to access to DOM elements.
class Ui {

  /// Constructor for Domos
  ///   If 'str' is null returns a Domo with element 'el' (e.g. Q(myTable))
  ///   Otherwise
  ///     - If 'str' starts with '#', returns element by id (e.g. Q("#myTable")
  ///     - If 'str' starts with '@', returns element by id (e.g. Q("@myTable")
  ///     - Otherwise return the indicated object (e.g. Q("table"))
  public static function Q (?str: String, ?el: js.html.Element): dm.Domo {
    if (str == null) {
      return new Domo(el);
    }
    return switch str.charAt(0) {
      case '#': new Domo(
        js.Browser.document.getElementById(str.substring(1)));
      case '@': new Domo(
        js.Browser.document.querySelector(str.substring(1)));
      case _: new Domo(js.Browser.document.createElement(str));
    }
  }


  /// Returns a Domo It:
  ///   If 'str' is an empty string returns all elements of page.
  ///   If 'str' is of form "%xxx" returns elements with name "xxx".
  ///   If 'str' is of form ".xxx" returns elements of class 'xxx'.
  ///   If it is of form "xxx" returns elements with tag name 'xxx'.
  public static function QQ (?str: String): It<Domo> {
    final toIt = function (list: Dynamic): It<Domo> {
      var c = 0;
      final len = list.length;
      return new It<Domo> (
        function () {
          return c < len;
        },
        function () {
          return new Domo(cast(list.item(c++)));
        }
      );
    }
    if (str == "") {
      return toIt(js.Browser.document.getElementsByTagName("*"));
    }
    if (str.charAt(0) == "%") {
      return toIt(js.Browser.document.getElementsByName(str.substring(1)));
    }
    if (str.charAt(0) == ".") {
      return toIt(js.Browser.document.getElementsByClassName(str.substring(1)));
    }
    return toIt(js.Browser.document.getElementsByTagName(str));
  }

  /// Shows a modal message.
  ///   msg: Message. It will be convereted to String
  public static function alert (msg: Dynamic): Void {
    untyped js.Syntax.code("alert")(Std.string(msg));
  }

  /// Shows a modal message and retuns a confirmation.
  ///   msg: Message. It will be convereted to String
  public static function confirm (msg: Dynamic): Bool {
    return untyped js.Syntax.code("confirm")(Std.string(msg));
  }

  /// Shows a modal message and retuns a value.
  ///   msg: Message. It will be convereted to String
  ///   def: Default value. It will be convereted to String
  public static function prompt (msg: Dynamic, ?def: Dynamic): String {
    if (def == null) {
      return untyped js.Syntax.code("prompt")(Std.string(msg));
    } else {
      return untyped js.Syntax.code("prompt")(Std.string(msg), Std.string(def));
    }
  }

  /// Extracts variables of URL. Returns a map with next rules:
  ///   Expresions 'key = value' are changed in ["key" => "value"]
  ///   Expresion only with value are changes by ["its-order-index" => "value"].
  ///     (order-index is zero based)
  /// Example:
  ///   foo.com/bar?v1&k1=v2&v3 -> {"0" => v1, "k1" => v2, "2" => v3}
  /// NOTE: <i>keys and values are not trimized.</i>
  public static function url (): Map<String, String> {
    final search = js.Browser.location.search;
    if (search == "") {
      return new Map();
    }
    return It.from(search.substring(1).split("&")).reduce(
      new Tp(new Map(), 0),
      function (s, e) {
        final ix = e.indexOf("=");
        if (ix == -1) {
          s.e1.set(Std.string(s.e2), StringTools.urlDecode(e));
        } else {
          s.e1.set(
            StringTools.urlDecode(e.substring(0, ix)),
            StringTools.urlDecode(e.substring(ix + 1)));
        }
        return new Tp(s.e1, s.e2 + 1);
      }
    ).e1;
  }

  static final scripts = new Map<String, Domo>();
  /// Loads dynamically a javascript or css file.
  ///   path   : Complete path, including .js or .css extension.
  ///   action : Action after loading
  public static function load (path: String, action: Void -> Void): Void {
    var element: Domo;
    final head = js.Browser.document.getElementsByTagName("head")[0];

    if (scripts.exists(path)) {
      final obj = scripts.get(path);
      if (obj != null && obj.e != null) {
        head.removeChild(obj.e);
      }
      scripts.remove(path);
    }

    if (path.substring(path.length - 3) == ".js") {
      element = Q("script").att("type", "text/javascript").att("src", path);
    } else if (path.substring(path.length - 4) == ".css") {
      element = Q("link").att("rel", "stylesheet").att("type", "text/css")
        .att("href", path);
    } else {
      throw "'" + path + "' is not a .js or .css file";
    }
    scripts.set(path, element);
    head.appendChild(element.e);
    element.e.onload = e -> action();
  }

  /// Loads dynamically several javascript or css files. (they can go mixed).
  ///   paths  : Array with complete paths, including .js or .css extension.
  ///   action : Action after loading
  public static function loads (
    paths: Array<String>, action: Void -> Void
  ): Void {
    var lload: Void -> Void = null;

    lload = function () {
      if (paths.length == 0) {
        action();
      } else {
        load(paths.shift(), lload);
      }
    }
    lload();
  }

  /// Loads a text file from the server which hosts the current page.
  ///   path  : Path of file. Can be absolute, but without protocol
  ///           and name server (e.g. http://server.com/dir/file.txt, must be
  ///           written "/dir/file.txt"), or relative to page.
  ///   action: Callback which receives the text.
  public static function upload (path: String, action: String -> Void): Void {
    final url = path.charAt(0) == "/"
      ? "http://" + js.Browser.location.host + path
      : path;
    final request = new js.html.XMLHttpRequest();
    request.onreadystatechange = () ->
      if (request.readyState == 4) action(request.responseText);

    request.open("GET", url, true);
    request.send();
  }

  /// Management of Drag and Drop of files over an object.
  ///   o      : Object over whom is going to make Drag and Drop. It is supposse
  ///            it has white background
  ///   action : Action to make with files.
  ///
  /// NOTE: <i>For accessing to single files use <tt>fileList.item(n)</tt>. You
  /// can know the file number of files with <tt>fileList.length</tt>.</i>
  public static function ifiles (
    o: Domo, action: js.html.FileList->Void
  ): Domo {
    var style = o.getAtt("style");
    function handleDragOver (evt) {
      o.att("style", style + ";background-color: rgb(240, 245, 250);");
      evt.stopPropagation();
      evt.preventDefault();
      evt.dataTransfer.dropEffect = 'copy';
    }
    o.e.addEventListener("dragover", handleDragOver, false);

    o.e.addEventListener("dragleave", function (evt) {
      o.att("style", style);
    }, false);

    function handleDrop (evt) {
      o.att("style", style);
      evt.stopPropagation();
      evt.preventDefault();

      action(evt.dataTransfer.files);

    }
    o.e.addEventListener("drop", handleDrop, false);

    return o;
  }

  /// Changes key point of keyboard number block by comma.
  ///   inp : An input of text type.
  public static function changePoint (inp: Domo): Domo {
    var el: js.html.InputElement = cast(inp.e);
    final ac = el.onkeydown;
    el.onkeydown = function (e) {
      if (e.keyCode == 110) {
        var start = el.selectionStart;
        var end = el.selectionEnd;
        var text = el.value;

        el.value = text.substring(0, start) + "," + text.substring(end);
        el.selectionStart = start + 1;
        el.selectionEnd = start + 1;

        return false;
      }

      if (ac != null) ac(e);
      return true;
    }
    return inp;
  }

  /// Creates a image with border='0'.
  ///   name : Image name. If its extension is not indicated, the extension
  ///          '.png' will be used.
  ///          It must be placed in a directory named 'img'.
  public static function img (name: String): Domo {
    if (name.indexOf(".") == -1) name = name + ".png";
    return Q("img").att("src", "img/" + name);
  }

  /// Creates a image with border='0' and a 'opacity:0.4'.
  ///   name : Image name without extension ('.png' will be used).
  ///          It must be placed in a directory named 'img'.
  public static function lightImg (name: String): Domo {
    return img(name).att("style", "opacity:0.4");
  }

  /// Creates a text field which passes focus to another element.
  ///   targetId : Id of element which will receive the focus.
  public static function field (targetId: String): Domo {
    var r = Q("input").att("type", "text");
    r.e.onkeydown = function (e) {
      if (e.keyCode == 13) {
        e.preventDefault();
        Q("#" + targetId).e.focus();
      }
    }
    return r;
  }

  /// Creates a password field which passes focus to another element.
  ///   targetId : Id of element which will receive the focus.
  public static function pass (targetId: String): Domo {
    var r = Q("input").att("type", "password");
    r.e.onkeydown = function (e) {
      if (e.keyCode == 13) {
        e.preventDefault();
        Q("#" + targetId).e.focus();
      }
    }
    return r;
  }

  /// Create a link to a function.
  ///  f : Function to execute.
  public static function link (f: Function): Domo {
    return Q("span").att("style", "cursor:pointer").on(CLICK, f);
  }

  /// Create a select with list as entries. Every option has an id formed with
  /// 'idPrefix' + "_" + 'its list name' and a name equals to 'idPrefix'.
  /// The select widget has also the name 'idPrefix'.
  ///   idPrefix : Prefix to make option id.
  ///   list     : Entries of select. Default selected goes marked with '+'
  ///     (e.g. ["1", "+2", "3"])
  public static function select (
    idPrefix: String,
    list: Array<String>
  ): Domo {
    var r = Q("select").att("id", idPrefix);
    It.from(list).each(function (tx) {
      var op = Q("option");
      if (tx.length > 0 && tx.charAt(0) == "+") {
        tx = tx.substring(1);
        op.att("selected", "true");
      }
      op.text(tx).att("name", idPrefix).att("id", idPrefix + "_" + tx);
      var sEl:js.html.SelectElement = cast(r.e);
      var oEl:js.html.OptionElement = cast(op.e);

      sEl.add(oEl, null);
    });
    return r;
  }

  /// Emits a beep.
  public static function beep (): Void {
    final au = new js.html.audio.AudioContext();
    final o = au.createOscillator();
    o.frequency.value = 990;
    o.connect(au.destination);
    o.start(0);
    haxe.Timer.delay(() -> o.stop(0), 80).run();
  }

  /// Shows a image to scroll to top.
  /// image: Image name. If it has not extension, '.png' will be
  ///        used. It must be placed in a directory named 'img'.
  public static function upTop (image: String): Domo {
    return Q("div").style("position: fixed;bottom: 0px;right: 20px")
      .add(link(e -> js.Browser.window.scroll(0, 0))
        .add(img(image)));
  }

  /// Circle with a color.
  ///   color: Like "#d0ddde" or "rgb(245, 245, 248)".
  ///   side : Width and height in pixels of led.
  public static function led (color: String, side = 6): Domo {
    return Q("div")
      .style(
        "padding:" + side + "px;" +
         "border: 1px solid #002040;border-radius: " + side + "px;" +
         "background: " + color + ";"
      );
  }

  /// Returns a 'hrule' with a label.
  ///   label: 'Hrule' label.
  ///   margin: Length of line at left of title in pixels.
  public static function hrule (label: String, margin = 50): Domo {
    return Q("table")
      .style(
        "border-collapse: collapse;" +
        "border : 0px;" +
        "width : 100%")
      .add(Q("tr")
        .add(Q("td")
          .style("width: " + margin + "px")
          .add(Q("hr")))
        .add(Q("td")
          .style("white-space:nowrap; width: 5px")
          .text(" " + label + " "))
        .add(Q("td")
          .add(Q("hr"))))
    ;
  }

  /// Returns x position of mouse in browser window.
  public static function mouseX (ev: js.html.MouseEvent): Int {
    return js.Browser.document.documentElement.scrollLeft +
      js.Browser.document.body.scrollLeft +
      ev.clientX
    ;
  }

  /// Returns y position of mouse in browser window.
  public static function mouseY (ev: js.html.MouseEvent): Int {
    return js.Browser.document.documentElement.scrollTop +
      js.Browser.document.body.scrollTop +
      ev.clientY
    ;
  }

}
