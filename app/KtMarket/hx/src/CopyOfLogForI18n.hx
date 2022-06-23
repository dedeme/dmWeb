// Copyright 06-Oct-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Log widget.
/// Show entries of a log with rows of type 'LogRow'.
///
/// Example of CLIENT: -----------------------------------------------
///
/// import i18n._;
///
/// final logDiv = Q("div");
///
/// function load (fn: Array<LogRow> -> Void) {
///   Cts.client.send([
///     "page" => Js.ws("summary"),
///     "rq" => Js.ws("log")
///   ], rp -> {
///     final log = rp["log"].ra();
///     fn(log.map(js -> LogRow.fromJs(js)));
///   });
/// }
///
/// function reset (fn: () -> Void) {
///   Cts.client.send([
///     "page" => Js.ws("summary"),
///     "rq" => Js.ws("resetLog")
///   ], rp -> {
///     fn();
///   });
/// }
///
/// wgx
///   .removeAll()
///   .add(logDiv)
/// ;
/// Log.mk(logDiv, load, reset, _);
///
/// Example of SERVER interface (Go): --------------------------------
///
/// case "log":
///	  rp := map[string]json.T{
///	    "log": log.Read(),
///   }
///   return cgi.Rp(ck, rp)
/// case "resetLog":
///   log.Reset()
///   return cgi.RpEmpty(ck)
///
/// Example of SERVER data (Go): -------------------------------------
///
/// type T struct {
///   error bool
///   time  string
///   msg   string
/// }
///
/// func (r *T) ToJs() json.T {
///   return json.Wa([]json.T{
///     json.Wb(r.error),
///     json.Ws(r.time),
///     json.Ws(r.msg),
///   })
/// }
///
/// func FromJs(js json.T) *T {
///   a := js.Ra()
///   return &T{
///     a[0].Rb(),
///     a[1].Rs(),
///     a[2].Rs(),
///   }
/// }

package dm;

using StringTools;
import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Dt;
import dm.Js;
import dm.Opt;

/// Log widget.
/// Show entries of a log with rows of type 'LogRow'.
class Log {
  static var log: Log;

  final tableFrame = "background-color: rgb(240, 245, 250);" +
    "border: 1px solid rgb(110,130,150);" +
    "font-family: sans;font-size: 14px;" +
    "padding: 4px;border-radius: 4px;"
  ;
  final wg: Domo;
  final load: (Array<LogRow> -> Void) -> Void;
  final reset: (() -> Void) -> Void;
  final _: String -> String;
  var rows: Array<LogRow>;
  var minified: Bool;
  final lineWidth: Int;
  final linesNumber: Int;

  var is2Days: Bool;
  var isErrors: Bool;

  function new (
    wg: Domo,
    load: (Array<LogRow> -> Void) -> Void, reset: (() -> Void) -> Void,
    rows: Array<LogRow>, _ : String -> String,
    minified: Bool, lineWidth: Int, linesNumber: Int
  ) {
    this.wg = wg;
    this.load = load;
    this.reset = reset;
    this.rows = rows;
    this._ = _;
    this.minified = minified;
    this.lineWidth = lineWidth;
    this.linesNumber = linesNumber;

    is2Days = false;
    isErrors = true;

    show();
  }

  // View ----------------------------------------------------------------------

  function show (): Void {

    if (minified) view2();
    else view1();
  }

  function view1 () {
    function mkOption (isSel: Bool, id: String, action: () -> Void): Domo {
      final frame = "background-color: rgb(250, 250, 250);" +
        "border: 1px solid rgb(110,130,150);" +
        "padding: 4px;border-radius: 4px;"
      ;
      final link = "text-decoration: none;color: #000080;" +
        "font-weight: normal;cursor:pointer;"
      ;
      var r = Q("span").style(frame);
      if (!isSel) r = Ui.link(e -> action()).style(link);
      return r.text(id);
    }

    final lmenu = Q("div");
    final rmenu = Q("div");
    final area = Q("textarea").att("spellcheck", false)
      .att("readOnly", true)
      .att("rows", linesNumber).att("cols", lineWidth + 5);

    lmenu
      .add(Q("span")
        .add(mkOption(
          is2Days, _("2 Days"), () -> on2Days()
        )))
      .add(Q("span").html(" · "))
      .add(Q("span")
        .add(mkOption(
          !is2Days, _("All"), () -> onAllD()
        )))
    ;

    rmenu
      .add(Q("span")
        .add(mkOption(
          false, _("Reload"), () -> onReload()
        )))
      .add(Q("span").html(" · "))
      .add(Q("span")
        .add(mkOption(
          false, _("Delete"), () -> onDelete()
        )))
      .add(Q("span").html(" | "))
      .add(Q("span")
        .add(mkOption(
          isErrors, _("Errors"), () -> onErrors()
        )))
      .add(Q("span").html(" · "))
      .add(Q("span")
        .add(mkOption(
          !isErrors, _("All"), () -> onAll()
        )))
    ;

    final today = Date.now();
    final log = rows.copy();
    log.reverse();
    area.value(
      log
        .filter(e ->
          (is2Days ? Dt.df(today, e.date) < 3 : true) &&
          (isErrors ? e.isError : true)
        )
        .map(e -> e.format(lineWidth))
        .join("\n")
    );

    wg
      .removeAll()
      .add(Q("div").klass("head").style("padding-bottom:10px").text(_("Log")))
      .add(Q("table").att("align", "center").style(tableFrame)
        .add(Q("tr")
          .add(Q("td").style("text-align:left;width:40%")
            .add(lmenu))
          .add(led())
          .add(Q("td").style("text-align:right;widht:80%")
            .add(rmenu)))
        .add(Q("tr").add(Q("td").att("colspan", 3)))
        .add(Q("tr")
          .add(Q("td").att("colspan", 3).add(area))))
    ;
  }

  function view2 (): Void {
    wg
      .removeAll()
      .add(Q("div").klass("head").style("padding-bottom:10px").text(_("Log")))
      .add(Q("table").att("align", "center").style(tableFrame)
        .add(Q("tr")
          .add(Q("tr")
            .add(led()))))
    ;
  }

  function led (): Domo {
    final warns = rows.length != 0;
    final errs = dm.It.from(rows).indexf(e -> e.isError) != -1;
    return Q("td")
      .style("text-align:center;width:20%")
      .add(Q("span")
        .style(
            "border: 1px solid rgb(110,130,150);" +
            "border-radius: 8px;" +
            "background: " +
              (errs ? "#e04040" : warns ? "#e0e040" : "#ffffff") + ";" +
            "cursor:pointer"
          )
        .html("&nbsp;&nbsp;")
        .on(CLICK, () -> {
            minified = !minified;
            show();
          }))
    ;
  }



  // Control -------------------------------------------------------------------

  function on2Days (): Void {
    is2Days = true;
    view1();
  }

  function onAllD (): Void {
    is2Days = false;
    view1();
  }

  function onReload (): Void {
    load(rows -> {
      this.rows = rows;
      show();
    });
  }

  function onDelete (): Void {
    if (Ui.confirm(_("All log entries will be deleted.\nContinue?"))) {
      reset(() -> {
        onReload();
      });
    }
  }

  function onErrors (): Void {
    isErrors = true;
    view1();
  }

  function onAll (): Void {
    isErrors = false;
    view1();
  }

  // Static --------------------------------------------------------------------

  /// Creates and show a Log widget.
  ///   wg         : Container.
  ///   load       : Functión to call when reloading entries.
  ///   reset      : Function to call when clearing entries.
  ///   _          : I18n function.
  ///   minified   : if it is 'true', shows the widget minified.
  ///   lineWidth  : Sets the line width in characters.
  ///   linesNumber: Sets the lines number of widget.
  public static function mk (
    wg: Domo,
    load: (Array<LogRow> -> Void) -> Void,
    reset: (() -> Void) -> Void,
    _ : String -> String,
    minified = false,
    lineWidth = 120,
    linesNumber = 25
  ): Void {
    load(rows -> {
      log = new Log(wg, load, reset, rows, _, minified, lineWidth, linesNumber);
    });
  }

  /// Reloads log entries and shows them.
  public static function reload(): Void {
    if (log != null) log.onReload();
  }

}

/// Log entry.
class LogRow {
  /// Returns 'true' if the entry is an error.
  public var isError(default, null): Bool;
  /// Returns the date of entry.
  public var date(get, never): Date;
  function get_date() {
    final ix = time.indexOf("(");
    return time.charAt(2) == "-"
      ? Opt.eget(Dt.fromEn(time.substring(0, ix).trim()))
      : Opt.eget(Dt.fromIso(time.substring(0, ix).trim()))
    ;
  }
  var time: String;
  var msg: String;

  /// Constuctor
  ///   isError: If 'msg' is an error message.
  ///   time   : Time of message in format 'DD/MM/YYYY(HH:MM:SS)' or
  ///            "MM-DD-YYYY(HH:MM:SS)'.
  ///   msg    : Message
  public function new (isError: Bool, time: String, msg: String) {
    this.isError = isError;
    this.time = time;
    this.msg = msg;
  }

  /// Returns a formatted entry.
  ///   lineWidth: Width of line.
  public function format (lineWidth: Int): String {
    final indent = time.length + 3;
    final len = lineWidth - indent;
    final sep = isError ? " = " : " - ";
    return time + sep + format2(msg, indent, len);
  }

  public static function fromJs (js: Js): LogRow {
    final a = js.ra();
    return new LogRow(a[0].rb(), a[1].rs(), a[2].rs());
  }

  static function format2 (m: String, indent: Int, len: Int): String {
    if (m.trim() == "") return m;

    final r = [];
    Lambda.iter(m.split("\n"), l -> {
      final subr = [];

      while (l.length > len) {
        var line = l.substring(0, len);
        l = l.substring(len);
        final ix = line.lastIndexOf(" ");
        if (ix != -1 && line.substring(0, ix).trim() != "") {
          l = line.substring(ix + 1) + l;
          line = line.substring(0, ix);
        }
        subr.push(line);
      }

      if (l.trim() != "") subr.push(l);
      Lambda.iter(subr, subl -> r.push(subl));
    });

    var ind = "";
    for (i in 0...indent) ind += " ";
    return r.join("\n" + ind);
  }
}
