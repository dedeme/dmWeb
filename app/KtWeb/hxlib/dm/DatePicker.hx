// Copyright 16-Jun-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package dm;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;

/// DatePicker widget.
class DatePicker {
  /// DatePicker language ("en" or "es"). Default "es".
  /// It must be set before calling to 'mk' functions.
  public var lang = "es";

  /// Current date. Default today.
  /// It must be set before calling to 'mk' functions.
  var dateV = Date.now();
  public var date(get, set): Date;
  function get_date() return dateV;
  function set_date(d) {
    if (d != null) dateView = Dt.mk(1, Dt.month(d), Dt.year(d));
    dateV = d;
    return d;
  }

  /// Action to do when DatePicker is clicked.
  ///   - If 'none' is clicked an empty string is sent to 'action'.
  ///   - If a day is clicked a date in format 'YYYYMMDD' is sent to 'action'.
  /// It must be set before calling to 'mk' functions.
  public var action = s -> Ui.alert('"$s" was clicked');

  // First day of current month.
  var dateView: Date;

  // If DatePicker is style floating.
  var floating = false;

  // 'span' to show the calendar month.
  var elMonth = Q("span");

  // 'span' to show the calendar year.
  var elYear = Q("span");

  // Array<Array<td>> to show the calendar days.
  var elDays: Array<Array<Domo>> = [];

  // 'tr' 6th. row of calendar.
  var exTr = Q("tr");

  // 'tr' Last row of calendar
  var tr4 = Q("tr");

  // 'table' Table of days.
  var tb = Q("table");

  function months () {
    return lang == "en"
      ? ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul",
        "Aug", "Sep", "Oct", "Nov", "Dec"]
      : ["ene", "feb", "mar", "abr", "may", "jun", "jul",
        "ago", "sep", "oct", "nov", "dic"];
  }

  function weekDays () {
    return this.lang == "en"
      ? ["Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"]
      : ["dom", "lun", "mar", "mié", "jue", "vie", "sáb"];
  }

  function i18n () {
    return this.lang == "en"
      ? {firstWeekDay: 0, today: "Today", none: "None"}
      : {firstWeekDay: 1, today: "Hoy", none: "Nada"};
  }

  function previousMonth () {
    dateView = Dt.mk(1, Dt.month(dateView) - 1, Dt.year(dateView));
    load();
  }

  function nextMonth () {
    dateView = Dt.mk(1, Dt.month(dateView) + 1, Dt.year(dateView));
    load();
  }

  function previousYear () {
    dateView = Dt.mk(1, Dt.month(dateView), Dt.year(dateView) - 1);
    load();
  }

  function nextYear () {
    dateView = Dt.mk(1, Dt.month(dateView), Dt.year(dateView) + 1);
    load();
  }

  function today () {
    final today = Date.now();
    dateView = Dt.mk(1, Dt.month(today), Dt.year(today));
    load();
  }

  // When 'none' is clicked an empty string is sent to 'action'.
  function none () {
    date = null;
    load();
    action("");
  }

  // When a date is clicked it is sent to 'action' in format 'YYYYMMDD'.
  function clickDay (d) {
    date = d;
    load();
    action(Dt.to(date));
  }

  // Reload the DataPicker.
  function load () {
    elMonth.html(months()[Dt.month(dateView) - 1]);
    elYear.html(Std.string(Dt.year(dateView)));

    var ix = Dt.weekDay(dateView) - i18n().firstWeekDay;
    ix = ix < 0 ? 7 + ix : ix;
    final month = Dt.month(dateView);
    var date1 = Dt.mk(Dt.day(dateView) - ix, month, Dt.year(dateView));

    final today = Date.now();
    final tyear = Dt.year(today);
    final tmonth = Dt.month(today);
    final tday = Dt.day(today);

    var dyear = tyear;
    var dmonth = tmonth;
    var dday = tday;

    if (date != null) {
      dyear = Dt.year(date);
      dmonth = Dt.month(date);
      dday = Dt.day(date);
    }

    var extraRow = false;
    It.range(6).each(i -> {
      if (i == 5 && Dt.month(date1) == month) extraRow = true;
      It.range(7).each(j -> {
        final d = elDays[i][j].removeAll();
        final year1 = Dt.year(date1);
        final month1 = Dt.month(date1);
        final day1 = Dt.day(date1);

        if (day1 == dday && month1 == dmonth && year1 == dyear) {
          d.klass("select");
        } else {
          d.klass("day");
          if (Dt.month(date1) != month) d.klass("dayOut");
          if (Dt.weekDay(date1) == 6 || Dt.weekDay(date1) == 0) {
            d.klass("weekend");
            if (Dt.month(date1) != month) d.klass("weekendOut");
          }
        }
        if (day1 == tday && month1 == tmonth && year1 == tyear)
          d.klass("today");

        final ddate1 = date1;
        d.html("<span class='day'>" + Dt.day(ddate1) + "</span>");
        d.e.onclick = e -> clickDay(ddate1);

        date1 = Dt.mk(Dt.day(date1) + 1, Dt.month(date1), Dt.year(date1));
      });
    });

    if (tb.getAtt("hasTrEx") == "true") {
      tb.e.removeChild(exTr.e);
      tb.att("hasTrEx", "false");
    }

    if (extraRow) {
      tb.e.removeChild(tr4.e);

      tb.e.appendChild(exTr.e);
      tb.e.appendChild(tr4.e);
      tb.att("hasTrEx", "true");
    }
}

  /// Constructor.
  /// The process to construct a DatePicker is:
  ///   1. Call new DatePicker.
  ///   2. Set parameters lang, date and action if it is need.
  ///   3. Call one of 'mk' functions.
  /// See examples in 'mk' functions.
  public function new () {
    dateView = Dt.mk(1, Dt.month(date), Dt.year(date));
  }

  /// DatePicker widget.
  public function mk (): Domo {
    final mkArrow = (tx: String, f: Void -> Void) ->
      Q("td")
        .klass("arrow")
        .add(Q("span")
          .html(tx)
          .on(CLICK, e -> f()));

    final mkHeader = (colspan, txarr1, farr1, element, txarr2, farr2) ->
      Q("td")
        .att("colspan", colspan)
        .add(Q("table")
          .klass("in")
          .add(Q("tr")
            .add(mkArrow(txarr1, farr1))
            .add(Q("td")
              .style("vertical-align:bottom")
              .add(element.klass("title")))
            .add(mkArrow(txarr2, farr2))));

    elMonth = Q("span");
    elYear = Q("span");
    elDays = [];

    tr4 = Q("tr")
      .add(Q("td")
        .att("colspan", 4)
        .klass("left")
        .add(Q("span").klass("link")
          .html(i18n().today)
          .on(CLICK, e -> today())))
      .add(Q("td")
        .att("colspan", 3)
        .klass("right")
        .add(Q("span")
          .klass("link")
          .html(i18n().none)
          .on(CLICK, e -> none())));

    tb = Q("table")
      .att("hasTrEx", "false")
      .klass("dmDatePicker")
      .add(Q("tr")
        .add(mkHeader(
          3, "&laquo",
          () -> previousMonth(),
          elMonth,
          "&raquo;",
          () -> nextMonth()
        ))
        .add(Q("td"))
        .add(mkHeader(
          3, "&laquo",
          () -> previousYear(),
          elYear,
          "&raquo;",
          () -> nextYear()
        )))
      .add(Q("tr")
        .adds(It.range(7).map(i -> {
          var ix = i + i18n().firstWeekDay;
          ix = ix > 6 ? ix - 7 : ix;
          return Q("td")
            .html(weekDays()[ix])
          ;
        }).to()))
      .adds((() -> {
        final rows = It.range(5).map(_ -> {
          final tds = [];
          final tr = Q("tr")
            .adds(It.range(7).map(_ -> {
              final td = Q("td");
              tds.push(td);
              return td;
            }).to())
          ;
          elDays.push(tds);
          return tr;
        }).to(); // Force to calculate 'map'
        final tds = [];
        exTr = Q("tr")
          .adds(It.range(7).map(_ -> {
            final td = Q("td");
            tds.push(td);
            return td;
          }).to())
        ;
        elDays.push(tds);
        return It.from(rows);
      })().to())
      .add(tr4);
    load();
    return Q("div")
      .style(floating ? "position:absolute" : "position:relative")
      .add(tb)
    ;
  }

  /// Makes a DatePicker which depends on a button.
  ///   button: A Button.
  public function mkButton (button: Domo): Domo {
    final span = Q("span");
    var isShow = false;

    final btAction = () -> {
      if (!isShow) {
        span.add(mk());
        isShow = true;
        return;
      }
      span.removeAll();
      isShow = false;
    };
    button.e.onclick = btAction;

    final previousAction = action;
    action = s -> {
      previousAction(s);
      span.removeAll();
      isShow = false;
    };

    floating = true;
    return Q("span")
      .add(button)
      .add(span)
    ;
  }

  /// Makes a DatePicker which depends on a text field.
  ///   textInput: A text input.
  public function mkText (textInput: Domo): Domo {
    final format = s -> {
      final d = dm.Opt.get(Dt.from(s));
      return lang == "en"
        ? DateTools.format(d, "%m/%d/%Y")
        : DateTools.format(d, "%d/%m/%Y")
      ;
    };
    final span = Q("span");
    var isShow = false;

    final btAction = () -> {
      if (!isShow) {
        span.add(mk());
        isShow = true;
        return;
      }
      span.removeAll();
      isShow = false;
    };
    textInput.value(format(Dt.to(date)));
    textInput.e.onclick = btAction;
    textInput.e.onkeydown = e -> e.preventDefault();

    final previousAction = action;
    action = s -> {
      textInput.value(s == "" ? "" : format(s));
      previousAction(s);
      span.removeAll();
      isShow = false;
    };

    floating = true;
    return Q("span")
      .add(textInput)
      .add(span)
    ;
  }

}
