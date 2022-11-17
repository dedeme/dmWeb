// Copyright 07-Jun-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package dm;

import dm.Opt;

/// Line chart.
/// LineChart atributes:
///   - exArea
///     -atts
///       - background: String
///       - border
///         - color:  String
///         - dotted: Bool
///         - width: Int
///     - height: Int
///     - width: Int
///   - inPadding
///     - top: Int
///     - right: Int
///     - bottom: Int
///     - left: Int
///   - inAtts
///     - background: String
///     - border
///       - color:  String
///       - dotted: Bool
///       - width: Int
///   - chartPadding
///     - top: Int
///     - right: Int
///     - bottom: Int
///     - left: Int
///   - xAxis
///     - fontColor: String
///     - fontSize: Int
///     - isMonospace: Bool
///     - isItalic: Bool
///     - isBold: Bool
///     - grid
///       - color:  String
///       - dotted: Bool
///       - width: Int
///   - yAxis
///     - fontColor: String
///     - fontSize: Int
///     - isMonospace: Bool
///     - isItalic: Bool
///     - isBold: Bool
///     - grid
///       - color:  String
///       - dotted: Bool
///       - width: Int
///     - parts: Int
///   -lang: String
///   -data
///     - labels: Array<String>
///     - setLines: Array<LineChartSetLine>
///       · line
///         - color:  String
///         - dotted: Bool
///         - width: Int
///       · value: Float
///     - sets: Array<Array<Option<Float>>>
///     - setAtts: Array<LineChartLine>
///       · line
///         - color:  String
///         - dotted: Bool
///         - width: Int
///     - round: Int
///     - maxMinRound: Int
///     - drawGrid: (String, Int) -> Bool
///     - drawLabel: (String, Int) -> Bool
class LineChart {
  /// External area.
  public var exArea: LineChartArea;
  /// Padding between the beginning of external area and the beginning of internal area.
  public var inPadding: LineChartPadding;
  /// Attrubutes of internal area.
  public var inAtts: LineChartAreaAtts;
  /// Padding between the beginning of internal area and chart plotting.
  public var chartPadding: LineChartPadding;
  /// X axis attributes.
  public var xAxis: LineChartX;
  /// Y axis attributes.
  public var yAxis: LineChartY;
  /// Valid values are "es" and "en".
  /// Default: "es".
  public var lang: String;
  /// Chart data (not serializable)
  public var data: LineChartData;

  public function new (
    exArea: LineChartArea,
    inPadding: LineChartPadding,
    inAtts: LineChartAreaAtts,
    chartPadding: LineChartPadding,
    xAxis: LineChartX,
    yAxis: LineChartY,
    lang: String
  ) {
    this.exArea = exArea;
    this.inPadding = inPadding;
    this.inAtts = inAtts;
    this.chartPadding = chartPadding;
    this.xAxis = xAxis;
    this.yAxis = yAxis;
    this.lang = lang;

    data = LineChartData.mk();
  }

  public function mkWg (): Domo {
    function decFmt (n: Float) {
      if (lang == "en") return Dec.toEn(n, data.round);
      return Dec.toIso(n, data.round);
    }

    final hotLabels: Array<Float> = [];
    final hotSetlines: Array<Float> = [];
    final hotSets: Array<Array<Option<Float>>> = [];

    var max: Option<Float> = None;
    var min = 0.0;
    var gap = 0.0;

    // Set max, min and gap

    for (s in data.sets) for (val in s) {
      switch (val) {
        case Some(v):
          switch(max){
            case Some(m):
              max = Some(v > m ? v : m);
              min = v < min ? v : min;
            case None:
              max = Some(v);
              min = v;
          }
        case None:
      }
    }
    for (s in data.setLines) {
      switch(max){
        case Some(m):
          max = Some(s.value > m ? s.value : m);
          min = s.value < min ? s.value : min;
        case None:
          max = Some(s.value);
          min = s.value;
      }
    }
    final round = Math.pow(10, data.maxMinRound);
    switch (max) {
      case Some(m):
        final maxVal = (Math.round(m / round) + 1) * round;
        max = Some(maxVal);
        min = (Math.round(min / round) - 1) * round;
        gap = maxVal - min;
      case None:
    }

    // Set chart dimensions

    final w = exArea.width -
      inPadding.left - inPadding.right -
      chartPadding.left - chartPadding.right
    ;
    final h = exArea.height -
      inPadding.top - inPadding.bottom -
      chartPadding.top - chartPadding.bottom
    ;
    final x0 = inPadding.left + chartPadding.left;
    final xEnd = x0 + w;
    final y0 = exArea.height - inPadding.bottom - chartPadding.bottom;
    final yEnd = y0 - h;

    // Start plot

    final wg = Ui.Q("div");

    final cv = Ui.Q("canvas")
      .att("width", exArea.width)
      .att("height", exArea.height)
      .style("background:" + exArea.atts.background)
    ;
    final cv2 = Ui.Q("canvas")
      .att("width", 0)
      .att("height", 0)
      .style(
          "border: 1px solid black;" +
          "background:" + inAtts.background + ";" +
          "position: absolute;" +
          "visibility: hidden;"
        )
    ;
    cv2.on(MOUSEMOVE, (ev) ->
      if (
        ev.offsetX < cv2.getAtt("width") - 6 ||
        ev.offsetY < cv2.getAtt("height") - 6
      ) {
        cv2.setStyle("visibility", "hidden");
      }
    );


    final ctx = cast(cv.e, js.html.CanvasElement).getContext2d();

    // External area.

    if (exArea.atts.border.width > 0) {
      ctx.setLineDash(exArea.atts.border.dotted ? [4, 2] : []);
      ctx.lineWidth = exArea.atts.border.width;
      ctx.strokeStyle = exArea.atts.border.color;
      ctx.beginPath();
      ctx.rect(
        corr(0),
        corr(0),
        Math.round(exArea.width - 1),
        Math.round(exArea.height - 1)
      );
      ctx.stroke();
    }

    // Internal area.

    final ilf = inPadding.left;
    final itop = inPadding.top;
    final iw = exArea.width - inPadding.left - inPadding.right - 1;
    final ih = exArea.height - inPadding.top - inPadding.bottom - 1;

    ctx.fillStyle = inAtts.background;
    ctx.beginPath();
    ctx.rect(ilf, itop, iw, ih);
    ctx.fill();

    // Draw x

    ctx.fillStyle = xAxis.fontColor;
    ctx.font = "" + xAxis.fontSize + "px " +
      (xAxis.isMonospace ? "monospace" : "sans") +
      (xAxis.isItalic ? " italic" : "") +
      (xAxis.isBold ? " bold" : "")
    ;
    for (i in 0...data.labels.length) {
      final l = data.labels[i];

      if (!data.drawLabel(l, i)) continue;

      final lw = ctx.measureText(l).width;
      ctx.fillText(l,
        x0 + i * w / (data.labels.length - 1) - lw / 2,
        y0 + chartPadding.bottom + xAxis.fontSize
      );
    }

    for (i in 0...data.labels.length) {
      final l = data.labels[i];
      final cx = corr(x0 + i * w / (data.labels.length - 1));

      hotLabels.push(cx);

      if (i == 0 || i >= data.labels.length || !data.drawGrid(l, i)) continue;

      ctx.setLineDash(xAxis.grid.dotted ? [4, 2] : []);
      ctx.lineWidth = xAxis.grid.width;
      ctx.strokeStyle = xAxis.grid.color;
      ctx.beginPath();
      ctx.moveTo(cx, corr(y0));
      ctx.lineTo(cx, corr(yEnd));
      ctx.stroke();

    }

    // Draw y

    ctx.fillStyle = yAxis.fontColor;
    ctx.font = "" + yAxis.fontSize + "px " +
      (yAxis.isMonospace ? "monospace" : "sans") +
      (yAxis.isItalic ? " italic" : "") +
      (yAxis.isBold ? " bold" : "")
    ;

    var parts = yAxis.parts;
    if (parts < 1) parts = 1;
    for (i in 0...parts+1) {
      final yVal = min + i * gap / parts;
      final y = y0 - (yVal - min) * h / gap;

      final n = decFmt(yVal);
      final ms = ctx.measureText(n);
      ctx.fillText(
        n,
        inPadding.left - 4 - ms.width,
        y + yAxis.fontSize / 2.5
      );

      if (i == 0 || i == parts) continue;

      ctx.setLineDash(yAxis.grid.dotted ? [4, 2] : []);
      ctx.lineWidth = yAxis.grid.width;
      ctx.strokeStyle = yAxis.grid.color;
      ctx.beginPath();
      ctx.moveTo(corr(x0), corr(y));
      ctx.lineTo(corr(xEnd), corr(y));
      ctx.stroke();
    }

    // Draw lines data sets.
    for (dl in data.setLines) {
      final cy = y0 - (dl.value - min) * h / gap;
      hotSetlines.push(corr(cy));

      ctx.setLineDash(dl.line.dotted ? [4, 2] : []);
      ctx.lineWidth = dl.line.width;
      ctx.strokeStyle = dl.line.color;
      ctx.beginPath();
      ctx.moveTo(corr(x0), corr(cy));
      ctx.lineTo(corr(xEnd), corr(cy));
      ctx.stroke();
    }

    // Draw data sets

    switch (max) {
      case Some(mx):
        for (i in 0...data.sets.length) {
          final s = data.sets[i];
          final hotSetRow: Array<Option<Float>> = [];

          var cy0 = 0.0;
          var ixStart = 0;
          for (j in 0...s.length) {
            final sval = Opt.get(s[j]);
            if (sval == null) {
              hotSetRow.push(None);
              continue;
            }
            ixStart = j + 1;
            cy0 = corr(y0 - (sval - min) * h / gap);
            hotSetRow.push(Some(cy0));
            break;
          }

          ctx.setLineDash(data.setAtts[i].dotted ? [4, 2] : []);
          ctx.lineWidth = data.setAtts[i].width;
          ctx.strokeStyle = data.setAtts[i].color;
          ctx.beginPath();
          ctx.moveTo(corr(x0 + (ixStart - 1) * w / (s.length - 1)), cy0);
          var j = ixStart;
          while (j < s.length) {
            switch (s[j]) {
              case Some(v):
                final cy = corr(y0 - (v - min) * h / gap);
                hotSetRow.push(Some(cy));
                ctx.lineTo(corr(x0 + j * w / (s.length - 1)), cy);
                ++j;
              case None:
                hotSetRow.push(None);
                ++j;
                while (j < s.length) {
                  switch (s[j]) {
                    case Some(v):
                      final cy = corr(y0 - (v - min) * h / gap);
                      hotSetRow.push(Some(cy));
                      ctx.moveTo(corr(x0 + j * w / (s.length - 1)), cy);
                      ++j;
                      break;
                    case None:
                      hotSetRow.push(None);
                      ++j;
                  }
                }
            }
          }
          ctx.stroke();

          hotSets.push(hotSetRow);
        }
      case None:
    }

    // Draw internal frame

    if (inAtts.border.width > 0) {
      ctx.setLineDash(inAtts.border.dotted ? [4, 2] : []);
      ctx.lineWidth = inAtts.border.width;
      ctx.strokeStyle = inAtts.border.color;
      ctx.beginPath();
      ctx.rect(corr(ilf), corr(itop), Math.round(iw), Math.round(ih));
      ctx.stroke();
    }

    // Movement

    cv.on(MOUSEMOVE, ev -> {
      final cx = ev.offsetX;
      final cy = ev.offsetY;

      final x = (cx - x0) * data.labels.length / w;
      final y = min + (y0 - cy) * gap / h;

      var setlineIx = -1;
      var setlineDif = 0.0;
      for (i in 0...hotSetlines.length) {
        final v = hotSetlines[i];
        final dif = Math.abs(v - cy);
        if (dif < 4 && (setlineIx == -1 || dif < setlineDif)) {
          setlineIx = i;
          setlineDif = dif;
        }
      }

      var setIx = -1;
      var setValIx = -1;
      var setDif = 0.0;
      for (i in 0...data.sets.length) {
        final vs = data.sets[i];
        for (j in 0...vs.length) {
          final hotSet = Opt.get(hotSets[i][j]);
          if (hotSet == null) continue;

          final xdif = hotLabels[j] - cx;
          final ydif = hotSet - cy;
          final dif = Math.sqrt(xdif * xdif + ydif * ydif);

          if (dif < 4 && (setIx == -1 || dif <= setDif)) {
            setIx = i;
            setValIx = j;
            setDif = dif;
          }
        }
      }

      if (setlineIx != -1 || setIx != -1) {
        final yfirst = yAxis.fontSize;
        final ysecond = yfirst * 2 + yfirst / 4;
        final ysize = yfirst * 2.75;

        var tx1: String;
        var tx2: String;
        var color: String;

        if (setIx != -1) {
          tx1 = data.labels[setValIx];
          tx2 = decFmt(Opt.get(data.sets[setIx][setValIx]));
          color = data.setAtts[setIx].color;
        } else {
          tx1 = "Line";
          tx2 = decFmt(data.setLines[setlineIx].value);
          color = data.setLines[setlineIx].line.color;
        }

        var ctx2 = cast(cv2.e, js.html.CanvasElement).getContext2d();
        ctx2.font = "" + yAxis.fontSize + "px " +
          (yAxis.isMonospace ? "monospace" : "sans") +
          (yAxis.isItalic ? " italic" : "") +
          (yAxis.isBold ? " bold" : "")
        ;
        final ms1 = ctx2.measureText(tx1).width;
        final ms2 = ctx2.measureText(tx2).width;

        var margin1 = 4.0;
        var margin2 = Math.abs(ms1 - ms2) / 2 + margin1;
        var ms = ms1 + margin1 * 2;
        if (ms2 > ms1) {
          margin1 = margin2;
          margin2 = 4;
          ms = ms2 + margin2 * 2;
        }

        cv2
          .att("height", ysize)
          .att("width", ms)
          .setStyle("top", "" + (Ui.mouseY(ev) - ysize) + "px")
          .setStyle("left", "" + (Ui.mouseX(ev) - ms) + "px")
          .setStyle("visibility", "visible")
        ;

        ctx2 = cast(cv2.e, js.html.CanvasElement).getContext2d();
        ctx2.fillStyle = color;
        ctx2.font = "" + yAxis.fontSize + "px " +
          (yAxis.isMonospace ? "monospace" : "sans") +
          (yAxis.isItalic ? " italic" : "") +
          (yAxis.isBold ? " bold" : "")
        ;
        ctx2.fillText(tx1, margin1, yfirst);
        ctx2.fillText(tx2, margin2, ysecond);
      } else {
        cv2.setStyle("visibility", "hidden");
      }
    });

    return wg.add(cv).add(cv2);
  }

  /// Copies every attribute but 'labels' and 'datasets'.
  public function copy (): LineChart {
    return new LineChart(
      exArea, inPadding, inAtts, chartPadding, xAxis, yAxis, lang
    );
  }

  /// Serializes every attribute but 'labels' and 'datasets'.
  public function toJs (): Js {
    return Js.wa([
      exArea.toJs(),
      inPadding.toJs(),
      inAtts.toJs(),
      chartPadding.toJs(),
      xAxis.toJs(),
      yAxis.toJs(),
      Js.ws(lang)
    ]);
  }

  /// Restores every attribute but 'labels' and 'datasets' which are set to '[]'.
  public static function fromJs (js: Js): LineChart {
    final a = js.ra();
    return new LineChart(
      LineChartArea.fromJs(a[0]),
      LineChartPadding.fromJs(a[1]),
      LineChartAreaAtts.fromJs(a[2]),
      LineChartPadding.fromJs(a[3]),
      LineChartX.fromJs(a[4]),
      LineChartY.fromJs(a[5]),
      a[6].rs()
    );
  }

  public static function mk (): LineChart {
    final atts = LineChartAreaAtts.mk();
    atts.background = "#e9eaec";
    return new LineChart(
      LineChartArea.mk(),
      LineChartPadding.mk(),
      atts,
      new LineChartPadding(2, 1, 2, 1),
      LineChartX.mk(),
      LineChartY.mk(),
      "es"
    );
  }

  public static function corr (x: Float): Float {
    return Math.floor(x) + 0.5;
  }
}

/// Playground to draw padding.
class LineChartPadding {
  /// Top distance.
  public var top: Int;
  /// Right distance.
  public var right: Int;
  /// Bottom distance.
  public var bottom: Int;
  /// Left distance.
  public var left: Int;

  public function new (top: Int, right: Int, bottom: Int, left: Int) {
    this.top = top;
    this.right = right;
    this.bottom = bottom;
    this.left = left;
  }

  public function copy (): LineChartPadding {
    return new LineChartPadding(top, right, bottom, left);
  }

  public function toJs (): Js {
    return Js.wa([
      Js.wi(top),
      Js.wi(right),
      Js.wi(bottom),
      Js.wi(left),
    ]);
  }

  public static function fromJs (js: Js): LineChartPadding {
    final a = js.ra();
    return new LineChartPadding(
      a[0].ri(),
      a[1].ri(),
      a[2].ri(),
      a[3].ri()
    );
  }

  public static function mk () {
    return new LineChartPadding(8, 10, 20, 60);
  }
}

/// Line properties.
class LineChartLine {
  /// Line width.
  public var width: Int;
  /// Line color.
  public var color: String;
  /// 'true' if line is dotted
  public var dotted: Bool;

  public function new (width: Int, color: String, dotted: Bool) {
    this.width = width;
    this.color = color;
    this.dotted = dotted;
  }

  public function copy (): LineChartLine {
    return new LineChartLine(width, color, dotted);
  }

  public function toJs (): Js {
    return Js.wa([
      Js.wi(width),
      Js.ws(color),
      Js.wb(dotted)
    ]);
  }

  public static function fromJs (js: Js): LineChartLine {
    final a = js.ra();
    return new LineChartLine(
      a[0].ri(),
      a[1].rs(),
      a[2].rb()
    );
  }

  public static function mk (): LineChartLine {
    return new LineChartLine(1, "#002040", false);
  }
}

/// Attributtes of a chart area (extern or intern).
class LineChartAreaAtts {
  /// Border line.
  public var border: LineChartLine;
  /// Area background.
  public var background: String;

  public function new (border: LineChartLine, background: String) {
    this.border = border;
    this.background = background;
  }

  public function copy (): LineChartAreaAtts {
    return new LineChartAreaAtts(border.copy(), background);
  }

  public function toJs (): Js {
    return Js.wa([
      border.toJs(),
      Js.ws(background)
    ]);
  }

  public static function fromJs (js: Js): LineChartAreaAtts {
    final a = js.ra();
    return new LineChartAreaAtts(
      LineChartLine.fromJs(a[0]),
      a[1].rs()
    );
  }

  public static function mk (): LineChartAreaAtts {
    return new LineChartAreaAtts(LineChartLine.mk(), "#fbfdff");
  }
}

/// External area attibutes.
class LineChartArea {
  public var width: Int;
  public var height: Int;
  public var atts: LineChartAreaAtts;

  public function new (width: Int, height: Int, atts: LineChartAreaAtts) {
    this.width = width;
    this.height = height;
    this.atts = atts;
  }

  public function copy (): LineChartArea {
    return new LineChartArea(width, height, atts.copy());
  }

  public function toJs (): Js {
    return Js.wa([
      Js.wi(width),
      Js.wi(height),
      atts.toJs()
    ]);
  }

  public static function fromJs (js: Js): LineChartArea {
    final a = js.ra();
    return new LineChartArea(
      a[0].ri(),
      a[1].ri(),
      LineChartAreaAtts.fromJs(a[4])
    );
  }

  public static function mk (): LineChartArea {
    return new LineChartArea(400, 200, LineChartAreaAtts.mk());
  }
}

/// Fixed data set value.
class LineChartSetLine {
  public var value: Float;
  public var line: LineChartLine;

  public function new (value: Float, line: LineChartLine) {
    this.value = value;
    this.line = line;
  }

  public function mk (): LineChartSetLine {
    return new LineChartSetLine(0.0, LineChartLine.mk());
  }
}

/// Labels and datasets.
/// This data is not serializable.
class LineChartData {
  /// X axis labels.
  public final labels: Array<String>;
  /// Y data series.
  public final sets: Array<Array<Option<Float>>>;
  /// Y data series attibutes.
  public final setAtts: Array<LineChartLine>;
  /// Fixed data set values (empty by default).
  public var setLines: Array<LineChartSetLine>;
  /// Round of values to show. Beetwen 0 and 9, both inclusive:
  /// Default 0.
  public var round: Int;
  /// Round of maximum - minimum. Beetwen -9 and 9, both inclusive:
  ///   ..., -2 -> 0.001, -1 -> 0.01, 0 -> 0, 1 -> 10, 2 -> 100 ...
  /// Default 0.
  public var maxMinRound: Int;
  /// Function to draw labels.
  /// Default: '(l, i) -> true".
  ///   label (String): Label to decide.
  ///   index (Int): Index of the label.
  /// Examples:
  ///   - To draw al lines:
  ///       drawLabel = (l, i) -> true;
  ///   - To draw one of each 5 lines:
  ///       drawLabel = (l, i) -> i % 5 == 0 ? true: false;
  ///   - To draw some labels:
  ///       drawLabel = (l, i) -> l.startsWith("-") ? true : false;
  public var drawLabel: (String, Int) -> Bool;
  /// Function to draw grid for labels.
  /// Default: '(l, i) -> false".
  ///   label (String): Label to decide.
  ///   index (Int): Index of the label.
  /// Examples:
  ///   - To draw al lines:
  ///       drawLine = (l, i) -> true;
  ///   - To draw one of each 5 lines:
  ///       drawLine = (l, i) -> i % 5 == 0 ? true: false;
  ///   - To draw some labels:
  ///       drawLine = (l, i) -> l.startsWith("-") ? true : false;
  public var drawGrid: (String, Int) -> Bool;

  public function new (
    labels: Array<String>,
    sets: Array<Array<Option<Float>>>,
    setAtts: Array<LineChartLine>
  ) {
    if (labels.length == 0)
      throw new haxe.Exception("'labels' does not have values");
    if (sets.length == 0)
      throw new haxe.Exception("'sets' does not have values");

    if (labels.length != sets[0].length)
      throw new haxe.Exception(
        "Number of labels (" + labels.length +
        ") does not match number of sets values (" + sets[0].length + ")"
      );

    if (sets.length != setAtts.length)
      throw new haxe.Exception(
        "Number of sets (" + sets.length +
        ") does not match number of sets Attributes (" + setAtts.length + ")"
      );

    this.labels = labels;
    this.sets = sets;
    this.setAtts = setAtts;
    setLines = [];
    drawLabel = (l, i) -> true;
    drawGrid = (l, i) -> true;
    round = 2;
    maxMinRound = 0;
  }

  public static function mk () {
    final labels = ["Mon", "Tue", "Wen", "Thu", "Fri", "Sat", "Sun"];
    final sets = [
      [1.0, 2.0, 9.54, 10.2, 6.2, -7, 7].map(e -> Some(e)),
      [2, -4, -2.15, -5.2, 7, 3, 4].map(e -> Some(e))
    ];
    final setAtts = [LineChartLine.mk(),LineChartLine.mk()];
    setAtts[0].color = "#000080";
    setAtts[1].color = "#800000";

    return new LineChartData(labels, sets, setAtts);
  }
}

/// Properties of X axis (labels).
class LineChartX {
  /// Font size.
  public var fontSize: Int;
  /// 'true' if font is 'monospace'. Otherswise it is 'sans'.
  public var isMonospace: Bool;
  /// 'true' if font is italic.
  public var isItalic: Bool;
  /// 'true' if font is bold.
  public var isBold: Bool;
  /// Font color.
  public var fontColor: String;
  /// Attributes of grid lines.
  public var grid: LineChartLine;

  public function new (
    fontSize: Int, isMonospace: Bool, isItalic: Bool, isBold: Bool,
    fontColor: String, grid: LineChartLine
  ) {
    this.fontSize = fontSize;
    this.isMonospace = isMonospace;
    this.isItalic = isItalic;
    this.isBold = isBold;
    this.fontColor = fontColor;
    this.grid = grid;
  }

  public function copy (): LineChartX {
    return new LineChartX(
      fontSize, isMonospace, isItalic, isBold, fontColor, grid
    );
  }

  public function toJs (): Js {
    return Js.wa([
      Js.wi(fontSize),
      Js.wb(isMonospace),
      Js.wb(isItalic),
      Js.wb(isBold),
      Js.ws(fontColor),
      grid.toJs()
    ]);
  }

  public static function fromJs (js: Js): LineChartX {
    final a = js.ra();
    return new LineChartX(
      a[0].ri(),
      a[1].rb(),
      a[2].rb(),
      a[3].rb(),
      a[4].rs(),
      LineChartLine.fromJs(a[5])
    );
  }

  public static function mk (): LineChartX {
    return new LineChartX(
      12, false, false, false, "#000000", new LineChartLine(1, "#808080", true)
    );
  }
}

/// Properties of Y axis (values).
class LineChartY {
  /// Font size.
  public var fontSize: Int;
  /// 'true' if font is 'monospace'. Otherswise it is 'sans'.
  public var isMonospace: Bool;
  /// 'true' if font is italic.
  public var isItalic: Bool;
  /// 'true' if font is bold.
  public var isBold: Bool;
  /// Font color.
  public var fontColor: String;
  /// Attributes of grid lines.
  public var grid: LineChartLine;
  /// Parts to divide the chart horizontally (minimum 1).
  public var parts: Int;

  public function new (
    fontSize: Int, isMonospace: Bool, isItalic: Bool, isBold: Bool,
    fontColor: String, grid: LineChartLine, parts: Int
  ) {
    this.fontSize = fontSize;
    this.isMonospace = isMonospace;
    this.isItalic = isItalic;
    this.isBold = isBold;
    this.fontColor = fontColor;
    this.grid = grid;
    this.parts = parts;
  }

  public function copy (): LineChartY {
    return new LineChartY(
      fontSize, isMonospace, isItalic, isBold, fontColor, grid, parts
    );
  }

  public function toJs (): Js {
    return Js.wa([
      Js.wi(fontSize),
      Js.wb(isMonospace),
      Js.wb(isItalic),
      Js.wb(isBold),
      Js.ws(fontColor),
      grid.toJs(),
      Js.wi(parts)
    ]);
  }

  public static function fromJs (js: Js): LineChartY {
    final a = js.ra();
    return new LineChartY(
      a[0].ri(),
      a[1].rb(),
      a[2].rb(),
      a[3].rb(),
      a[4].rs(),
      LineChartLine.fromJs(a[5]),
      a[6].ri()
    );
  }

  public static function mk (): LineChartY {
    return new LineChartY(
      12, false, false, false, "#000000",
      new LineChartLine(1, "#808080", true), 4
    );
  }
}
