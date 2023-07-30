import * as iter from '../../_js/iter.js';import * as str from '../../_js/str.js';import * as bytes from '../../_js/bytes.js';import * as cryp from '../../_js/cryp.js';import * as dic from '../../_js/dic.js';import * as timer from '../../_js/timer.js';import * as js from '../../_js/js.js';import * as storage from '../../_js/storage.js';import * as sys from '../../_js/sys.js';import * as math from '../../_js/math.js';import * as domo from '../../_js/domo.js';import * as ui from '../../_js/ui.js';import * as arr from '../../_js/arr.js';import * as time from '../../_js/time.js';import * as client from '../../_js/client.js';import * as b64 from '../../_js/b64.js';




import * as lineChart from  "../../libdm/lineChart.js";
import * as i18n from  "../../i18n.js";

const Q =sys.$checkNull( ui.q);
const II =sys.$checkNull( i18n.tlt);


export  function mk(Labels, Values)  {sys.$params(arguments.length, 2);
  const back =sys.$checkNull( "#fafafa");

  const Chart =sys.$checkNull( lineChart.mkExample());
  Chart.ExArea.width =sys.$checkExists(Chart.ExArea.width,sys.$checkNull( 610));
  Chart.ExArea.height =sys.$checkExists(Chart.ExArea.height,sys.$checkNull( 160));
  Chart.ExArea.Atts.Border.width =sys.$checkExists(Chart.ExArea.Atts.Border.width,sys.$checkNull( 0));
  Chart.ExArea.Atts.background =sys.$checkExists(Chart.ExArea.Atts.background,sys.$checkNull( back));
  Chart.InPadding.right =sys.$checkExists(Chart.InPadding.right,sys.$checkNull( 6));
  Chart.InPadding.bottom =sys.$checkExists(Chart.InPadding.bottom,sys.$checkNull( 12));
  Chart.InPadding.left =sys.$checkExists(Chart.InPadding.left,sys.$checkNull( 85));
  Chart.ChartPadding.top =sys.$checkExists(Chart.ChartPadding.top,sys.$checkNull( 4));
  Chart.ChartPadding.right =sys.$checkExists(Chart.ChartPadding.right,sys.$checkNull( 4));
  Chart.ChartPadding.bottom =sys.$checkExists(Chart.ChartPadding.bottom,sys.$checkNull( 4));
  Chart.ChartPadding.left =sys.$checkExists(Chart.ChartPadding.left,sys.$checkNull( 4));
  Chart.InAtts.background =sys.$checkExists(Chart.InAtts.background,sys.$checkNull( "#e9e9e9"));

  const Atts =sys.$checkNull( [
    lineChart.mkLine(1, "#000080", false),
    lineChart.mkLine(1, "#008000", false),
    lineChart.mkLine(1, "#800000", false)
  ]);

  const Data =sys.$checkNull( lineChart.mkData(Labels, Values, Atts));
  Data.UnarySets =sys.$checkExists(Data.UnarySets,sys.$checkNull( [lineChart.mkUnarySet(
    II("Dif. 0"), 0, lineChart.mkLine(1, "#000000", false)
  )]));
  Data.round =sys.$checkExists(Data.round,sys.$checkNull( 2));
  Data.drawGrid =sys.$checkExists(Data.drawGrid, function(lb, i)  {sys.$params(arguments.length, 2);
    if (sys.asBool(sys.$eq(i , 0)))  return false;
     return sys.$neq(Labels[i - 1] , lb);
  });
  Data.drawLabel =sys.$checkExists(Data.drawLabel,sys.$checkNull( Data.drawGrid));
  Data.maxMinRound =sys.$checkExists(Data.maxMinRound, function(x, n)  {sys.$params(arguments.length, 2);   return -2;});

   return Q("table")
    .att("align", "center")
    .add(Q("tr")
      .add(Q("td")
        .klass("frame0")
        .style("background-color:" + back)
        .add(lineChart.mkWg(Chart, Data))))
  ;
};
