import * as iter from '../../_js/iter.js';import * as str from '../../_js/str.js';import * as bytes from '../../_js/bytes.js';import * as cryp from '../../_js/cryp.js';import * as dic from '../../_js/dic.js';import * as timer from '../../_js/timer.js';import * as js from '../../_js/js.js';import * as storage from '../../_js/storage.js';import * as sys from '../../_js/sys.js';import * as math from '../../_js/math.js';import * as domo from '../../_js/domo.js';import * as ui from '../../_js/ui.js';import * as arr from '../../_js/arr.js';import * as time from '../../_js/time.js';import * as client from '../../_js/client.js';import * as b64 from '../../_js/b64.js';




import * as lineChart from  "../../libdm/lineChart.js";

const Q =sys.$checkNull( ui.q);


export  function mk(Labels, Values)  {sys.$params(arguments.length, 2);
  const Chart =sys.$checkNull( lineChart.mkExample());
  Chart.ExArea.width =sys.$checkExists(Chart.ExArea.width,sys.$checkNull( 610));
  Chart.ExArea.height =sys.$checkExists(Chart.ExArea.height,sys.$checkNull( 320));
  Chart.ExArea.Atts.Border.width =sys.$checkExists(Chart.ExArea.Atts.Border.width,sys.$checkNull( 0));
  Chart.InPadding.top =sys.$checkExists(Chart.InPadding.top,sys.$checkNull( 10));
  Chart.InPadding.right =sys.$checkExists(Chart.InPadding.right,sys.$checkNull( 10));
  Chart.InPadding.bottom =sys.$checkExists(Chart.InPadding.bottom,sys.$checkNull( 20));
  Chart.InPadding.left =sys.$checkExists(Chart.InPadding.left,sys.$checkNull( 85));
  Chart.ChartPadding.top =sys.$checkExists(Chart.ChartPadding.top,sys.$checkNull( 18));
  Chart.ChartPadding.right =sys.$checkExists(Chart.ChartPadding.right,sys.$checkNull( 4));
  Chart.ChartPadding.bottom =sys.$checkExists(Chart.ChartPadding.bottom,sys.$checkNull( 18));
  Chart.ChartPadding.left =sys.$checkExists(Chart.ChartPadding.left,sys.$checkNull( 4));

  const dif =sys.$checkNull( arr.peek(Values[0])[0] - Values[0][0]);
  const back =sys.$checkNull(sys.asBool( dif > 0)
    ? "#e9e9f2"
    :sys.asBool( dif < 0)
      ? "#f2e9e9"
      : "#e9e9e9")
    ;
  Chart.ExArea.Atts.background =sys.$checkExists(Chart.ExArea.Atts.background,sys.$checkNull( back));

  const Atts =sys.$checkNull( [
    lineChart.mkLine(1.2, "#414141", false)
  ]);
  const Data =sys.$checkNull( lineChart.mkData(Labels, Values, Atts));
  Data.round =sys.$checkExists(Data.round,sys.$checkNull( 0));
  Data.drawGrid =sys.$checkExists(Data.drawGrid, function(lb, i)  {sys.$params(arguments.length, 2);
    if (sys.asBool(sys.$eq(i , 0)))  return false;
     return sys.$neq(Labels[i - 1] , lb);
  });
  Data.drawLabel =sys.$checkExists(Data.drawLabel,sys.$checkNull( Data.drawGrid));

   return Q("table")
    .att("align", "center")
    .add(Q("tr")
      .add(Q("td")
        .klass("frame0")
        .style("background-color:" + back)
        .add(lineChart.mkWg(Chart, Data))))
  ;
};
