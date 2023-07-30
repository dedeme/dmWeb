import * as iter from '../_js/iter.js';import * as str from '../_js/str.js';import * as bytes from '../_js/bytes.js';import * as cryp from '../_js/cryp.js';import * as dic from '../_js/dic.js';import * as timer from '../_js/timer.js';import * as js from '../_js/js.js';import * as storage from '../_js/storage.js';import * as sys from '../_js/sys.js';import * as math from '../_js/math.js';import * as domo from '../_js/domo.js';import * as ui from '../_js/ui.js';import * as arr from '../_js/arr.js';import * as time from '../_js/time.js';import * as client from '../_js/client.js';import * as b64 from '../_js/b64.js';




import * as menu from  "../libdm/menu.js";
import * as cosPg from  "../pgs/charts/cosPg.js";
import * as historicPg from  "../pgs/charts/historicPg.js";
import * as operationsPg from  "../pgs/charts/operationsPg.js";
import * as i18n from  "../i18n.js";
import * as fns from  "../fns.js";

const Q =sys.$checkNull( ui.q);
const II =sys.$checkNull( i18n.tlt);


export  function mk(wg, modelId)  {sys.$params(arguments.length, 2);
  const Chart =sys.$checkNull( ["cos"]);
  const Show =sys.$checkNull( [[]]);



  
   function go(chart)  {sys.$params(arguments.length, 1);
    Chart[0] =sys.$checkExists(Chart[0],sys.$checkNull( chart));
    Show[0]();
  };



  
  Show[0] =sys.$checkExists(Show[0], function()  {sys.$params(arguments.length, 0);
    const Lopts =sys.$checkNull( [
      menu.toption("cos", II("Companies"), function()  {sys.$params(arguments.length, 0); go("cos");}),
      menu.separator(),
      menu.toption("historic", II("Historic"), function()  {sys.$params(arguments.length, 0); go("historic");}),
      menu.separator(),
      menu.toption("operations", II("Operations"), function()  {sys.$params(arguments.length, 0); go("operations");})
    ]);
    const menuWg =sys.$checkNull( menu.mk(Lopts, [], Chart[0], false));

    const body =sys.$checkNull( Q("div"));
    switch (Chart[0]) {
      case "historic":{ historicPg.mk(body, modelId);break;}
      case "operations":{ operationsPg.mk(body, modelId);break;}
      default:{ cosPg.mk(body, modelId);}
    }

    wg
      .removeAll()
      .add(menuWg)
      .add(body)
    ;
  });

  Show[0]();

};
