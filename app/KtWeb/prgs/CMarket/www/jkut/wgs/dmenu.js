import * as iter from '../_js/iter.js';import * as str from '../_js/str.js';import * as bytes from '../_js/bytes.js';import * as cryp from '../_js/cryp.js';import * as dic from '../_js/dic.js';import * as timer from '../_js/timer.js';import * as js from '../_js/js.js';import * as storage from '../_js/storage.js';import * as sys from '../_js/sys.js';import * as math from '../_js/math.js';import * as domo from '../_js/domo.js';import * as ui from '../_js/ui.js';import * as arr from '../_js/arr.js';import * as time from '../_js/time.js';import * as client from '../_js/client.js';import * as b64 from '../_js/b64.js';




import * as menu from  "../libdm/menu.js";
import * as i18n from  "../i18n.js";

const Q =sys.$checkNull( ui.q);
const II =sys.$checkNull( i18n.tlt);




 function mkUpMenu(selected)  {sys.$params(arguments.length, 1);
  const Lopts =sys.$checkNull( [
    menu.tlink("home", II("Home"), []),
    menu.separator(),
    menu.tlink("acc", II("Accounting"), []),
    menu.separator(),
    menu.tlink("daily", II("Daily Quotes"), [])
  ]);

  const Ropts =sys.$checkNull( [
    menu.tlink("settings", II("Annotations & Settings"), [])
  ]);

   return menu.mk(Lopts, Ropts, selected, false);
};






export  function mk(wg, selected)  {sys.$params(arguments.length, 2);
  const upDiv =sys.$checkNull( Q("div").style("padding:0px"));
  const upMenu =sys.$checkNull( mkUpMenu(selected));
  const downDiv =sys.$checkNull( Q("div"));
  const Hidden =sys.$checkNull( [false]);

  const Show =sys.$checkNull( [[]]);

  

  
   function change(ev)  {sys.$params(arguments.length, 1);
    Hidden[0] =sys.$checkExists(Hidden[0],sys.$checkNull( !sys.asBool(Hidden[0])));
    upDiv.removeAll().style("padding:0px");
    if (sys.asBool(!sys.asBool(Hidden[0]))) upDiv.add(upMenu);
  };

  
  
   function setDownMenu(menuWg)  {sys.$params(arguments.length, 1);
    downDiv
      .removeAll()
      .add(menuWg)
    ;
    Hidden[0] =sys.$checkExists(Hidden[0],sys.$checkNull( true));
    upDiv.removeAll().style("padding:0px");
  };

  

  
  Show[0] =sys.$checkExists(Show[0], function()  {sys.$params(arguments.length, 0);
    wg
      .removeAll()
      .add(upDiv
        .removeAll()
        .style("padding:0px")
        .add(upMenu))
      .add(downDiv)
    ;
  });

  Show[0]();

   return {setDownMenu:setDownMenu, change:change};
};



export  function setDownMenu(dbmenu, downMenu)  {sys.$params(arguments.length, 2);  return dbmenu.setDownMenu(downMenu);};



export  function mkHiddenButton(dbmenu)  {sys.$params(arguments.length, 1);  return menu.mkEntry(
    [],
    ui.link(dbmenu.change)
      .add(ui.img("menu")
        .style("vertical-align:middle"))
  );};
