import * as iter from '../../../_js/iter.js';import * as str from '../../../_js/str.js';import * as bytes from '../../../_js/bytes.js';import * as cryp from '../../../_js/cryp.js';import * as dic from '../../../_js/dic.js';import * as timer from '../../../_js/timer.js';import * as js from '../../../_js/js.js';import * as storage from '../../../_js/storage.js';import * as sys from '../../../_js/sys.js';import * as math from '../../../_js/math.js';import * as domo from '../../../_js/domo.js';import * as ui from '../../../_js/ui.js';import * as arr from '../../../_js/arr.js';import * as time from '../../../_js/time.js';import * as client from '../../../_js/client.js';import * as b64 from '../../../_js/b64.js';




import * as menu from  "../../../libdm/menu.js";
import * as cts from  "../../../data/cts.js";
import * as ann from  "../../../data/acc/ann.js";
import * as msg from  "../../../wgs/msg.js";
import * as allWg from  "../../../pgs/settings/acc/allWg.js";
import * as invWg from  "../../../pgs/settings/acc/invWg.js";
import * as i18n from  "../../../i18n.js";

const Q =sys.$checkNull( ui.q);
const II =sys.$checkNull( i18n.tlt);




export  async  function mk(wg)  {sys.$params(arguments.length, 1);
  const Rp =sys.$checkNull( await  client.send({
    prg: cts.appName,
    module: "Settings",
    source: "acc/AccPg",
    rq: "idata"
  }));
  const investors =sys.$checkNull( Rp.investors); 

  const menuDiv =sys.$checkNull( Q("div"));
  const body =sys.$checkNull( Q("div"));

  const MkMenu =sys.$checkNull( [[]]);

  

  
   function all()  {sys.$params(arguments.length, 0);
    MkMenu[0]("all");
    allWg.mk(body);
  };

  
   function investor(inv)  {sys.$params(arguments.length, 1);
    MkMenu[0](II("Inv") + "-" + inv);
    invWg.mk(body, inv);
  };

  
   function closeYear()  {sys.$params(arguments.length, 0); msg.info(String.raw
`<p>Para cerrar el ejercicio hay que reiniciar el servidor una vez que
el nuevo año ha comenzado.</p>
<p>Para modificar datos del año anterior hay que
  <ol>
    <li>Eliminar los archivos 'data/investor/diaries/<i>lastYear</i>.db'.</li>
    <li>Recargar la página web (sin reiniciar el servidor)</li>
    <li>Realizar las modificaciones.</li>
    <li>Reiniciar el servidor.</li>
  </ol></p>
<p><i>Todos los datos del actual ejercicio se perderán</i></p>
<p>Antes del paso '1' se puede hacer una copia de los archivos que se van
a eliminar, borrando los asientos de inicialización, y después del proceso
pegar dichos archivos a los nuevos creados.</p>
`, function(){sys.$params(arguments.length, 0);});};


  

  
  MkMenu[0] =sys.$checkExists(MkMenu[0], function(sel)  {sys.$params(arguments.length, 1);
    const Lopts =sys.$checkNull( [
      menu.toption("all", II("All"), all)
    ]);
    for (let i = 0;i < investors; ++i) {
      const lb =sys.$checkNull( II("Inv") + "-" + i);
      arr.push(Lopts, menu.separator());
      arr.push(Lopts, menu.toption(lb, lb, function()  {sys.$params(arguments.length, 0); investor(i);}));
    }
    const Ropts =sys.$checkNull( [menu.toption("close", II("Close Year Help"), closeYear)]);
    menuDiv
      .removeAll()
      .add(menu.mk(Lopts, Ropts, sel, false))
    ;
  });

  wg
    .removeAll()
    .add(menuDiv)
    .add(body)
  ;

  all();
};
