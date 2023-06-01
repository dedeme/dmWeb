import * as iter from '../_js/iter.js';import * as str from '../_js/str.js';import * as bytes from '../_js/bytes.js';import * as cryp from '../_js/cryp.js';import * as dic from '../_js/dic.js';import * as timer from '../_js/timer.js';import * as js from '../_js/js.js';import * as storage from '../_js/storage.js';import * as sys from '../_js/sys.js';import * as math from '../_js/math.js';import * as domo from '../_js/domo.js';import * as ui from '../_js/ui.js';import * as arr from '../_js/arr.js';import * as time from '../_js/time.js';import * as client from '../_js/client.js';import * as b64 from '../_js/b64.js';




import * as modalBox from  "../libdm/modalBox.js";
import * as image from  "../data/image.js";
import * as cts from  "../data/cts.js";
import * as dimSelector from  "../wgs/dimSelector.js";
import * as tr from  "../wgs/tr.js";
import * as i18n from  "../i18n.js";

const Q =sys.$checkNull( ui.q);
const II =sys.$checkNull( i18n.tlt);


export  async  function mk(wg)  {sys.$params(arguments.length, 1);
  const Rp =sys.$checkNull( await  client.send({
    prg: cts.appName,
    source: "Home",
    rq: "idata"
  }));

  const Images =sys.$checkNull( arr.map(Rp.images, image.fromJs));
  const Group =sys.$checkNull( [0]);

  

  
   async  function save(ev)  {sys.$params(arguments.length, 1);
    if (sys.asBool(ui.confirm(i18n.fmt(II("Save pictures in group %0?"), [Group[0]])))) {
      cts.BoxContent
        .removeAll()
        .add(ui.img("wait.gif"))
      ;
      modalBox.show(cts.Box, true);
      await client.send({
        prg: cts.appName,
        source: "Home",
        rq: "save",
        group: Group[0]
      });
      window.location.reload(true);
    }}
  ;

  
   async  function mkSave(td)  {sys.$params(arguments.length, 1);
    td
      .removeAll()
      .add(ui.img("wait.gif"));
    const Rp =sys.$checkNull( await  client.send({
      prg: cts.appName,
      source: "Home",
      rq: "idata2"
    }));
    const Duplicates =sys.$checkNull( Rp.duplicates);
    Group[0] =sys.$checkExists(Group[0],sys.$checkNull( Rp.group));

    if (sys.asBool(!sys.asBool(Duplicates))) {
      td
        .removeAll()
        .add(ui.link(save)
          .klass("link")
          .text(i18n.fmt(II("Save in Group %0"), [Group[0]])))
      ;
    } else {
      td
        .removeAll()
        .html("<p>" + II("There are duplicated pictures") + ":</p><p>" +
            arr.join(Duplicates, "<br>") + "</p>")
      ;
    }
  };

  
   function changeDim(dim)  {sys.$params(arguments.length, 1);
    
     function cancel()  {sys.$params(arguments.length, 0); modalBox.show(cts.Box, false);};
    
     async  function accept(dim)  {sys.$params(arguments.length, 1);
      await client.send({
        prg: cts.appName,
        source: "Home",
        rq: "changeDim",
        width: dim.width,
        height: dim.height
      });
      window.location.reload(true);
    };

    dimSelector.mk(cts.BoxContent, dim, cancel, accept);
    modalBox.show(cts.Box, true);
  };

  
   async  function mkDim(td)  {sys.$params(arguments.length, 1);
    td
      .removeAll()
      .add(ui.img("wait.gif"));
    const Rp =sys.$checkNull( await  client.send({
      prg: cts.appName,
      source: "Home",
      rq: "dim"
    }));
    const dim =sys.$checkNull( "" + Rp.width + " x " + Rp.height);

    td
      .removeAll()
      .add(ui.link(function(ev)  {sys.$params(arguments.length, 1); changeDim(dim);})
        .klass("link")
        .text(dim))
    ;
  };

  

  if (sys.asBool(!sys.asBool(Images))) {
    wg
      .removeAll()
      .add(Q("table")
        .att("align", "center")
        .add(Q("tr")
          .add(Q("td")
          .klass("frame")
          .text(II("There are no pictures to adjust")))))
    ;
    return;
  }

  const TdSave =sys.$checkNull( Q("td")
    .klass("frame")
    .style("text-aling:left;width:5px;white-space:nowrap"))
  ;
  const TdDim =sys.$checkNull( Q("td")
    .klass("frame")
    .style("text-align:right;width:5px;white-space:nowrap"))
  ;
  const Trs =sys.$checkNull( []);
  arr.push(Trs, Q("tr")
    .add(Q("td")
      .att("colspan", 5)
      .style(
          "pading-top:15px;" +
          "pading-bottom:10px"
        )
      .add(Q("table")
        .klass("main")
        .add(Q("tr")
          .add(TdSave)
          .add(Q("td"))
          .add(TdDim))))
  );
  arr.push(Trs, Q("tr")
    .add(Q("td")
      .att("colspan", 5)
      .add(Q("hr")))
  );

  mkSave(TdSave);
  mkDim(TdDim);
  for (let ix  of sys.$forObject( iter.$range(0,arr.size(Images)))) arr.push(Trs, tr.mk(Images, ix));

  wg
    .removeAll()
    .add(Q("table")
      .att("align", "center")
      .adds(Trs))
    .add(cts.Box.wg)
  ;

};
