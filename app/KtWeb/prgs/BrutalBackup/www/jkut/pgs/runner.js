import * as iter from '../_js/iter.js';import * as str from '../_js/str.js';import * as bytes from '../_js/bytes.js';import * as cryp from '../_js/cryp.js';import * as dic from '../_js/dic.js';import * as timer from '../_js/timer.js';import * as js from '../_js/js.js';import * as storage from '../_js/storage.js';import * as sys from '../_js/sys.js';import * as math from '../_js/math.js';import * as domo from '../_js/domo.js';import * as ui from '../_js/ui.js';import * as arr from '../_js/arr.js';import * as time from '../_js/time.js';import * as client from '../_js/client.js';import * as b64 from '../_js/b64.js';




import * as i18n from  "../i18n.js";
import * as cts from  "../data/cts.js";
import * as progressBar from  "../wgs/progressBar.js";

const Q =sys.$checkNull( ui.q);
const II =sys.$checkNull( i18n.tlt);



export  async  function mk(wg, id)  {sys.$params(arguments.length, 2);
  const Rp =sys.$checkNull( await  client.send({
    prg: cts.appName,
    source: "Runner",
    rq: "run",
    id: id
  }));
  const tm =sys.$checkNull( math.toInt(Rp.tm));

  const startTime =sys.$checkNull( time.now());
  const tmTimer =sys.$checkNull( timer.mk(1000));
  const bkTimer =sys.$checkNull( timer.mk(1000));
  const progressDiv =sys.$checkNull( Q("div"));
  const TmProgressBar =sys.$checkNull( [[]]);
  const BkProgressBar =sys.$checkNull( [[]]);
  const editor =sys.$checkNull( Q("textarea")
    .att("rows", 10)
    .att("cols", 80)
    .att("spellcheck", false)
    .disabled(true))
  ;

  if (sys.asBool(tm < 0)) {
    TmProgressBar[0] =sys.$checkExists(TmProgressBar[0],sys.$checkNull( progressBar.mk(progressDiv, 0)));
    TmProgressBar[0].lock(II("Time to read files list is not kown yet"));
  } else {
    const TmProgressBar =sys.$checkNull( progressBar.mk(progressDiv, tm));
    TmProgressBar.show();
  }

  

  
   function toList(ev)  {sys.$params(arguments.length, 1);
    window.location.assign("");
  };

  
   async  function bkProgress()  {sys.$params(arguments.length, 0);
    const Rp =sys.$checkNull( await  client.send({
      prg: cts.appName,
      source: "Runner",
      rq: "state",
      id: id
    }));
    const State =sys.$checkNull( arr.map(Rp.state, math.toInt));
    const Errors =sys.$checkNull( Rp.errors);

    if (sys.asBool(sys.$eq(arr.size(State) , 0))) {
      timer.stop(bkTimer);
      BkProgressBar[0].lock(II("Backup finalized"));
    } else {
      BkProgressBar.setValue(State[0]);
    }

    editor.text(arr.join(Errors, "\n"));
  };

  
   async  function tmProgress()  {sys.$params(arguments.length, 0);
    const Rp =sys.$checkNull( await  client.send({
      prg: cts.appName,
      source: "Runner",
      rq: "state",
      id: id
    }));
    const State =sys.$checkNull( arr.map(Rp.state, math.toInt));
    const Errors =sys.$checkNull( Rp.errors);

    if (sys.asBool(sys.$neq(arr.size(State) , 1))) {
      timer.stop(tmTimer);
      await client.send({
        prg: cts.appName,
        source: "Runner",
        rq: "setTm",
        id: id,
        tm: time.now() - startTime
      });
      const end =sys.$checkNull(sys.asBool( sys.$eq(arr.size(State) , 2)) ? State[1] : 0);
      BkProgressBar[0] =sys.$checkExists(BkProgressBar[0],sys.$checkNull( progressBar.mk(progressDiv, end)));
      BkProgressBar[0].show();
      timer.run(bkTimer, bkProgress);
    } else {
      TmProgressBar[0].setValue((time.now - startTime) / 1000);
    }

    editor.text(arr.join(Errors, "\n"));
  };


  

  wg
    .removeAll()
    .add(Q("table")
      .klass("main")
      .add(Q("tr")
        .add(Q("td")
          .style("width: 5px;white-space:nowrap;")
          .add(Q("div")
            .klass("head")
            .text(i18n.fmt(II("Backup of '%0'"), [id]))))
        .add(Q("td")
          .style("text-align: right")
          .add(ui.link(toList)
            .klass("link")
            .text(II("List"))))))
    .add(Q("hr"))
    .add(progressDiv)
    .add(Q("table")
      .att("align", "center")
      .add(Q("tr")
        .add(Q("td")
          .add(editor))))
  ;

  timer.run(tmTimer, tmProgress);
};
