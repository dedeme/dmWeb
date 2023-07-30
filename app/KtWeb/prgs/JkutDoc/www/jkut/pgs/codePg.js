import * as iter from '../_js/iter.js';import * as str from '../_js/str.js';import * as bytes from '../_js/bytes.js';import * as cryp from '../_js/cryp.js';import * as dic from '../_js/dic.js';import * as timer from '../_js/timer.js';import * as js from '../_js/js.js';import * as storage from '../_js/storage.js';import * as sys from '../_js/sys.js';import * as math from '../_js/math.js';import * as domo from '../_js/domo.js';import * as ui from '../_js/ui.js';import * as arr from '../_js/arr.js';import * as time from '../_js/time.js';import * as client from '../_js/client.js';import * as b64 from '../_js/b64.js';




import * as i18n from  "../i18n.js";
import * as cts from  "../cts.js";
import * as msgPg from  "../pgs/msgPg.js";

const Q =sys.$checkNull( ui.q);
const II =sys.$checkNull( i18n.tlt);

const reserved =sys.$checkNull( "async await break catch class continue default do else " +
  "eval false finally for if import new null return switch trace throw " +
  "true try while")
;
const lib =sys.$checkNull( "arr b64 bytes client cryp dic domo iter js math regex storage " +
  "str sys time ui")
;
const special =sys.$checkNull( "Q Math window II");


const stCode =sys.$checkNull( 0);
const stLong =sys.$checkNull( stCode + 1); 
const stQ =sys.$checkNull( stLong + 1); 


export  async  function mk(wg, pack, path, anchor)   {sys.$params(arguments.length, 4);
  const prefix =sys.$checkNull(sys.asBool( anchor.startsWith("hp::")) ? "hp::" : "hp:");
  const Left =sys.$checkNull( [""]);
  const Right =sys.$checkNull( [""]);
  const LineCounter =sys.$checkNull( [0]);
  const CharQuotes =sys.$checkNull( [""]);
  const State =sys.$checkNull( [stCode]);

  
   function newLine() {sys.$params(arguments.length, 0);
    LineCounter[0] +=sys.$checkExists(LineCounter[0],sys.$checkNull( 1));
    Right[0] +=sys.$checkExists(Right[0],sys.$checkNull( "<br>"));
    Left[0] +=sys.$checkExists(Left[0],sys.$checkNull( "<span style='font-family: monospace;font-size: 12px;" +
      "background-color: rgb(215, 215, 215);color: #998866;'>" +
      formatN(LineCounter[0]) +
      "</span><br>"));
  };

  
   function processCode(l)  {sys.$params(arguments.length, 1);
    
     function makeLink(code)  {sys.$params(arguments.length, 1);
      const Bf =sys.$checkNull( [""]);
      for (let i = 0;i < str.len(code); ++i) {
        const ch =sys.$checkNull( code[i]);
        if (sys.asBool(sys.$eq(ch , "#"))) {
          Bf[0] +=sys.$checkExists(Bf[0],sys.$checkNull( "_"));
        } else if (sys.asBool(ch > " ")) {
          Bf[0] +=sys.$checkExists(Bf[0],sys.$checkNull( ch));
        }
      }

      const Ix =sys.$checkNull( [Bf[0].indexOf("=")]);
      const ix2 =sys.$checkNull( Bf[0].indexOf("("));
      if (sys.asBool(sys.asBool(sys.$eq(Ix[0] ,  -1)) || sys.asBool((sys.asBool(sys.$neq(ix2 ,  -1)) && sys.asBool(ix2 < Ix[0]))))) {
        Ix[0] =sys.$checkExists(Ix[0],sys.$checkNull( ix2));
        if (sys.asBool(sys.$neq(Ix[0] ,  -1))) {
          if (sys.asBool(str.starts(sys.$slice(Bf[0],Ix[0],null), "(*"))) {
            const ix2 =sys.$checkNull( Bf[0].indexOf("(", Ix[0] + 1));
            if (sys.asBool(sys.$neq(ix2 ,  -1))) Ix[0] =sys.$checkExists(Ix[0],sys.$checkNull( ix2));
          }
        }
      }
      if (sys.asBool(sys.$eq(Ix[0] ,  -1))) Ix[0] =sys.$checkExists(Ix[0],sys.$checkNull( Bf[0].indexOf(";")));
      if (sys.asBool(sys.$eq(Ix[0] ,  -1))) Ix[0] =sys.$checkExists(Ix[0],sys.$checkNull( Bf[0].length));

       return sys.$slice(Bf[0],null,Ix[0]);
    };

    const R =sys.$checkNull( [toHtml(l)]);

    for (let w  of sys.$forObject( reserved.split(" "))) {
      const Ix =sys.$checkNull( [str.index(R[0], w)]);
      while (sys.asBool(sys.$neq(Ix[0] ,  -1))) {
        const ix1 =sys.$checkNull( Ix[0]);
        const ix2 =sys.$checkNull( ix1 + w.length);
        if (sys.asBool(sys.asBool((sys.asBool(sys.$eq(ix1 , 0)) || sys.asBool(isNotId(R[0][ix1 - 1])))) &&
            sys.asBool((sys.asBool(sys.$eq(ix1 , str.len(R[0]))) || sys.asBool(isNotId(R[0][ix2])))))
        ) {
          R[0] =sys.$checkExists(R[0],sys.$checkNull( sys.$slice(R[0],null,Ix[0]) + "<span==>" + w +
            "</span>" + sys.$slice(R[0],Ix[0] + w.length,null)));
        }
        Ix[0] =sys.$checkExists(Ix[0],sys.$checkNull( str.indexFrom(R[0], w, ix2 + 25)));
      }
    }
    R[0] =sys.$checkExists(R[0],sys.$checkNull( str.replace(R[0], "<span==>", "<span class='reserved'>")));

    for (let w  of sys.$forObject( lib.split(" "))) {
      const Ix =sys.$checkNull( [str.index(R[0], w)]);
      while (sys.asBool(sys.$neq(Ix[0] ,  -1))) {
        const ix1 =sys.$checkNull( Ix[0]);
        const ix2 =sys.$checkNull( ix1 + w.length);
        if (sys.asBool(sys.asBool((sys.asBool(sys.$eq(ix1 , 0)) || sys.asBool(isNotId(R[0][ix1 - 1])))) &&
            sys.asBool((sys.asBool(sys.$eq(ix1 , str.len(R[0]))) || sys.asBool(isNotId(R[0][ix2])))))
        ) {
          R[0] =sys.$checkExists(R[0],sys.$checkNull( sys.$slice(R[0],null,Ix[0]) + "<span class='package'>" + w +
            "</span>" + sys.$slice(R[0],Ix[0] + w.length,null)));
        }
        Ix[0] =sys.$checkExists(Ix[0],sys.$checkNull( str.indexFrom(R[0], w, ix2 + 25)));
      }
    }

    for (let w  of sys.$forObject( special.split(" "))) {
      const Ix =sys.$checkNull( [str.index(R[0], w)]);
      while (sys.asBool(sys.$neq(Ix[0] ,  -1))) {
        const ix1 =sys.$checkNull( Ix[0]);
        const ix2 =sys.$checkNull( ix1 + w.length);
        if (sys.asBool(sys.asBool((sys.asBool(sys.$eq(ix1 , 0)) || sys.asBool(isNotId(R[0][ix1 - 1])))) &&
            sys.asBool((sys.asBool(sys.$eq(ix1 , str.len(R[0]))) || sys.asBool(isNotId(R[0][ix2])))))
        ) {
          R[0] =sys.$checkExists(R[0],sys.$checkNull( sys.$slice(R[0],null,Ix[0]) + "<span class='special'>" + w +
            "</span>" + sys.$slice(R[0],Ix[0] + w.length,null)));
        }
        Ix[0] =sys.$checkExists(Ix[0],sys.$checkNull( str.indexFrom(R[0], w, ix2 + 25)));
      }
    }

    const UpperBf =sys.$checkNull( [""]);
    const St =sys.$checkNull( [0]);
    Right[0] +=sys.$checkExists(Right[0],sys.$checkNull( arr.reduce(str.split(R[0], ""), "", function(seed, ch)  {sys.$params(arguments.length, 2);
      if (sys.asBool(sys.asBool(sys.$eq(St[0] , 0)) || sys.asBool(sys.$eq(St[0] , 3)))) { 
        if (sys.asBool(isNumber(ch))) {
          St[0] =sys.$checkExists(St[0],sys.$checkNull( 1));
           return seed + "<span class='number'>" + ch;
        }
        if (sys.asBool(isUpper(ch))) {
          UpperBf[0] =sys.$checkExists(UpperBf[0],sys.$checkNull( ch));
          St[0] =sys.$checkExists(St[0],sys.$checkNull( 2));
           return seed;
        }
        if (sys.asBool(isNotId(ch))) {
          St[0] =sys.$checkExists(St[0],sys.$checkNull( 3));
           return seed + ch;
        }
        St[0] =sys.$checkExists(St[0],sys.$checkNull( 4));
         return seed + ch;
      }
      if (sys.asBool(sys.$eq(St[0] , 1))) { 
        if (sys.asBool(isNumber(ch)))
           return seed + ch;
        St[0] =sys.$checkExists(St[0],sys.$checkNull( 4));
        if (sys.asBool(isNotId(ch))) {
          St[0] =sys.$checkExists(St[0],sys.$checkNull( 3));
        }
         return seed + "</span>" + ch;
      }
      if (sys.asBool(sys.$eq(St[0] , 2))) { 
        if (sys.asBool(isNotId(ch))) {
          St[0] =sys.$checkExists(St[0],sys.$checkNull( 3));
          if (sys.asBool(sys.$eq(str.index(" " + special + " ", " " + UpperBf[0] + " ") ,  -1)))
             return seed + "<span class='container'>" + UpperBf[0] + "</span>" + ch;
          else
             return seed + UpperBf[0] + ch;
        }
        UpperBf[0] +=sys.$checkExists(UpperBf[0],sys.$checkNull( ch));
         return seed;
      } 
      if (sys.asBool(isNotId(ch)))
        St[0] =sys.$checkExists(St[0],sys.$checkNull( 3));
       return seed + ch;
    })));
    if (sys.asBool(sys.asBool(sys.$eq(St[0] , 1)) || sys.asBool(sys.$eq(St[0] , 2)))) {
      Right[0] +=sys.$checkExists(Right[0],sys.$checkNull( "</span>"));
    }

    if (sys.asBool(str.len(l) > 0)) {
      const ch =sys.$checkNull( l[0]);
      if (sys.asBool(sys.asBool(sys.asBool(ch > " ") &&
        sys.asBool(sys.$neq(ch , "("))) &&
        sys.asBool(sys.$neq(ch , "}")))
      ) {
        Left[0] +=sys.$checkExists(Left[0],sys.$checkNull( "<span id='" + prefix +
          makeLink(str.trim(l)) +
          "'></span>"));
      }
    }

  };

  
   function processLine(l)  {sys.$params(arguments.length, 1);
    if (sys.asBool(sys.$eq(State[0] , stLong))) { 
      const ix =sys.$checkNull( l.indexOf("*/"));
      if (sys.asBool(sys.$neq(ix ,  -1))) {
        State[0] =sys.$checkExists(State[0],sys.$checkNull( stCode));
        Right[0] +=sys.$checkExists(Right[0],sys.$checkNull( toHtml(sys.$slice(l,null,ix + 2)) + "</span>"));
        processLine(sys.$slice(l,ix + 2,null));
      } else {
        Right[0] +=sys.$checkExists(Right[0],sys.$checkNull( toHtml(l)));
        newLine();
      }
    } else if (sys.asBool(sys.$eq(State[0] , stQ))) { 
      const qix =sys.$checkNull( l.indexOf(CharQuotes[0]));
      if (sys.asBool(sys.$eq(qix ,  -1))) {
        if (sys.asBool(sys.$eq(str.len(CharQuotes[0]) , 3))) {
          Right[0] +=sys.$checkExists(Right[0],sys.$checkNull( toHtml(l)));
          newLine();
        } else {
          Right[0] +=sys.$checkExists(Right[0],sys.$checkNull( toHtml(l) + "</span>"));
          newLine();
          State[0] =sys.$checkExists(State[0],sys.$checkNull( stCode));
        }
        return;
      }
      if (sys.asBool(sys.$eq(CharQuotes[0].length , 3))) {
        State[0] =sys.$checkExists(State[0],sys.$checkNull( stCode));
        Right[0] +=sys.$checkExists(Right[0],sys.$checkNull( toHtml(sys.$slice(l,null,qix + 3)) + "</span>"));
        processLine(sys.$slice(l,qix + 3,null));
      } else {
        const bix =sys.$checkNull( l.indexOf("\\"));
        if (sys.asBool(sys.asBool(sys.$neq(bix ,  -1)) && sys.asBool(bix < qix))) {
          Right[0] +=sys.$checkExists(Right[0],sys.$checkNull( toHtml(sys.$slice(l,null,bix + 2))));
          processLine(sys.$slice(l,bix + 2,null));
        } else {
          State[0] =sys.$checkExists(State[0],sys.$checkNull( stCode));
          Right[0] +=sys.$checkExists(Right[0],sys.$checkNull( toHtml(sys.$slice(l,null,qix + 1)) + "</span>"));
          processLine(sys.$slice(l,qix + 1,null));
        }
      }
    } else { 
      if (sys.asBool(sys.$eq(l.trim() , ""))) {
        newLine();
        return;
      }
      const R =sys.$checkNull( [0]);
      const Pos =sys.$checkNull( [2000]);
      const Ix =sys.$checkNull( [l.indexOf("/*")]); 
      if (sys.asBool(sys.$neq(Ix[0] ,  -1))) {
        R[0] =sys.$checkExists(R[0],sys.$checkNull( 1));
        Pos[0] =sys.$checkExists(Pos[0],sys.$checkNull( Ix[0]));
      }
      Ix[0] =sys.$checkExists(Ix[0],sys.$checkNull( l.indexOf("//"))); 
      if (sys.asBool(sys.asBool(sys.$neq(Ix[0] ,  -1)) && sys.asBool(Ix[0] < Pos[0]))) {
        R[0] =sys.$checkExists(R[0],sys.$checkNull( 2));
        Pos[0] =sys.$checkExists(Pos[0],sys.$checkNull( Ix[0]));
      }
      Ix[0] =sys.$checkExists(Ix[0],sys.$checkNull( l.indexOf("\""))); 
      if (sys.asBool(sys.asBool(sys.$neq(Ix[0] ,  -1)) && sys.asBool(Ix[0] < Pos[0]))) {
        R[0] =sys.$checkExists(R[0],sys.$checkNull( 3));
        Pos[0] =sys.$checkExists(Pos[0],sys.$checkNull( Ix[0]));
      }
      Ix[0] =sys.$checkExists(Ix[0],sys.$checkNull( l.indexOf("'"))); 
      if (sys.asBool(sys.asBool(sys.$neq(Ix[0] ,  -1)) && sys.asBool(Ix[0] < Pos[0]))) {
        R[0] =sys.$checkExists(R[0],sys.$checkNull( 4));
        Pos[0] =sys.$checkExists(Pos[0],sys.$checkNull( Ix[0]));
      }

      if (sys.asBool(sys.$eq(R[0] , 1))) { 
        processCode(sys.$slice(l,null,Pos[0]));
        const l2 =sys.$checkNull( sys.$slice(l,Pos[0] + 2,null));
        if (sys.asBool(str.starts(l2, "*"))) {
          Right[0] +=sys.$checkExists(Right[0],sys.$checkNull( "<span class='docComment'>/*"));
          State[0] =sys.$checkExists(State[0],sys.$checkNull( stLong));
        } else {
          Right[0] +=sys.$checkExists(Right[0],sys.$checkNull( "<span class='comment'>/*"));
          State[0] =sys.$checkExists(State[0],sys.$checkNull( stLong));
        }
        processLine(l2);
      } else if (sys.asBool(sys.$eq(R[0] , 2))) { 
        processCode(sys.$slice(l,null,Pos[0]));
        const l2 =sys.$checkNull( sys.$slice(l,Pos[0] + 2,null));
        if (sys.asBool(str.starts(l2, "/"))) {
          Right[0] +=sys.$checkExists(Right[0],sys.$checkNull( "<span class='docComment'>//"));
        } else {
          Right[0] +=sys.$checkExists(Right[0],sys.$checkNull( "<span class='docComment'>//"));
        }
        Right[0] +=sys.$checkExists(Right[0],sys.$checkNull( toHtml(l2) + "</span>"));
        newLine();
      } else if (sys.asBool(sys.$eq(R[0] , 3))) { 
        processCode(sys.$slice(l,null,Pos[0]));
        State[0] =sys.$checkExists(State[0],sys.$checkNull( stQ));
        const l2 =sys.$checkNull( sys.$slice(l,Pos[0] + 1,null));
        if (sys.asBool(str.starts(l2, "\"\""))) {
          CharQuotes[0] =sys.$checkExists(CharQuotes[0],sys.$checkNull( "\"\"\""));
          Right[0] +=sys.$checkExists(Right[0],sys.$checkNull( "<span class='quote'>\"\"\""));
          processLine(sys.$slice(l2,2,null));
        } else {
          CharQuotes[0] =sys.$checkExists(CharQuotes[0],sys.$checkNull( "\""));
          Right[0] +=sys.$checkExists(Right[0],sys.$checkNull( "<span class='quote'>\""));
          processLine(l2);
        }
      } else if (sys.asBool(sys.$eq(R[0] , 4))) { 
        processCode(sys.$slice(l,null,Pos[0]));
        State[0] =sys.$checkExists(State[0],sys.$checkNull( stQ));
        CharQuotes[0] =sys.$checkExists(CharQuotes[0],sys.$checkNull( "'"));
        Right[0] +=sys.$checkExists(Right[0],sys.$checkNull( "<span class='quote'>'"));
        processLine(sys.$slice(l,Pos[0] + 1,null));
      } else {
        processCode(l);
        newLine();
      }
    }
  };

  
   function process(code)  {sys.$params(arguments.length, 1);
    for (let l  of sys.$forObject( code.split("\n"))) processLine(l);
  };

  

  const Rp =sys.$checkNull( await  client.ssend({
    prg: "JkutDoc",
    source: "CodePg",
    rq: "code",
    pack: pack,
    path: path
  }));

  if (sys.asBool(!sys.asBool(Rp.code))) {
    msgPg.mk(wg, i18n.fmt(II("[%0] Jkut file not found."), [path]), true);
    return;
  }

  const code =sys.$checkNull( Rp.code[0]);
  process(code);

  const barIx =sys.$checkNull( str.lastIndex(path, "/") + 1);
  Q("@title").text(
    cts.appName + " - " + sys.$slice(path,barIx,null) + ".jkut"
  );

  wg
    .removeAll()
    .add(Q("table")
      .att("id", prefix)
      .add(Q("tr")
        .add(Q("td")
          .klass("frame")
          .add(Q("a")
            .att("href", "?" + pack + "@" + path)
            .text(path)))))
    .add(Q("table")
      .style("boder:0;width:100%")
      .att("cellspacing", "0")
      .add(Q("tr")
        .add(Q("td")
          .klass("prel")
          .style("width:10px")
          .html(Left[0]))
        .add(Q("td")
          .klass("prer")
          .html(Right[0]))))
    .adds(arr.fromIter(iter.map(iter.$range(0,30), function(i)  {sys.$params(arguments.length, 1);  return Q("p").html("&nbsp;");})))
  ;

  const Tg =sys.$checkNull( sys.$null((Q("#" + anchor).e)));
  if (sys.asBool(Tg)) Tg[0].scrollIntoView(true);
};


 function isNumber(ch)  {sys.$params(arguments.length, 1);  return sys.asBool(ch >= "0") && sys.asBool(ch <= "9");};


 function isUpper(ch)  {sys.$params(arguments.length, 1);  return (sys.asBool(ch >= "A") && sys.asBool(ch <= "Z"));};


 function isLetter(ch)  {sys.$params(arguments.length, 1);  return sys.asBool((sys.asBool(ch >= "a") && sys.asBool(ch <= "z"))) || sys.asBool((sys.asBool(ch >= "A") && sys.asBool(ch <= "Z")));};


 function isNotId(ch)  {sys.$params(arguments.length, 1);  return sys.asBool(!sys.asBool(isNumber(ch))) && sys.asBool(!sys.asBool(isLetter(ch)));};


 function toHtml(s)  {sys.$params(arguments.length, 1);  return s
  .split("&").join("&amp;")
  .split(" ").join("&nbsp;")
  .split("<").join("&lt;")
;};


 function formatN(n)  {sys.$params(arguments.length, 1);
  const r =sys.$checkNull( "" + n);
   return iter.reduce(
    iter.$range(0, 4 - str.len(r)), r, function(seed, i)  {sys.$params(arguments.length, 2);  return "&nbsp;" + seed;}
  );
};
