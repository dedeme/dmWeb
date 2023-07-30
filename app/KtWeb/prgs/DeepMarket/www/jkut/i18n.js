import * as iter from './_js/iter.js';import * as str from './_js/str.js';import * as bytes from './_js/bytes.js';import * as cryp from './_js/cryp.js';import * as dic from './_js/dic.js';import * as timer from './_js/timer.js';import * as js from './_js/js.js';import * as storage from './_js/storage.js';import * as sys from './_js/sys.js';import * as math from './_js/math.js';import * as domo from './_js/domo.js';import * as ui from './_js/ui.js';import * as arr from './_js/arr.js';import * as time from './_js/time.js';import * as client from './_js/client.js';import * as b64 from './_js/b64.js';



const esDic =sys.$checkNull( {
  "Appr.": "Aprx.",
  "Approximation": "Aproximación",
  "Assets": "Activo",
  "Average": "Media",
  "Best": "Mejores",
  "Buys": "Compras",
  "Cash": "Caja",
  "Change near start corner from\n[%0, %1]\nto\n[%2, %3]?": "¿Cambiar la esquina de comienzo (Cerca) desde\n[%0, %1]\na\n[%2, %3]?",
  "Charts": "Gráficos",
  "Click %0 to continue.": "Hacer click %0 para continuar.",
  "Close": "Cierre",
  "Companies": "Compañías",
  "Date": "Fecha",
  "Eval.": "Eval.",
  "Far - Ranking": "Clasificación (Lejos)",
  "Historic": "Histórico",
  "KtWeb session is closed.\nAuthenticating from KtWeb:Main.": "La sesión de KtWeb ha sido cerrada.\nHay que autenticarse en KtWeb:Main.",
  "Malformed URL": "URL mal formada",
  "Near - Ranking": "Clasificación (Cerca)",
  "Operations": "Operaciones",
  "Orders": "Órdenes",
  "Portfolio": "Cartera",
  "Prfs.": "Bfs.",
  "Profits (%)": "Beneficios (%)",
  "Quarantine": "Cuarentena",
  "Sales": "Ventas",
  "Sells": "Ventas",
  "Session is expired.": "La sesión ha expirado.",
  "Sls.": "Vts.",
  "Soon the near star corner will be changed to\n[%0, %1]": "En breve la esquina de comienzo (Cerca) se cambiará a\n[%0, %1]",
  "Start": "Inicio",
  "Value '%0' for 'days loss' is not valid": "El valor %0 no es valido para 'dias perdiendo'",
  "Value '%0' for 'days win' is not valid": "El valor %0 no es valido para 'dias ganando'",
  "Value [%0,%1] for parameters is not valid": "El valor de [%0,%1] no es válido para los parámetros",
  "Withdrawals": "Retiradas",
  "Without Data": "Sin datos",
  "Worst": "Peores",
  "[Far - Hot Map]": "Mapa de calor (Lejos)",
  "[Far - List]": "Listado (Lejos)",
  "[Near - Hot Map]": "Mapa de calor (Cerca)",
  "[Near - List]": "Listado (Cerca)",
  "here": "aquí"
});

export  function es() {sys.$params(arguments.length, 0); Lang[0] =sys.$checkExists(Lang[0],sys.$checkNull( "es"));};

const enDic =sys.$checkNull( {
  "Appr.": "Appr.",
  "Approximation": "Approximation",
  "Assets": "Assets",
  "Average": "Average",
  "Best": "Best",
  "Buys": "Buys",
  "Cash": "Cash",
  "Change near start corner from\n[%0, %1]\nto\n[%2, %3]?": "Change near start corner from\n[%0, %1]\nto\n[%2, %3]?",
  "Charts": "Charts",
  "Click %0 to continue.": "Click %0 to continue.",
  "Close": "Close",
  "Companies": "Companies",
  "Date": "Date",
  "Eval.": "Eval.",
  "Far - Ranking": "Far - Ranking",
  "Historic": "Historic",
  "KtWeb session is closed.\nAuthenticating from KtWeb:Main.": "KtWeb session is closed.\nAuthenticating from KtWeb:Main.",
  "Malformed URL": "Malformed URL",
  "Near - Ranking": "Near - Ranking",
  "Operations": "Operations",
  "Orders": "Orders",
  "Portfolio": "Portfolio",
  "Prfs.": "Prfs.",
  "Profits (%)": "Profits (%)",
  "Quarantine": "Quarantine",
  "Sales": "Sales",
  "Sells": "Sells",
  "Session is expired.": "Session is expired.",
  "Sls.": "Sls.",
  "Soon the near star corner will be changed to\n[%0, %1]": "Soon the near star corner will be changed to\n[%0, %1]",
  "Start": "Start",
  "Value '%0' for 'days loss' is not valid": "Value '%0' for 'days loss' is not valid",
  "Value '%0' for 'days win' is not valid": "Value '%0' for 'days win' is not valid",
  "Value [%0,%1] for parameters is not valid": "Value [%0,%1] for parameters is not valid",
  "Withdrawals": "Withdrawals",
  "Without Data": "Without Data",
  "Worst": "Worst",
  "[Far - Hot Map]": "[Far - Hot Map]",
  "[Far - List]": "[Far - List]",
  "[Near - Hot Map]": "[Near - Hot Map]",
  "[Near - List]": "[Near - List]",
  "here": "here"
});

export  function en() {sys.$params(arguments.length, 0); Lang[0] =sys.$checkExists(Lang[0],sys.$checkNull( "en"));};


 function dicByKey(s)  {sys.$params(arguments.length, 1);    
  return sys.$eq(s,"es")? esDic:
  sys.$eq(s,"en")? enDic:
   "Unreachable"
;};

const Lang =sys.$checkNull( ["es"]);

export  function getLang() {sys.$params(arguments.length, 0);  return Lang[0];};


export  function tlt(s)  {sys.$params(arguments.length, 1);
  const T =sys.$checkNull( dic.get(dicByKey(Lang[0]), s));
  return sys.asBool( T) ? T[0] : s;
};


export  function fmt(tp, Rpls)  {sys.$params(arguments.length, 2);
  const R =sys.$checkNull( [tp]);
  for (let i = 0;i < arr.size(Rpls); ++i) R[0] =sys.$checkExists(R[0],sys.$checkNull( str.replace(R[0], "%" + sys.toStr(i), Rpls[i])));
   return R[0];
};
