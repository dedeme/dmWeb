import * as iter from './_js/iter.js';import * as str from './_js/str.js';import * as bytes from './_js/bytes.js';import * as cryp from './_js/cryp.js';import * as dic from './_js/dic.js';import * as timer from './_js/timer.js';import * as js from './_js/js.js';import * as storage from './_js/storage.js';import * as sys from './_js/sys.js';import * as math from './_js/math.js';import * as domo from './_js/domo.js';import * as ui from './_js/ui.js';import * as arr from './_js/arr.js';import * as time from './_js/time.js';import * as client from './_js/client.js';import * as b64 from './_js/b64.js';



const esDic = sys.$checkNull({
  "2 Days": "2 días",
  "All": "Todo",
  "All log entries will be deleted.\nContinue?": "Todas las entradas serán borradas.\n¿Continuar?",
  "AllRankings": "Todas",
  "Assets": "Patrimonio",
  "Assets average": "Media del patrimonio",
  "Bests": "Mejores",
  "Click %0 to continue.": "Click %0 para continuar.",
  "Current": "Actual",
  "Cycle": "Ciclo",
  "Delete": "Borrar",
  "Duplicates": "Repeticiones",
  "Errors": "Errores",
  "Evaluating": "Evaluando",
  "Fleas number": "Numero de pulgas",
  "Generating": "Generando",
  "Home": "Inicio",
  "Id": "Id",
  "KtWeb session is closed.\nAuthenticating from KtWeb:Main.": "La sesión de KtWeb ha sido cerrada.\nHay que autenticarse en KtWeb:Main.",
  "Log": "Registro",
  "Models": "Modelos",
  "Ms.": "Ms.",
  "Pos.": "Pos.",
  "Position average": "Posición media",
  "Ranking": "Clasificación",
  "Rankings": "Clasificaciones",
  "Reload": "Recargar",
  "Saving": "Guardando",
  "Selecting": "Seleccionando",
  "Session is expired.": "Las sesión ha expirado.",
  "Start": "Comenzar",
  "Stopped": "Detenido",
  "Worsts": "Peores",
  "here": "aquí"
});

export function es () {sys.$params(arguments.length, 0); Lang[0] =sys.$checkExists(Lang[0], sys.$checkNull("es"));};

const enDic = sys.$checkNull({
  "2 Days": "2 Days",
  "All": "All",
  "All log entries will be deleted.\nContinue?": "All log entries will be deleted.\nContinue?",
  "AllRankings": "All",
  "Assets": "Assets",
  "Assets average": "Assets average",
  "Bests": "Bests",
  "Click %0 to continue.": "Click %0 to continue.",
  "Current": "Current",
  "Cycle": "Cycle",
  "Delete": "Delete",
  "Duplicates": "Duplicates",
  "Errors": "Errors",
  "Evaluating": "Evaluating",
  "Fleas number": "Fleas number",
  "Generating": "Generating",
  "Home": "Home",
  "Id": "Id",
  "KtWeb session is closed.\nAuthenticating from KtWeb:Main.": "KtWeb session is closed.\nAuthenticating from KtWeb:Main.",
  "Log": "Log",
  "Models": "Models",
  "Ms.": "Ms.",
  "Pos.": "Pos.",
  "Position average": "Position average",
  "Ranking": "Ranking",
  "Rankings": "Rankings",
  "Reload": "Reload",
  "Saving": "Saving",
  "Selecting": "Selecting",
  "Session is expired.": "Session is expired.",
  "Start": "Start",
  "Stopped": "Stopped",
  "Worsts": "Worsts",
  "here": "here"
});

export function en () {sys.$params(arguments.length, 0); Lang[0] =sys.$checkExists(Lang[0], sys.$checkNull("en"));};


function dicByKey (s)  {sys.$params(arguments.length, 1);    
  return sys.$eq(s,"es")? esDic:
  sys.$eq(s,"en")? enDic:
   "Unreachable"
;};

const Lang = sys.$checkNull(["es"]);

export function getLang () {sys.$params(arguments.length, 0);  return Lang[0];};


export function tlt (s)  {sys.$params(arguments.length, 1);
  const T = sys.$checkNull(dic.get(dicByKey(Lang[0]), s));
  return sys.asBool( T) ? T[0] : s;
};


export function fmt (tp, Rpls)  {sys.$params(arguments.length, 2);
  const R = sys.$checkNull([tp]);
  for (let i = 0;i < arr.size(Rpls); ++i) R[0] =sys.$checkExists(R[0], sys.$checkNull(str.replace(R[0], "%" + sys.toStr(i), Rpls[i])));
   return R[0];
};
