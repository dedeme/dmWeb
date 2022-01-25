// Copyright 24-Dic-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/cts.h"
#include "dmc/str.h"


char *cts_app_name (void) {
  return "Bet1x2";
}

char *cts_data_dir (void) {
  return "data";
}

char *cts_data_version (void) {
  return str_cat(cts_app_name(), "\nData version: 202112\n", NULL);
}

char *cts_web_app_dir(void) {
  return str_cat("dmcgi/", cts_app_name(), NULL);
}

int cts_expiration (void) {
  return 900;
}

char *cts_year(void) {
  return "2021";
}

Mchar *cts_teams(void) {
  Mchar *r = mchar_new();
  mchar_put(r, "real-madrid","Real Madrid");
  mchar_put(r, "valencia","Valencia CF");
  mchar_put(r, "atletico","Atlético de Madrid");
  mchar_put(r, "real-sociedad","Real Sociedad");
  mchar_put(r, "athletic","Athletic Club");
  mchar_put(r, "sevilla","Sevilla FC");
  mchar_put(r, "barcelona","FC Barcelona");
  mchar_put(r, "mallorca","RCD Mallorca");
  mchar_put(r, "betis","Real Betis");
  mchar_put(r, "elche","Elche CF");
  mchar_put(r, "cadiz","Cádiz CF");
  mchar_put(r, "osasuna","CA Osasuna");
  mchar_put(r, "rayo","Rayo Vallecano");
  mchar_put(r, "villarreal","Villarreal CF");
  mchar_put(r, "levante","Levante UD");
  mchar_put(r, "espanyol","RCD Espanyol");
  mchar_put(r, "granada","Granada CF");
  mchar_put(r, "celta","RC Celta");
  mchar_put(r, "getafe","Getafe CF");
  mchar_put(r, "alaves","Deportivo Alavés");
  return r;
}

Achar *cts_marca_teams(void) {
  return achar_new_from(
    "186", // real-madrid
    "191", // valencia
    "175", // atletico
    "188", // real-sociedad
    "174", // athletic
    "179", // sevilla
    "178", // barcelona
    "181", // mallorca
    "185", // betis
    "954", // elche
    "1737", // cadiz
    "450", // osasuna
    "184", // rayo
    "449", // villarreal
    "855", // levante
    "177", // espanyol
    "5683", // granada
    "176", // celta
    "1450", // getafe
    "173", // alaves
    NULL
  );
}
#include <stdio.h>
Achar *cts_as_teams(void) {
  return achar_new_from(
    "1", // real-madrid
    "17", // valencia
    "42", // atletico
    "16", // real-sociedad
    "5", // athletic
    "53", // sevilla
    "3", // barcelona
    "11", // mallorca
    "171", // betis
    "121", // elche
    "91", // cadiz
    "13", // osasuna
    "2", // rayo
    "19", // villarreal
    "136", // levante
    "8", // espanyol
    "347", // granada
    "6", // celta
    "172", // getafe
    "4", // alaves
    NULL
  );
}

Achar *cts_sportium_teams(void) {
  return achar_new_from(
    "Real Madrid", // real-madrid
    "Valencia CF", // valencia
    "Atlético Madrid", // atletico
    "Real Sociedad", // real-sociedad
    "Athletic Club", // athletic
    "FC Sevilla", // sevilla
    "FC Barcelona", // barcelona
    "RCD Mallorca", // mallorca
    "Real Betis", // betis
    "Elche", // elche
    "Cádiz CF", // cadiz
    "Osasuna", // osasuna
    "Rayo Vallecano", // rayo
    "Villarreal CF", // villarreal
    "Levante UD", // levante
    "RCD Espanyol", // espanyol
    "Granada CF", // granada
    "Celta de Vigo", // celta
    "Getafe CF", // getafe
    "Dep. Alaves", // alaves
    NULL
  );
}
