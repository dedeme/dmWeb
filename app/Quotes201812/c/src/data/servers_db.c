// Copyright 11-Sept-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/servers_db.h"
#include "data/Finanzas.h"
#include "data/Invertia.h"
#include "data/Infomercados.h"

static int n_servers = 3;
static Server *servers[] = {NULL, NULL, NULL};

Aserver *servers_db_list(void) {
  if (!servers[0]) {
    servers[0] = finanzas_mk();
    servers[1] = invertia_mk();
    servers[2] = infomercados_mk();
  }
  Aserver *r = aserver_new();
  RANGE0(i, n_servers) {
    aserver_add(r, servers[i]);
  }_RANGE
  return r;
}

Server *servers_db_get(char *name) {
  if (!servers[0]) {
    servers[0] = finanzas_mk();
    servers[1] = invertia_mk();
    servers[2] = infomercados_mk();
  }
  RANGE0(i, n_servers) {
    if (str_eq(server_name(servers[i]), name)) {
      return servers[i];
    }
  }_RANGE
  exc_illegal_state(str_printf("Server '%s' does not exist", name));
  // Unreacheble
  return NULL;
}
