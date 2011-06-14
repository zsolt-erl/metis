#include <sys/types.h>
#include <unistd.h>
#include <erl_driver.h>
#include <ei.h>
#include <erl_interface.h>

#define CMD_SET_UID       1
#define CMD_SET_GID       2
#define CMD_SET_EUID      3
#define CMD_SET_EGID      4

#define CMD_GET_UID       51
#define CMD_GET_GID       52
#define CMD_GET_EUID      53
#define CMD_GET_EGID      54

#define CMD_FORMAT_ERRNO  100

typedef struct setuid_drv_t {
  ErlDrvPort      port;
  FILE            *log;
  ErlDrvTermData  ok_atom;
  ErlDrvTermData  error_atom;
} setuid_drv_t;

static ErlDrvData
start (ErlDrvPort port, char *cmd);

static void
stop (ErlDrvData drv);

static int
control (ErlDrvData drv,
  unsigned int command,
  char *buf,
  int len,
  char **rbuf,
  int rlen);

typedef uid_t (*uid_getter_t) ();
typedef gid_t (*gid_getter_t) ();

typedef int (*uid_setter_1_t) (uid_t);
typedef int (*gid_setter_1_t) (gid_t);

static void
get_uid (setuid_drv_t *drv, uid_getter_t getter);

static void
get_gid (setuid_drv_t *drv, gid_getter_t getter);

static void
set_uid (setuid_drv_t *drv, uid_setter_1_t setter, char *uid);

static void
set_gid (setuid_drv_t *drv, gid_setter_1_t setter, char *gid);

static void
format_errno (setuid_drv_t *drv, char *err);

