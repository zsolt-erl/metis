#include "setuid_drv.h"
#include <stdio.h>
#include <string.h>

static ErlDrvEntry driver_entry__ = {
  NULL,                             /* init */
  start,                            /* startup (defined below) */
  stop,                             /* shutdown (defined below) */
  NULL,                             /* output */
  NULL,                             /* ready_input */
  NULL,                             /* ready_output */
  "setuid_drv",                     /* the name of the driver */
  NULL,                             /* finish */
  NULL,                             /* handle */
  control,                          /* control */
  NULL,                             /* timeout */
  NULL,                             /* outputv (defined below) */
  NULL,                             /* ready_async */
  NULL,                             /* flush */
  NULL,                             /* call */
  NULL,                             /* event */
  ERL_DRV_EXTENDED_MARKER,          /* ERL_DRV_EXTENDED_MARKER */
  ERL_DRV_EXTENDED_MAJOR_VERSION,   /* ERL_DRV_EXTENDED_MAJOR_VERSION */
  ERL_DRV_EXTENDED_MAJOR_VERSION,   /* ERL_DRV_EXTENDED_MINOR_VERSION */
  ERL_DRV_FLAG_USE_PORT_LOCKING,    /* ERL_DRV_FLAGs */
  NULL,                             /* handle2 */
  NULL,                             /* process_exit */
  NULL                              /* stop_select */
};

DRIVER_INIT (setuid_driver)
{
  return &driver_entry__;
}

static ErlDrvData
start (ErlDrvPort port, char *cmd)
{
  FILE *log = stdout;

  /*
    fopen ("priv/logs/erlang-setuid-drv.log", "a+");
    if (!log)
    {
      fprintf (stderr, "Couldn't create log file\n");
      fflush (stderr);
      return (ErlDrvData) -1;
    }

  */

  setuid_drv_t *drv = (setuid_drv_t *)driver_alloc (sizeof (setuid_drv_t));
  if (!drv)
    {
      fprintf (log, "Couldn't allocate memory for driver\n");
      fflush (log);
      fclose (log);

      return (ErlDrvData) -1;
    }

  drv->port       = port;
  drv->log        = log;
  drv->ok_atom    = driver_mk_atom ("ok");
  drv->error_atom = driver_mk_atom ("error");

  fprintf (drv->log, "Start setuid driver\n");
  fflush (drv->log);

  return (ErlDrvData) drv;
}

static void
stop (ErlDrvData p)
{
  setuid_drv_t *drv = (setuid_drv_t *)p;

  fprintf (drv->log, "Stop setuid driver\n");
  fflush (drv->log);
  fclose (drv->log);

  drv->log = 0;

  driver_free (drv);
}

static int
control (ErlDrvData p,
         unsigned int command,
         char *buf,
         int len,
         char **rbuf,
         int rlen)
{
  setuid_drv_t *drv = (setuid_drv_t *)p;
  if (len)
    buf[len] = 0;

  switch (command)
    {
    case CMD_SET_UID:
      set_uid (drv, setuid, buf);
      break;
    case CMD_SET_GID:
      set_gid (drv, setgid, buf);
      break;
    case CMD_SET_EUID:
      set_uid (drv, seteuid, buf);
      break;
    case CMD_SET_EGID:
      set_gid (drv, setegid, buf);
      break;
    case CMD_GET_UID:
      get_uid (drv, getuid);
      break;
    case CMD_GET_GID:
      get_gid (drv, getgid);
      break;
    case CMD_GET_EUID:
      get_uid (drv, geteuid);
      break;
    case CMD_GET_EGID:
      get_gid (drv, getegid);
      break;
    case CMD_FORMAT_ERRNO:
      format_errno (drv, buf);
      break;
    }

  return 0;
}

static void
send_ok (setuid_drv_t *drv)
{
  ErlDrvTermData result [] = {
      ERL_DRV_ATOM, drv->ok_atom,
      ERL_DRV_TUPLE, 1
  };

  driver_output_term (drv->port,
                      result,
                      sizeof (result) / sizeof (result[0]));
}

static void
send_errno (setuid_drv_t *drv)
{
  ErlDrvTermData result[] = {
      ERL_DRV_ATOM, drv->error_atom,
      ERL_DRV_UINT, errno,
      ERL_DRV_TUPLE, 2
  };

  driver_output_term (drv->port,
                      result,
                      sizeof (result) / sizeof (result[0]));
}

static void
set_uid (setuid_drv_t *drv, uid_setter_1_t setter, char *cmd)
{
  uid_t uid = (uid_t) atoi (cmd);

  if (!(*setter) (uid))
    {
      send_ok (drv);
    }
  else
    {
      send_errno (drv);
    }
}

static void
set_gid (setuid_drv_t *drv, gid_setter_1_t setter, char *cmd)
{
  gid_t gid = (gid_t) atoi (cmd);

  if (!(*setter) (gid))
    {
      send_ok (drv);
    }
  else
    {
      send_errno (drv);
    }
}

static void
get_uid (setuid_drv_t *drv, uid_getter_t getter)
{
  ErlDrvTermData result [] = {
      ERL_DRV_ATOM, drv->ok_atom,
      ERL_DRV_UINT, (*getter) (),
      ERL_DRV_TUPLE, 2
  };

  driver_output_term (drv->port,
                      result,
                      sizeof (result) / sizeof (result[0]));
}

static void
get_gid (setuid_drv_t *drv, uid_getter_t getter)
{
  ErlDrvTermData result [] = {
      ERL_DRV_ATOM, drv->ok_atom,
      ERL_DRV_UINT, (*getter) (),
      ERL_DRV_TUPLE, 2
  };

  driver_output_term (drv->port,
                      result,
                      sizeof (result) / sizeof (result[0]));
}

static void
format_errno (setuid_drv_t *drv, char *buf)
{
  char *msg = strerror (atoi (buf));
  size_t len = strlen (msg);

  ErlDrvTermData result [] = {
      ERL_DRV_STRING, (ErlDrvTermData)msg, len
  };

  driver_output_term (drv->port, 
                      result,
                      sizeof (result) / sizeof (result[0]));
}
