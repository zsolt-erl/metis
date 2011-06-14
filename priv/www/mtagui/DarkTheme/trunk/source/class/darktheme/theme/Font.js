/* ************************************************************************

   Copyright:
     2010 Norbert Schröder

   License:
     LGPL: http://www.gnu.org/licenses/lgpl.html
     EPL: http://www.eclipse.org/org/documents/epl-v10.php

   Authors:
     * Norbert Schröder (scro34)

************************************************************************ */

qx.Theme.define("darktheme.theme.Font",
{
  extend : qx.theme.modern.Font,

  fonts :
  {
    "default" :
    {
      size : (qx.bom.client.System.WINVISTA || qx.bom.client.System.WIN7) ? 12 : 11,
      lineHeight : 1.4,
      family : qx.bom.client.Platform.MAC ? [ "Lucida Grande" ] :
        (qx.bom.client.System.WINVISTA || qx.bom.client.System.WIN7) ?
        [ "Segoe UI", "Candara" ] :
		[ "Verdana", "Lucida Sans", "Tahoma", "Liberation Sans", "Arial", "sans-serif" ]
    }
  }
});