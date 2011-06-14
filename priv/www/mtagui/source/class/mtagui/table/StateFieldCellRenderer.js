qx.Class.define("mtagui.table.StateFieldCellRenderer",
{
    extend : qx.ui.table.cellrenderer.Default,

    members :
    {
	_getCellStyle : function(cellInfo)
	{
	    var value = cellInfo.value;
	    var row = cellInfo.row;
	    var style="font-weight:bold;";
	    var backgroundStyle;

	    if (row%2 == 0) {
		backgroundStyle="background-color: #f3f3f3;";
	    } else {
		backgroundStyle="background-color: #e4e4e4;";
	    }

	    if (value && value.search("deliver")>-1)  {
		style+="font-size:14px; color:green;";
	    } else {
		style+="font-size:14px; color:red;";
	    }
	    return this.base(arguments, cellInfo) + style + backgroundStyle;
	},

	_getContentHtml : function(cellInfo)
	{
	    return cellInfo.value;
	}
    }
});