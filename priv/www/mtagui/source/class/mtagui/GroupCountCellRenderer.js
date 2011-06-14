qx.Class.define("mtagui.GroupCountCellRenderer",
{
    extend : qx.ui.table.cellrenderer.Default,
  
    members :
    {
        _getCellStyle : function(cellInfo)
        {
	    var colors=["lightblue","red","green","yellow","orange"];

            var value = cellInfo.value;
            var row = cellInfo.row;
            var style="font-weight:bold; font-size:14px; color: black;";
            var backgroundStyle;

	    if (row<5) {
                backgroundStyle="background-color:"+colors[row]+";";
	    } else if (row%2 == 0) {
                backgroundStyle="background-color: white;";
            } else {
                backgroundStyle="background-color: #dddddd;";
            }

            return this.base(arguments, cellInfo) + style + backgroundStyle;
        },
        
        _getContentHtml : function(cellInfo)
        {
            return cellInfo.value;
        }
    }
});
