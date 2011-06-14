/* ************************************************************************

   Copyright:

   License:

   Authors:

************************************************************************ */

/* ************************************************************************

#asset(mtagui/*)

************************************************************************ */

qx.Class.define("mtagui.table.RiakLogAnalyzerRemoteDataModel",
{
    extend : qx.ui.table.model.Remote,
 
    members :
    {
	_COLUMN_ORDER : ["id", "time", "node", "type", "from", "receiver", "subject"],
	startdate: new Date(),
	starttime:"",
	enddate: new Date(),
	endtime:"",
	// filters
	fnode:"", 
	ftype:"",
	ffrom:"",
	frecv:"",
	fsubject:"",

	// overloaded - called whenever the table requests the row count
	_loadRowCount : function()
	{
	    // Call the backend service (example) - using XmlHttp 
	    var baseUrl  = '/services/LogAnalyzerRowCount.yaws';
	    var dateformatter=new qx.util.format.DateFormat("EEE, dd MMM y");
	    var parameters = "?startdate=" + dateformatter.format( this.startdate );
	    parameters += "&starttime=" + this.starttime;
	    parameters += "&enddate="   + dateformatter.format( this.enddate );
	    parameters += "&endtime="   + this.endtime;
	    parameters += "&fnode="     + this.fnode;
	    parameters += "&ftype="     + this.ftype;
	    parameters += "&ffrom="     + this.ffrom;
	    parameters += "&frecv="     + this.frecv;
	    parameters += "&fsubject="  + this.fsubject;

	    var url = baseUrl + parameters;
	    var req = new qx.io.remote.Request(url, "GET", "application/json");
	    req.setTimeout(15000);
	    
	    // Add listener
	    req.addListener("completed", this._onRowCountCompleted, this);
	    req.addListener("timeout", function() {alert("Request timed out.");});
	    
	    // send request
	    req.send();
	},
	
	// Listener for request of "_loadRowCount" method
	_onRowCountCompleted : function(response)
	{
	    var result = response.getContent();
	    if (result != null)
	    {
		// Apply it to the model - the method "_onRowCountLoaded" has to be called
		this._onRowCountLoaded(result);
	    }
	},
	
	
	// overloaded - called whenever the table requests new data
	_loadRowData : function(firstRow, lastRow)
	{
	    // Call the backend service (example) - using XmlHttp 
	    var baseUrl  = '/services/LogAnalyzerRowData.yaws';
	    var dateformatter=new qx.util.format.DateFormat("EEE, dd MMM y");
	    var parameters = "?startdate=" + dateformatter.format( this.startdate );
	    parameters += "&starttime=" + this.starttime;
	    parameters += "&enddate="   + dateformatter.format( this.enddate );
	    parameters += "&endtime="   + this.endtime;
	    parameters += "&fnode="     + this.fnode;
	    parameters += "&ftype="     + this.ftype;
	    parameters += "&ffrom="     + this.ffrom;
	    parameters += "&frecv="     + this.frecv;
	    parameters += "&fsubject="  + this.fsubject;
	    parameters += "&firstrow="  + firstRow;
	    parameters += "&lastrow="   + lastRow;

	    var url = baseUrl + parameters;
	    var req = new qx.io.remote.Request(url, "GET", "application/json");
	    req.setTimeout(15000);
	    
	    // Add listener
	    req.addListener("completed", this._onLoadRowDataCompleted, this);      
	    req.addListener("timeout", function() {alert("Request timed out.");});
	    
	    // send request
	    req.send();
	},
	
	// Listener for request of "_loadRowData" method
	_onLoadRowDataCompleted : function(response)
	{
            var result = response.getContent();
	    if (result != null)
	    {
		// Apply it to the model - the method "_onRowDataLoaded" has to be called
		this._onRowDataLoaded(result);   
	    }        
	}
    }
});