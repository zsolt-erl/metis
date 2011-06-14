/* ************************************************************************

   Copyright:

   License:

   Authors:

************************************************************************ */

/* ************************************************************************

#asset(myapp/*)

************************************************************************ */

qx.Class.define("mtagui.table.RemoteDataModel",
{
    extend : qx.ui.table.model.Remote,
 
    members :
    {
	_COLUMN_ORDER : ["dmid","regexp","state","daily_limit","hourly_limit","max_conn","msg_per_conn",
			 "relay_type","c_open_conn","c200","c4xx","c5xx","host_msg_queue"],

	// overloaded - called whenever the table requests the row count
	_loadRowCount : function()
	{
	    // Call the backend service (example) - using XmlHttp 
	    var baseUrl  = 'http://'+window.location.host+'/services/getTableCount.yaws';
	    var dateformatter=new qx.util.format.DateFormat("dd-MMM-yyyy");

	    var url = baseUrl;
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
	    var baseUrl  = 'http://'+window.location.host+'/services/getTableRowData.yaws';

	    var req = new qx.io.remote.Request(baseUrl, "GET", "application/json");
	    req.setTimeout(15000);
	    this.debug("sent row data request");
	    
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