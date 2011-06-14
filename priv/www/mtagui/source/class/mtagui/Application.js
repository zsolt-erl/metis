/* ************************************************************************

   Copyright:

   License:

   Authors:

************************************************************************ */

/* ************************************************************************

#asset(mtagui/*)
#asset(qx/*)

************************************************************************ */

/**
 * This is the main application class of your custom application "mtagui"
 */
qx.Class.define("mtagui.Application",
{
  extend : qx.application.Standalone,

  /*
  *****************************************************************************
     MEMBERS
  *****************************************************************************
  */

  members :
  {
      widgets: {domaintable : {}},
      config : new Object(),
      

      /**
       * This method contains the initial application code and gets called
       * during startup of the application
       *
       * @lint ignoreDeprecated(alert)
       */
      main : function()
      {
	  // Call super class
	  this.base(arguments);
	  
	  // Enable logging in debug variant
	  if (qx.core.Variant.isSet("qx.debug", "on"))
	  {
              // support native logging capabilities, e.g. Firebug for Firefox
              qx.log.appender.Native;
              // support additional cross-browser console. Press F7 to toggle visibility
              qx.log.appender.Console;
	  }

	  /*
	   -------------------------------------------------------------------------
           Below is your actual application code...
	   -------------------------------------------------------------------------
	  */

	  var root=this.getRoot();
	  var widgets=this.widgets;

	  this.loadConfig(this);

	  // set up background
	  var decorator=new qx.ui.decoration.Single();
	  decorator.setBackgroundImage("mtagui/bl1280.jpg");
	  root.set({decorator:decorator});

	  var main_cont = new qx.ui.container.Composite();
	  var l=new qx.ui.layout.VBox();
	  main_cont.setLayout(l);

	  var title_cont = new qx.ui.container.Composite();
	  title_cont.set(
	      {
		  layout          : new qx.ui.layout.VBox(5),
		  width: 1280,
		  font            : new qx.bom.Font(16, ["Verdana", "sans-serif"]).set( {bold:true} )
	      });

	  var menubar =new qx.ui.toolbar.ToolBar();
	  var toolsMenu=new qx.ui.toolbar.MenuButton("Tools");

	  menubar.add( new qx.ui.basic.Label("Metis v0.1").set({padding: 7}) );
	  menubar.add( new qx.ui.toolbar.Separator() );
	  menubar.add( toolsMenu );
	  
	  toolsMenu.setMenu( this.getToolsMenu() );
	  title_cont.add( menubar );

	  var display_cont = new qx.ui.container.Composite();
	  display_cont.set(
	    {
	      layout : new qx.ui.layout.Grid(10, 10),
	      padding : 20
	    });

	  root.add(main_cont);
	  main_cont.add(title_cont);
	  main_cont.add(display_cont);

	  var receivedWidget=this.makeTile("Received:", "");
	  display_cont.add( receivedWidget.contMain, {row:0, column:0});

	  var sentWidget=this.makeTile("Sent:", "");
	  display_cont.add( sentWidget.contMain, {row:0, column:1} );

	  var queueWidget=this.makeTile("Queue:", "");
	  display_cont.add( queueWidget.contMain, {row:0, column:2} );

	  var otherStatsWidget=this.makeTile("Other Stats", "", 350);
	  display_cont.add( otherStatsWidget.contMain, {row:0, column:3} );

	  this.widgets.received=receivedWidget;
	  this.widgets.sent=sentWidget;
	  this.widgets.queue=queueWidget;
	  this.widgets.otherStats=otherStatsWidget;

	  otherStatsWidget.contDisplay.add( new qx.ui.basic.Label("Deleted:"), {left:10, top:20});
	  this.widgets.lblDeletedCounter=new qx.ui.basic.Label("0").set({width:80, textAlign:"right"});
	  otherStatsWidget.contDisplay.add( this.widgets.lblDeletedCounter, {left: 150, top:20});

	  otherStatsWidget.contDisplay.add( new qx.ui.basic.Label("Softbounce:"), {left:10, top:50});
	  this.widgets.lblSoftbounceCounter=new qx.ui.basic.Label("127").set({width:80, textAlign:"right"});
	  otherStatsWidget.contDisplay.add( this.widgets.lblSoftbounceCounter, {left: 150, top:50});

	  otherStatsWidget.contDisplay.add( new qx.ui.basic.Label("Hardbounce:"), {left:10, top:80});
	  this.widgets.lblHardbounceCounter=new qx.ui.basic.Label("342439").set({width:80, textAlign:"right"});
	  otherStatsWidget.contDisplay.add( this.widgets.lblHardbounceCounter, {left: 150, top:80});

	  var receivedChart=this.makeChart();
	  receivedWidget.contDisplay.add(receivedChart.chart);
	  var sentChart=this.makeChart();
	  sentWidget.contDisplay.add(sentChart.chart);
	  var queueChart=this.makeChart();
	  queueWidget.contDisplay.add(queueChart.chart);

	  //var table=this.widgets.table=this.createTable();
	  //this.debug("table created");
	  //display_cont.add( table, {row:1, column:0, colSpan:4} );

	  var NodeDisplay=this.createNodeDisplay(this.clients);
	  this.widgets.nodedisplay=NodeDisplay;
	  this.debug({"Nodes":NodeDisplay});
	  
	  display_cont.add( NodeDisplay.container, {row:1, column:0, colSpan:4});

	  var domaintable=this.widgets.domaintable=this.createDomainTable();
	  root.add(domaintable.window);

	  var qbreakdown=this.widgets.qbreakdown=this.createQueueBreakdown();
	  root.add(qbreakdown.window);

	  var logviewer=this.widgets.logviewer=this.createLogViewer();
	  root.add(logviewer.window);
	  
	  var LogAnalyzer=this.widgets.loganalyzer=this.createLogAnalyzer();
	  root.add(LogAnalyzer.window);

	  loop(this);
	  var interval = window.setInterval( loop, this.config.main_loop_delay*1000, this );
	  if (this.config.main_loop_exit) 
	      {
		  window.setTimeout(function() {clearInterval(interval);}, this.config.main_loop_exit*1000);		  
	      }

	  function loop(parent)
	  {
	      var url  = '/services/getPTCounts.yaws';
	      var req = new qx.io.remote.Request(url, "GET", "application/json");
	      req.setAsynchronous(false);
	      req.setTimeout(10*60*1000);
	      req.addListener("completed", function(event){
				  var response=event.getContent();
				  parent.widgets.received.lblCounter.setValue( response.counts.queued.toString() );
				  parent.widgets.sent.lblCounter.setValue( response.counts.sent.toString() );
				  parent.widgets.queue.lblCounter.setValue( response.counts.ptqueue.toString() );

				  parent.widgets.lblDeletedCounter.setValue( response.counts.deleted.toString() );
				  parent.widgets.lblSoftbounceCounter.setValue( response.counts.softbounce.toString() );
				  parent.widgets.lblHardbounceCounter.setValue( response.counts.hardbounce.toString() );

				  receivedChart.redraw(response.timelines.rcvdtl);
				  sentChart.redraw(response.timelines.senttl);
				  queueChart.redraw(response.timelines.queuetl);
				  parent.refreshNodeDisplay( parent.widgets.nodedisplay );
				  //				  parent.refreshTable();

			      });
	      req.send();
	  }
      },

      makeTile: function(title, counter_val, W, H)
      {
	  var width = 280;
	  var height = 200;

	  if (W) width=W;
	  if (H) height=H;

	  var decorator=new qx.ui.decoration.Single(1).set(
	      {
		  colorTop    : "#21344f",
		  colorLeft   : "#aaaaaa",
		  colorRight  : "#aaaaaa",
		  colorBottom : "#aaaaaa"
	      });

	  var container = new qx.ui.container.Composite();
	  container.set(
	      {
		  layout: new qx.ui.layout.VBox(),
		  width: width,
		  height: height,
		  allowStretchX: false,
		  allowStretchY: false,
		  decorator : decorator
	      });

	  var title_cont = new qx.ui.container.Composite();
	  title_cont.set(
	      {
		  layout:          new qx.ui.layout.HBox(5),
		  width:           width,
		  height:          35,
		  padding:         5,
		  backgroundColor: "#21344f",
		  textColor:       "#ffffff",
		  font: new qx.bom.Font(18, ["Verdana", "sans-serif"]).set( {bold:true} )
	      });

	  var title_label_name=new qx.ui.basic.Label(title);
	  var title_label_value=new qx.ui.basic.Label(counter_val);

	  title_cont.add( title_label_name );
	  title_cont.add( title_label_value );

	  var display_cont = new qx.ui.container.Composite().set(
	      {
		  textColor: "#111188",
		  height: height-35,
		  font: new qx.bom.Font(16, ["Verdana", "sans-serif"]).set( {bold:true} )
	      });
	  display_cont.setLayout(new qx.ui.layout.Canvas());
	  var display_background=new qx.ui.core.Widget().set(
	      {
		  width: width,
		  height: height-35,
		  backgroundColor: "#ffffff",
		  opacity : 0.6
	      });
	  display_cont.add( display_background, {width: "100%", height:"100%"});

	  container.add(title_cont);
	  container.add(display_cont);

	  return ({contMain: container, lblCounter: title_label_value, contDisplay: display_cont});
      },

      makeChart: function(){

	  var th=this;

          var chart = new qxprotovis.Panel().set({width:260, height:160});

          var pv = chart.getPv();
          var data = pv.range(60).map(function(x) {return Math.random();});
//	  this.debug(data);

          var vis = chart.getPanel()
              .bottom(5)
              .left(50)
              .right(0)
              .top(5);

          var x = pv.Scale.linear(0, 59).range(0, 205);
          var y = pv.Scale.linear(0, 1.0).range(0, 150);

	  // X axis
	  vis.add(pv.Rule)
	      .bottom(0)
	      .lineWidth(2)
	      .strokeStyle("#000");

          // Y-axis and ticks.
	  vis.add(pv.Rule)
	      .left(0)
	      .lineWidth(1)
	      .strokeStyle("#000");

          vis.add(pv.Rule)
              .data(function(){return y.ticks();})
              .bottom(y)
              .left(-4)
              .right(-4)
              .strokeStyle(function(d){return d ? "rgba(128,128,128,.2)" : "#000";})
              .anchor("left").add(pv.Label)
              .text(y.tickFormat);

	  var bar = vis.add(pv.Line)
	      .data(data)
	      .left( function() {return x(this.index);})
	      .bottom( function(d) {return y(d);} )
	      .lineWidth(2);

          vis.render();

	  var redraw=function(newData)
	  {
	      var min=pv.min(newData)-5;
	      if (min<0) {min=0;}
	      var max=pv.max(newData)+5;

	      y.domain(min, max);
	      bar.data(newData);
	      vis.render();
	  };

	  return {chart: chart, data: data, vis:vis, bar:bar, redraw: redraw};
      },

      createTable : function(){
	  var tableModel = new qx.ui.table.model.Simple();

	  tableModel.setColumns([ "ID", "Domain", "State", "Limit(D)", "Limit(Hr)", "MaxConn", "Msg/Conn", "Type",
	  "OpenConn", "Sent(Hr,D,T)", "SoftBounce(Hr,D,T)", "HardBounce(Hr,D,T)", "MsgQueue"],
	  ["dmid","regexp","state","daily_limit","hourly_limit","max_conn","msg_per_conn",
	  "relay_type","c_open_conn","c200","c4xx","c5xx","host_msg_queue"]);

	  tableModel.setEditable(false);
	  tableModel.sortByColumn(1, true);
	  var table = new qx.ui.table.Table(tableModel);
			
	  var colModel=table.getTableColumnModel();
	  colModel.setColumnWidth(0,50);
	  colModel.setColumnWidth(1,100);
	  colModel.setColumnWidth(2,60);
	  colModel.setColumnWidth(3,120);
	  colModel.setColumnWidth(4,120);
	  colModel.setColumnWidth(5,60);
	  colModel.setColumnWidth(6,70);
	  colModel.setColumnWidth(7,60);
	  colModel.setColumnWidth(8,70);
	  colModel.setColumnWidth(9,120);
	  colModel.setColumnWidth(10,120);
	  colModel.setColumnWidth(11,120);
	  colModel.setColumnWidth(12,60);

	  var stateFieldRenderer = new mtagui.table.StateFieldCellRenderer();
	  colModel.setDataCellRenderer(2, stateFieldRenderer);

	  var tcm = table.getTableColumnModel();
	  tcm.setCellEditorFactory(12, new mtagui.table.ModalCellEditorFactory());
	  tableModel.setColumnEditable(12, true);

	  return table;
      },

      refreshTable : function()
      {
	  this.debug("refreshTable");
	  var baseUrl  = 'http://'+window.location.host+'/services/getTableRowData.yaws';
	  var url = baseUrl;
	  var req = new qx.io.remote.Request(url, "GET", "application/json");
	  req.setTimeout(15000);
	  var table=this.widgets.table;
	  var loadedFun=function(response){
	      table.getTableModel().setRowsAsMapArray(response.getContent());
	  };

	  req.addListener("completed", loadedFun, this);      
	  req.addListener("timeout", function() {alert("Request timed out.");});
	  req.send();
      },
      
      getToolsMenu : function()
      {
	  var menu = new qx.ui.menu.Menu;
	  var maillogButton = new qx.ui.menu.Button("Mail Log Cache");
	  var analyzeButton = new qx.ui.menu.Button("Analyze");

	  maillogButton.addListener("execute", this.openLogViewer, this);
	  analyzeButton.addListener("execute", this.openLogAnalyzer, this);

	  menu.add(maillogButton);
	  menu.add(analyzeButton);
	  return menu;
      },
      
      //--------------------Domain Table--------------------------------------------------------------------------
      createDomainTable : function(){
	  var tableModel = new qx.ui.table.model.Simple();

	  tableModel.setColumns([ "ID", "Domain", "State", "Limit(D)", "Limit(Hr)", "MaxConn", "Msg/Conn", "Type",
				  "OpenConn", "Sent(Hr,D,T)", "SoftBounce(Hr,D,T)", "HardBounce(Hr,D,T)", "MsgQueue"],
				["dmid","regexp","state","daily_limit","hourly_limit","max_conn","msg_per_conn",
				 "relay_type","c_open_conn","c200","c4xx","c5xx","host_msg_queue"]);

	  tableModel.setEditable(false);
	  tableModel.sortByColumn(1, true);
	  var table = new qx.ui.table.Table(tableModel);
			
	  var colModel=table.getTableColumnModel();
	  colModel.setColumnWidth(0,50);
	  colModel.setColumnWidth(1,100);
	  colModel.setColumnWidth(2,60);
	  colModel.setColumnWidth(3,120);
	  colModel.setColumnWidth(4,120);
	  colModel.setColumnWidth(5,60);
	  colModel.setColumnWidth(6,70);
	  colModel.setColumnWidth(7,60);
	  colModel.setColumnWidth(8,70);
	  colModel.setColumnWidth(9,120);
	  colModel.setColumnWidth(10,120);
	  colModel.setColumnWidth(11,120);
	  colModel.setColumnWidth(12,60);

	  var stateFieldRenderer = new mtagui.table.StateFieldCellRenderer();
	  colModel.setDataCellRenderer(2, stateFieldRenderer);

	  var tcm = table.getTableColumnModel();
	  tcm.setCellEditorFactory(12, new mtagui.table.ModalCellEditorFactory());
	  tableModel.setColumnEditable(12, true);

	  var window = new qx.ui.window.Window("Domain Control Table");
	  window.setLayout(new qx.ui.layout.HBox(4));
	  window.set(
	  { 
	      padding: 3,
	      showMaximize: false,
	      showMinimize: false,
	      width: 930,
	      height: 400
	  });
	  window.moveTo(100, 100);
	  
	  window.add( table );
	  
	  window.addListener("close", this.closeDomainTable, this);
	  
	  return {window: window, table: table};
      },

      openDomainTable : function()
      {
	  this.parent.widgets.domaintable.node=this.nodename;
	  this.parent.widgets.domaintable.interval = window.setInterval( this.parent.loopDomainTable, 
						  this.parent.config.main_loop_delay*1000, this.parent );
	  var Window=this.parent.widgets.domaintable.window;
	  Window.setCaption( this.nodename );
	  this.parent.loopDomainTable(this.parent);
	  Window.open();
      },

      closeDomainTable : function()
      {
	  clearInterval( this.widgets.domaintable.interval );
      },

      loopDomainTable : function(parent)
      {
	  // alert(parent);alert(this);
	  var Node = parent.widgets.domaintable.node;
//	  var Node = mtagui.Application.widgets.domaintable.node;	  
	  var url = '/services/getDomainTableData.yaws?node='+Node;
	  var req = new qx.io.remote.Request(url, "GET", "application/json");
	  req.setTimeout(15000);

	  var table=parent.widgets.domaintable.table;
	  var loadedFun=function(response){
	      table.getTableModel().setRowsAsMapArray(response.getContent());
	  };

	  req.addListener("completed", loadedFun, this);      
	  req.addListener("timeout", function() {alert("Request timed out.");});
	  req.send();
      },
      //END-----------------Domain Table--------------------------------------------------------------------------

      //--------------------Queue Breakdown-----------------------------------------------------------------------
      openQueueBreakdown : function()
      {
	  this.widgets.qbreakdown.interval = 
	      window.setInterval( this.loopQueueBreakdown, this.config.breakdown_loop_delay*1000, this );
	  this.widgets.qbreakdown.window.open();
      },

      closeQueueBreakdown : function()
      {
	  clearInterval( this.widgets.qbreakdown.interval );
      },
      
      loopQueueBreakdown : function(parent)
      {
//	  parent.debug('queue loop');
	  var url = '/services/getQueueData.yaws';
	  var req = new qx.io.remote.Request(url, "GET", "application/json");
	  req.setTimeout(15000);
	  var sTable=parent.widgets.qbreakdown.subjectTable;
	  var tTable=parent.widgets.qbreakdown.tryTable;
	  var dTable=parent.widgets.qbreakdown.domainTable;

	  var loadedFun=function(response){
	      // update subject distribution table
	      var sModel=sTable.getTableModel();
	      var sDistrib=response.getContent().subject_distrib;
	      var sDiff=sModel.getRowCount()-sDistrib.length;
	      if (sDiff>0)
		  {
		      sModel.removeRows(0, sDiff);
		  }
	      sModel.setRowsAsMapArray(sDistrib);
	      
	      // update try count distribution table
	      var tModel=tTable.getTableModel();
	      var tDistrib=response.getContent().trycount_distrib;
	      var tRowCount=tModel.getRowCount();
	      var tDiff=tRowCount-tDistrib.length;
	      if (tDiff>0)
		  {
		      tModel.removeRows(0, tDiff);
		  }
	      tModel.setRowsAsMapArray(tDistrib);

	      // update domain distribution table
	      var dModel=dTable.getTableModel();
	      var dDistrib=response.getContent().domain_distrib;
	      var dDiff=dModel.getRowCount()-dDistrib.length;
	      if (dDiff>0) { dModel.removeRows(0, dDiff); }
	      dModel.setRowsAsMapArray(dDistrib);
	  };

	  req.addListener("completed", loadedFun, this);      
	  req.addListener("timeout", function() {alert("Request timed out.");});
	  req.send();
      },
      
      createQueueBreakdown : function()
      {
	  var subjectTableModel = new qx.ui.table.model.Simple();
	  subjectTableModel.setColumns(["Subject", "Amount"], ["subject","amount"]);
	  subjectTableModel.setEditable(false);
	  subjectTableModel.sortByColumn(1, true);
	  var subjectTable = new qx.ui.table.Table(subjectTableModel);
	  subjectTable.setWidth(480);
	  var stColModel=subjectTable.getTableColumnModel();
	  stColModel.setColumnWidth(0,400);
	  stColModel.setColumnWidth(1,70);

	  var tryTableModel = new qx.ui.table.model.Simple();
	  tryTableModel.setColumns(["Try Count", "Amount"], ["trycount","amount"]);
	  tryTableModel.setEditable(false);
	  tryTableModel.sortByColumn(1, true);
	  var tryTable = new qx.ui.table.Table(tryTableModel);
	  tryTable.setWidth(150);
	  var ttColModel=tryTable.getTableColumnModel();
	  ttColModel.setColumnWidth(0,70);
	  ttColModel.setColumnWidth(1,70);

	  var domainTableModel = new qx.ui.table.model.Simple();
	  domainTableModel.setColumns(["Domain", "Amount"], ["domain","amount"]);
	  domainTableModel.setEditable(false);
	  domainTableModel.sortByColumn(1, true);
	  var domainTable = new qx.ui.table.Table(domainTableModel);
	  domainTable.setWidth(280);
	  var dtColModel=domainTable.getTableColumnModel();
	  dtColModel.setColumnWidth(0,200);
	  dtColModel.setColumnWidth(1,70);
	  
	  var window = new qx.ui.window.Window("Queue Breakdown");
	  window.setLayout(new qx.ui.layout.HBox(4));
	  window.set(
	  { 
	      padding: 3,
	      showMaximize: false,
	      showMinimize: false,
	      width: 930,
	      height: 400
	  });
	  window.moveTo(100, 100);
	  
	  window.add( subjectTable );
	  window.add( tryTable );
	  window.add( domainTable );
	  
	  window.addListener("close", this.closeQueueBreakdown, this);
	  
	  return {window: window, subjectTable: subjectTable, tryTable: tryTable, domainTable: domainTable};
      },
      //END-----------------Queue Breakdown-----------------------------------------------------------------------



      //--------------------Node Display--------------------------------------------------------------------------


      // needs to return an object
      // widgets array is indexed with the name of the client
      // return:
      //	  {
      //	      container : Container,
      //	      widgets: array[
      //		  {
      //		      queue_label,
      //		      queue_bar,
      //		      load_label,
      //		      load_bar
      //		  }
      //	      ]
      //	  }
      createNodeDisplay : function(Clients) {
	  var Return = {};
	  Return.widgets = new Array();
	  
	  var Font= new qx.bom.Font(12, ["Verdana", "sans-serif"]).set( {bold:true} );
	  var ScrollPane = new qx.ui.container.Scroll();
	  var Container = new qx.ui.container.Composite( new qx.ui.layout.VBox(5) );
	  var MaxLen = 1;
	  for (var i in Clients){
	      if (Clients[i].queue_len>MaxLen) { MaxLen=Clients[i].queue_len; }
	  };
	  // this is the max number on the scale, if MaxLen=900  ScaleMax will be 1000
	  var ScaleMax=Math.pow(10, Math.floor( Math.log(MaxLen)/Math.LN10 )+1 );
	  
	  var Grid, NodeButton, NodeData, QueueLenLabel, BarContainer, QueueLenBar;
	  for (var i in Clients) {
	      Grid = new qx.ui.container.Composite( new qx.ui.layout.Grid(5,0) ).set(
	      {
		  backgroundColor: "#21344f",
		  font : Font,
		  padding: 5,
		  opacity: 0.8
	      });

	      // column 0
	      NodeButton = new qx.ui.toolbar.Button( Clients[i].name ).set(
		  {
		      minWidth: 250,
		      maxWidth: 250
		  });
	      NodeButton.setUserData("nodename", Clients[i].name);
	      NodeButton.addListener("execute", this.openDomainTable, 
				     {parent:this, nodename:NodeButton.getUserData("nodename")});
	      Grid.add(NodeButton, {column : 0, row: 0});
	      
	      // column 1
	      NodeData = new qx.ui.form.TextArea().set(
		  {
		      minWidth: 300,
		      maxWidth: 300,
		      readOnly: true,
		      value: "FROM: "+Clients[i].relay_from.join(", ")+"\nTO: "+Clients[i].relay_to.join(", ")
		  });
	      Grid.add(NodeData, {column: 1, row: 0, rowSpan: 2});
	      
	      // column 2,3
	      
	      // Queue length display
	      QueueLenLabel = new qx.ui.basic.Label("Queue:"+Clients[i].queue_len.toString()).set(
		  {
		      minWidth: 120,
		      maxWidth: 120,
		      height: 20
		  });
	      Grid.add(QueueLenLabel, {column: 2, row: 0});
	      
	      BarContainer = new qx.ui.container.Composite( new qx.ui.layout.VBox() ).set(
		  {
		      minWidth: 500,
		      maxWidth: 500,
		      height: 10
		  });
	      Grid.add(BarContainer, {column: 3, row: 0});
	      QueueLenBar = new qx.ui.core.Widget().set(
		  {
		      maxWidth: Math.round( (Clients[i].queue_len/ScaleMax)*500 ),
		      height: 5,
		      marginTop: 7,
		      backgroundColor: "white"
		  });
	      BarContainer.add(QueueLenBar);



	      // Load display
	      LoadLabel = new qx.ui.basic.Label("Load:"+Clients[i].load.toString()).set(
		  {
    		      height: 20
		  });
	      Grid.add(LoadLabel, {column: 2, row: 1});
	      
	      BarContainer2 = new qx.ui.container.Composite( new qx.ui.layout.VBox() ).set(
		  {
		      height: 10
		  });
	      Grid.add(BarContainer2, {column: 3, row: 1});
	      LoadBar = new qx.ui.core.Widget().set(
		  {
		      maxWidth: Math.round( (Clients[i].load/100)*500 ),
		      height: 5,
		      marginTop: 7,
		      backgroundColor: "orange"
		  });
	      BarContainer2.add(LoadBar);
	      
	      // add it to the main container
	      Container.add( Grid );

	      this.debug(Clients[i]);
	      Return.widgets[ Clients[i].name ]=
		  {
		      queue_label: QueueLenLabel,
		      queue_bar:   QueueLenBar,
		      load_label:  LoadLabel,
		      load_bar:    LoadBar
		  };
	  }
	  Return.container=Container;
	  this.debug({"Return":Return.container});
	  return Return;
      },
      
      refreshNodeDisplay : function(NodeDisplay)
      {
	  var url = '/services/getClientList.yaws';
	  var req = new qx.io.remote.Request(url, "GET", "application/json");
	  req.setAsynchronous(false);
	  req.setTimeout(15000);

	  var parent=this;
	  var loadedFun=function(response){
	      var Clients=response.getContent().clientlist;
	      var MaxLen = 1;
	      for (var i in Clients){
		  if (Clients[i].queue_len>MaxLen) { MaxLen=Clients[i].queue_len; }
	      };
	      // this is the max number on the scale, if MaxLen=900  ScaleMax will be 1000
	      var ScaleMax=Math.pow(10, Math.floor( Math.log(MaxLen)/Math.LN10 )+1 );

	      for (var i in Clients) {
		  ClientWidget=NodeDisplay.widgets[Clients[i].name],
		  ClientWidget.queue_label.setValue( "Queue:"+Clients[i].queue_len.toString() );
		  ClientWidget.queue_bar.setMaxWidth( Math.round( (Clients[i].queue_len/ScaleMax)*500 ) ),		  
		  ClientWidget.load_label.setValue( "Load:"+Clients[i].load.toString() );
		  ClientWidget.load_bar.setMaxWidth( Math.round( (Clients[i].load/100)*500 ) );
	      }
	  };

	  req.addListener("completed", loadedFun, this);      
	  req.addListener("timeout", function() {alert("Request timed out.");});
	  req.send();
      },
      //END-----------------Node Display--------------------------------------------------------------------------

      
      //--------------------Log Viewer----------------------------------------------------------------------------
      createLogViewer : function()
      {
	  var logviewTableModel = new qx.ui.table.model.Simple();
	  logviewTableModel.setColumns(["Time", "Node", "Type", "From", "Recv", "Subject", "Try Count"], 
				       ["time", "node", "type", "from", "recv", "subject", "try_count"]);
	  logviewTableModel.setEditable(false);

	  var logviewTable = new qx.ui.table.Table(logviewTableModel);
	  logviewTable.setWidth(1100);
	  var lvColModel=logviewTable.getTableColumnModel();
	  lvColModel.setColumnWidth(0,150);
	  lvColModel.setColumnWidth(1,150);
	  lvColModel.setColumnWidth(2,100);
	  lvColModel.setColumnWidth(3,150);
	  lvColModel.setColumnWidth(4,150);
	  lvColModel.setColumnWidth(5,275);
	  lvColModel.setColumnWidth(6,75);

	  var window = new qx.ui.window.Window("Log Cache Viewer");
	  window.setLayout(new qx.ui.layout.HBox(4));
	  window.set(
	  { 
	      padding: 3,
	      showMaximize: false,
	      showMinimize: false,
	      height: 400
	  });
	  window.moveTo(100, 100);
	  
	  window.add( logviewTable );
	  
	  window.addListener("close", this.closeLogViewer, this);
	  
	  return {window: window, logviewTable: logviewTable};
      },

      openLogViewer : function()
      {
	  this.widgets.logviewer.interval = 
	      window.setInterval( this.refreshLogViewer, this.config.logviewer_loop_delay*1000, this );
	  this.widgets.logviewer.window.open();
	  this.refreshLogViewer(this);
      },

      closeLogViewer : function()
      {
	  clearInterval( this.widgets.logviewer.interval );
      },

      refreshLogViewer : function(parent)
      {
	  parent.debug('logview loop');
	  var url = '/services/getEventLog.yaws';
	  var req = new qx.io.remote.Request(url, "GET", "application/json");
	  req.setTimeout(15000);
	  var lvTable=parent.widgets.logviewer.logviewTable;

	  var loadedFun=function(response){
	      var lvModel=lvTable.getTableModel();
	      var lvData=response.getContent();
	      parent.debug(lvModel.getRowCount());
	      parent.debug(lvData.length);
	      var Diff=lvModel.getRowCount()-lvData.length;
	      if (Diff>0) 
	      { 
		  lvModel.removeRows(0, Diff);
		  parent.debug(Diff);
	      }
	      lvModel.setRowsAsMapArray(lvData);
	  };

	  req.addListener("completed", loadedFun, this);      
	  req.addListener("timeout", function() {alert("Request timed out.");});
	  req.send();
      },
      //END-----------------Log Viewer----------------------------------------------------------------------------


      //--------------------Log Analyzer--------------------------------------------------------------------------
      createLogAnalyzer : function(){
	  var logFont   = new qx.bom.Font(12, ["Verdana", "sans-serif"]).set( {bold:true} );
	  var groupFont = new qx.bom.Font(14, ["Verdana", "sans-serif"]).set( {bold:true} );	  
	  var LogAnalyzer = {};

	  // CREATE MAIN WINDOW 
	  
	  var main_container = new qx.ui.window.Window("Log Analyzer").set(
	      {
		  layout : new qx.ui.layout.Grid(),
		  width:200,
		  showClose : true,
		  showMaximize : true,
		  showMinimize : false,
		  allowMinimize : false
	      });

	  // --------------------filter options- --------------
	  var fo_layout=new qx.ui.layout.Grid(10,5);
	  var fo_decorator=new qx.ui.decoration.Beveled("#4E575E", "#93A5B2");
	  var fo_container = new qx.ui.container.Composite( fo_layout ).set(
	      {
		  padding: 0,
		  width : 1170,
		  maxHeight: 120,
		  allowGrowY: true,
		  font: logFont,
		  decorator: fo_decorator
	      });
	  main_container.add( fo_container, {row:0, column:0, colSpan:2} );
	  
	  // START DATE, TIME
	  fo_container.add( new qx.ui.basic.Label("Start").set({alignY: "middle"}), {row:0,column:1});
	  var dateStartDate = new qx.ui.form.DateField().set(
	      {
		  value: new Date(),
		  dateFormat: new qx.util.format.DateFormat("EEE, d MMM yyyy"),
		  width: 200,
		  maxHeight:50
	      });
	  dateStartDate.addListener("changeValue",function(event){ LogAnalyzerTableModel.startdate=event.getData();},this);

	  var txtStartTime = new qx.ui.form.TextField("00:00:00").set({height : 30});
	  txtStartTime.addListener("changeValue",function(event){ LogAnalyzerTableModel.starttime=event.getData(); },this);

	  fo_container.add(dateStartDate,{row:0,column:2});
	  fo_container.add(txtStartTime,{row:0,column:3});
	  
	  // END DATE, TIME
	  fo_container.add( new qx.ui.basic.Label("End").set({alignY: "middle"}), {row:0,column:4});
	  var dateEndDate = new qx.ui.form.DateField().set(
	      {value: new Date(),
	       dateFormat: new qx.util.format.DateFormat("EEE, d MMM yyyy"),
	       width: 200});
	  dateEndDate.addListener("changeValue",function(event){ LogAnalyzerTableModel.enddate=event.getData(); },this);

	  var txtEndTime = new qx.ui.form.TextField("00:00:00").set({height : 30});
	  txtEndTime.addListener("changeValue",function(event){ LogAnalyzerTableModel.endtime=event.getData(); },this);

	  fo_container.add(dateEndDate,{row:0,column:5});
	  fo_container.add(txtEndTime,{row:0,column:6});

	  // Reload Button
	  var btnReload = new qx.ui.form.Button("Reload","icon/16/actions/view-refresh.png");
	  btnReload.addListener("execute", function(){this.refreshLogAnalyzer(LogAnalyzer);}, this);

	  fo_container.add(btnReload, {row:0, column:7});

	  // Filter Row
	  var filter_row   = new qx.ui.container.Composite( new qx.ui.layout.HBox() );
	  var frLabel      = new qx.ui.basic.Label("Filters / Groups").set({width : 150, alignY : "middle"});
	  var frNode       = new qx.ui.form.TextField().set({width : 180});
	  var frType       = new qx.ui.form.TextField().set({width : 80});
	  var frFrom       = new qx.ui.form.TextField().set({width : 155});
	  var frRecv       = new qx.ui.form.TextField().set({width : 155});
	  var frSubject    = new qx.ui.form.TextField().set({width : 310});


	  frNode.addListener("changeValue", function() { LogAnalyzerTableModel.fnode=frNode.getValue();  }, this);
	  frType.addListener("changeValue", function() { LogAnalyzerTableModel.ftype=frType.getValue();  }, this);
	  frFrom.addListener("changeValue", function() { LogAnalyzerTableModel.ffrom=frFrom.getValue();  }, this);
	  frRecv.addListener("changeValue", function() { LogAnalyzerTableModel.frecv=frRecv.getValue();  }, this);
	  frSubject.addListener("changeValue", function() { LogAnalyzerTableModel.fsubject=frSubject.getValue();  }, this);

	  fo_container.add(filter_row, {row:1, column:0, colSpan:8});

	  // END-----------------filter options----------------


	  // Group Row
	  var group_row   = new qx.ui.container.Composite( new qx.ui.layout.HBox() );
	  var grLabel      = new qx.ui.basic.Label("Groups").set({width : 140, alignY : "middle"});
	  var grNode       = new qx.ui.form.CheckBox().set({width : 20});
	  var grType       = new qx.ui.form.CheckBox().set({width : 20});
	  var grFrom       = new qx.ui.form.CheckBox().set({width : 20});
	  var grRecv       = new qx.ui.form.CheckBox().set({width : 20});
	  var grSubject    = new qx.ui.form.CheckBox().set({width : 20});

	  filter_row.add( frLabel );

	  filter_row.add( frNode );
	  filter_row.add( grNode );

	  filter_row.add( frType );
	  filter_row.add( grType );

	  filter_row.add( frFrom );
	  filter_row.add( grFrom );

	  filter_row.add( frRecv );
	  filter_row.add( grRecv );

	  filter_row.add( frSubject );
	  filter_row.add( grSubject );
	  fo_container.add(group_row, {row:2, column:0, colSpan:8});

	  // END-----------------filter options----------------




	  // --------------------log analyzer table----------------
	  var LogAnalyzerTableModel = new mtagui.table.RiakLogAnalyzerRemoteDataModel();
	  LogAnalyzerTableModel.setColumns(["Time", "Node", "Type", "From", "Receiver", "Subject"],
					   ["time", "node", "type", "from", "receiver", "subject"]);
	  LogAnalyzerTableModel.setEditable(false);
	  LogAnalyzerTableModel.sortByColumn(0, true);
	  LogAnalyzerTableModel.startdate=dateStartDate.getValue();
	  LogAnalyzerTableModel.starttime=txtStartTime.getValue();
	  LogAnalyzerTableModel.enddate=dateEndDate.getValue();
	  LogAnalyzerTableModel.endtime=txtEndTime.getValue();
	  var LogAnalyzerTable = new qx.ui.table.Table(LogAnalyzerTableModel).set({height:300});
	  var colModel=LogAnalyzerTable.getTableColumnModel();
	  colModel.setColumnWidth(0,150);
	  colModel.setColumnWidth(1,200);
	  colModel.setColumnWidth(2,100);
	  colModel.setColumnWidth(3,175);
	  colModel.setColumnWidth(4,175);
	  colModel.setColumnWidth(5, 350);

	  main_container.add(LogAnalyzerTable, {row : 1, column : 0, colSpan: 2});
	  // END-----------------log analyzer table----------------

	  // --------------------group counts table----------------
	  var GroupCountsTableModel = new qx.ui.table.model.Simple();
	  GroupCountsTableModel.setColumns(["Group", "Count"], ["group", "sum"]);
	  GroupCountsTableModel.setEditable(false);
	  GroupCountsTableModel.sortByColumn(1, true);
	  var GroupCountsTable = new qx.ui.table.Table(GroupCountsTableModel).set(
	      {
		  height : 300, 
		  width  : 580,
		  font   : groupFont
	      });
	  var GCcolModel=GroupCountsTable.getTableColumnModel();
	  GCcolModel.setColumnWidth(0,350);
	  GCcolModel.setColumnWidth(1,100);
	  var Renderer=new mtagui.GroupCountCellRenderer();
	  GCcolModel.setDataCellRenderer(0, Renderer);
	  GCcolModel.setDataCellRenderer(1, Renderer);

	  main_container.add(GroupCountsTable, {row : 2, column : 0});
	  // END-----------------group counts table----------------


	  // --------------------graph-----------------------------


          var panel = new qxprotovis.Panel().set({width:560, height:300});

          var pv = panel.getPv();
          var data = pv.range(60).map(function(x) {return Math.random()*99;});

          var vis = panel.getPanel()
              .bottom(15)
              .left(50)
              .right(0)
              .top(5);

          var x = pv.Scale.linear(0, 99).range(0, 500);
          var y = pv.Scale.linear(0, 99).range(0, 270);

	  // X axis
	  vis.add(pv.Rule)
	      .bottom(0)
	      .lineWidth(2)
	      .strokeStyle("#000");

          // Y-axis and ticks.
	  vis.add(pv.Rule)
	      .left(0)
	      .lineWidth(1)
	      .strokeStyle("#000");

          vis.add(pv.Rule)
              .data(function(){return y.ticks();})
              .bottom(y)
              .left(-4)
              .right(-4)
              .strokeStyle(function(d){return d ? "rgba(128,128,128,.2)" : "#000";})
              .anchor("left").add(pv.Label)
              .text(y.tickFormat);

	  var leftLabel=vis.add(pv.Label)
	      .bottom(-15)
	      .left(x(0));

	  var rightLabel=vis.add(pv.Label)
	      .bottom(-15)
	      .left(x(99)-100);

	  var lines=new Array();
	  var colors=["lightblue","red","green","yellow","orange"];

	  for (var i=0; i<5; i++){
	      lines[i]=vis.add(pv.Line)
		  .data([])
		  .left( function(d) {return x(d.time);} )
		  .bottom( function(d) {return y(d.count);} )
		  .lineWidth(2)
		  .strokeStyle( colors[i] );
	  };
	  
          vis.render();

	  var redraw=function(newData, mindate, maxdate, lines, leftLabel, rightLabel)
	  {
	      var i, 
	      minCount, maxCount, 
	      minTimeSlot, maxTimeSlot, 
	      tmpMinCount, tmpMaxCount, 
	      tmpMinTimeSlot, tmpMaxTimeSlot, 
	      usedData;

	      // remove lines from graph
	      for (i in lines){
		  lines[i].data([]);
	      };

	      // update left and right labels
	      leftLabel.text(mindate);
	      rightLabel.text(maxdate);

	      // only using the first 5 groups to make graphs
	      usedData=newData.slice(0,5);
	      
	      qx.log.Logger.debug(this,newData[0]);
	      qx.log.Logger.debug(this,'Length:'+usedData.length);

	      minCount = pv.min(newData[0].tl, function(d) {return d.count;});
	      maxCount = pv.max(newData[0].tl, function(d) {return d.count;});
	      /*
	       Need to calculate min and max timeslots becaues timeslot can be negative. 
	       Time of the log entries in an object in the database can be earlier than the time when the object was
	       saved (which is the key of the object) Currently an object contains the last 5 minutes of log entries 
	       but this can change in a later version. Safest way to handle it is to calculate min, max timeslot
	       */

	      minTimeSlot = pv.min(newData[0].tl, function(d) {return d.time;});
	      maxTimeSlot = pv.max(newData[0].tl, function(d) {return d.time;});

	      for (i=0; i<usedData.length; i++){
		  qx.log.Logger.debug(this,usedData[i].tl);

		  lines[i].data( usedData[i].tl );

		  tmpMinCount = pv.min(newData[i].tl, function(d) {return d.count;});
		  tmpMaxCount = pv.max(newData[i].tl, function(d) {return d.count;});
		  if (tmpMinCount < minCount) { minCount=tmpMinCount; };
		  if (tmpMaxCount > maxCount) { maxCount=tmpMaxCount; };

		  tmpMinTimeSlot = pv.min(newData[i].tl, function(d) {return d.time;});
		  tmpMaxTimeSlot = pv.max(newData[i].tl, function(d) {return d.time;});
		  if (tmpMinTimeSlot < minTimeSlot) { minTimeSlot=tmpMinTimeSlot; };
		  if (tmpMaxTimeSlot > maxTimeSlot) { maxTimeSlot=tmpMaxTimeSlot; };
	      };

	      y.domain(minCount, maxCount);
	      x.domain(minTimeSlot, maxTimeSlot);
	      vis.render();
	  };

	  var Graph={
	      panel: panel,
	      data: data,
	      vis: vis,
	      lines: lines,
	      leftLabel: leftLabel,
	      rightLabel: rightLabel,
	      redraw: redraw
	  };

	  main_container.add(panel, {row : 2, column : 1});

	  // END-----------------graph-----------------------------


	  main_container.moveTo(20, 30);
	  
	  LogAnalyzer = {
	      window    : main_container,
	      startdate : dateStartDate,
	      starttime : txtStartTime,
	      enddate   : dateEndDate,
	      endtime   : txtEndTime,
	      fnode     : frNode,
	      ftype     : frType,
	      ffrom     : frFrom,
	      frecv     : frRecv,
	      fsubject  : frSubject,
	      gnode     : grNode,
	      gtype     : grType,
	      gfrom     : grFrom,
	      grecv     : grRecv,
	      gsubject  : grSubject,
	      latable   : LogAnalyzerTable,
	      gctable   : GroupCountsTable,
	      graph     : Graph
	  };

	  return LogAnalyzer;
	},

      openLogAnalyzer : function()
      {
	  this.widgets.loganalyzer.window.open();
      },

      refreshLogAnalyzer : function(LogAnalyzer)
      {
	  this.debug('refreshing log table data');

	  // refresh log table
	  LogAnalyzer.latable.getTableModel().reloadData();

	  // refresh group counts table

	  this.debug('refreshing group data');
	  var baseUrl  = '/services/LogAnalyzerGroupingData.yaws';
	  var dateformatter=new qx.util.format.DateFormat("EEE, dd MMM y");
	  var parameters = "?startdate=" + dateformatter.format( LogAnalyzer.startdate.getValue() );
	  parameters += "&starttime=" + LogAnalyzer.starttime.getValue();
	  parameters += "&enddate="   + dateformatter.format( LogAnalyzer.enddate.getValue() );
	  parameters += "&endtime="   + LogAnalyzer.endtime.getValue();
	  parameters += "&fnode="     + LogAnalyzer.fnode.getValue();
	  parameters += "&ftype="     + LogAnalyzer.ftype.getValue();
	  parameters += "&ffrom="     + LogAnalyzer.ffrom.getValue();
	  parameters += "&frecv="     + LogAnalyzer.frecv.getValue();
	  parameters += "&fsubject="  + LogAnalyzer.fsubject.getValue();
	  parameters += "&gnode="     + LogAnalyzer.gnode.getValue();
	  parameters += "&gtype="     + LogAnalyzer.gtype.getValue();
	  parameters += "&gfrom="     + LogAnalyzer.gfrom.getValue();
	  parameters += "&grecv="     + LogAnalyzer.grecv.getValue();
	  parameters += "&gsubject="  + LogAnalyzer.gsubject.getValue();
	  
	  var url = baseUrl + parameters;

	  var req = new qx.io.remote.Request(url, "GET", "application/json");
	  req.setTimeout(15000);

	  var gctable=LogAnalyzer.gctable;
	  var loadedFun=function(response){
	      var Model=gctable.getTableModel();
	      var Content=response.getContent();
	      Model.removeRows(0, Model.getRowCount()-1);
	      Model.setRowsAsMapArray(Content.gridData);

	      LogAnalyzer.graph.redraw(Content.graphData, Content.mindate, Content.maxdate, 
				       LogAnalyzer.graph.lines, LogAnalyzer.graph.leftLabel, LogAnalyzer.graph.rightLabel);
	  };

	  req.addListener("completed", loadedFun, this);      
	  req.addListener("timeout", function() {alert("Request timed out.");});
	  req.send();
      },

      //----------------------------------------------------------------------------------------------------------

      loadConfig : function(parent)
      {
	  this.debug('load config');
	  var url = '/services/getGUIConfig.yaws';
	  var req = new qx.io.remote.Request(url, "GET", "application/json");
	  req.setAsynchronous(false);
	  req.setTimeout(15000);

	  var loadedFun=function(response){
	      parent.config=response.getContent();
	      this.debug(parent.config);
	  };

	  req.addListener("completed", loadedFun, this);      
	  req.addListener("timeout", function() {alert("Request timed out.");});
	  req.send();


	  this.debug('load client data');

	  var url2 = '/services/getClientList.yaws';
	  var req2 = new qx.io.remote.Request(url2, "GET", "application/json");
	  req2.setAsynchronous(false);
	  req2.setTimeout(15000);

	  var loadedFun2=function(response2){
	      parent.clients=response2.getContent().clientlist;
	      this.debug(parent.clients);
	  };

	  req2.addListener("completed", loadedFun2, this);      
	  req2.addListener("timeout", function() {alert("Request timed out.");});
	  req2.send();

      }
  }
});
