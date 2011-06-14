/* ************************************************************************

   Copyright:
     2010 Norbert Schröder

   License:
     LGPL: http://www.gnu.org/licenses/lgpl.html
     EPL: http://www.eclipse.org/org/documents/epl-v10.php

   Authors:
     * Norbert Schröder (scro34)

************************************************************************ */

qx.Theme.define("darktheme.theme.Color",
{
  colors :
  {
    /*
    ---------------------------------------------------------------------------
      BACKGROUND COLORS
    ---------------------------------------------------------------------------
    */

    // application, desktop, ...
	"background-application": "#626262",

    // pane color for windows, splitpanes, ...
    "background-pane" : "#525252",

    // textfields, ...
	"background-light" : "#929292",

    // headers, ...
	"background-medium" : "#727272",

    // splitpane
    "background-splitpane" : "#525252",

    // tooltip, ...
    "background-tip" : "#ffffdd",

    // error tooltip
    "background-tip-error": "#C72B2B",

    // tables, ...
    "background-odd" : "yellow",
	
	// datechooser
	"background-datechooser": "#424242",
	
	// window
	"background-window": "#525252",
	
	// selection
	"background-selected": "#398E9D",

    /*
    ---------------------------------------------------------------------------
      TEXT COLORS
    ---------------------------------------------------------------------------
    */

    // other types
    "text-gray": "teal",

    // labels
	"text-label": "white",
	
	// buttons
	"text-button": "white",

    // group boxes
	"text-title": "white",

    // text fields
	"text-input": "yellow",
	"text-textfield": "black",

    // states
	"text-hovered": "white",
	"text-disabled": "silver",
	"text-selected": "white",
	"text-active": "black",
	"text-inactive": "silver",
    "text-placeholder": "gray",
	
	//trees
	"text-tree": "black",

	//windows
    "text-window": "white",
	"text-caption": "white",
	
    /*
    ---------------------------------------------------------------------------
      BORDER COLORS
    ---------------------------------------------------------------------------
    */

    // menus, tables, scrollbars, list, etc.
	"border-main": "black",

    // between toolbars
    "border-separator": "#808080",

    // text fields
    "border-input": "#334866",

    // disabled text fields
    "border-disabled": "#B6B6B6",

    // tab view, window
    "border-pane": "#00204D",

    // buttons
    "border-button": "#666666",

    // tables (vertical line)
    "border-column": "#CCCCCC",

    // focus state of text fields
    "border-focused": "#99C3FE",

    // invalid form widgets
    "invalid": "#990000",
    "border-focused-invalid": "#FF9999",


    /*
    ---------------------------------------------------------------------------
      TABLE COLORS
    ---------------------------------------------------------------------------
    */

    // equal to "background-pane"
    "table-pane": "#F3F3F3",

    // own table colors
    "table-focus-indicator": "#80B4EF",
    "table-row-background-focused-selected": "#398E9D",
    "table-row-background-focused": "#80B4EF",
    "table-row-background-selected": "#398E9D",

    // equal to "background-pane" and "background-odd"
    "table-row-background-even": "#F3F3F3",
    "table-row-background-odd": "#E4E4E4",

    // equal to "text-selected" and "text-label"
    "table-row-selected": "#fffefe",
    "table-row": "#1a1a1a",

    // equal to "border-collumn"
    "table-row-line": "transparent",
    "table-column-line": "#CCCCCC"
    
  }
});
