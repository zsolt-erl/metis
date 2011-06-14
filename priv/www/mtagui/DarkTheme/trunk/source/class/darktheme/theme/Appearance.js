/* ************************************************************************

   Copyright:
     2010 Norbert Schröder

   License:
     LGPL: http://www.gnu.org/licenses/lgpl.html
     EPL: http://www.eclipse.org/org/documents/epl-v10.php

   Authors:
     * Norbert Schröder (scro34)

************************************************************************ */

/* ************************************************************************

#asset(darktheme/decoration/*)

************************************************************************ */

qx.Theme.define("darktheme.theme.Appearance",
{
  extend: qx.theme.modern.Appearance,

  appearances:
  {
    "atom/label": 
	{
      style : function(states)
      {
        return {
          textColor : states.disabled ? "text-disabled" : undefined
        };
      }
    },
	
    "button":
    {
      style: function(states)
      {
        return {
		  padding: states.pressed || states.checked 
		           || (states.checked && states.disabled) ? [4, 4, 0, 6] : [2, 6, 2, 4], 
          decorator: states.pressed || states.checked ?
                        "button-checked" :
                     states.hovered && !states.disabled ?
                        "button-hovered" : "button",
		  textColor: "text-button",
		  center: true
        };
      }
	},
	
	"button-red":
    {
      style: function(states)
      {
        return {
		  padding: states.pressed || states.checked 
		           || (states.checked && states.disabled) ? [4, 4, 0, 6] : [2, 6, 2, 4], 
          decorator: states.pressed || states.checked ?
                        "button-red-checked" :
                     states.hovered && !states.disabled ?
                        "button-red-hovered" : "button",
		  textColor: "text-button",
		  center: true
        };
      }
	},
	
	"button-simple":
    {
      alias: "atom",

      style: function(states)
      {
        return {
		  padding: states.pressed || states.checked 
		           || (states.checked && states.disabled) ? [4, 2, 2, 4] : [3], 
          decorator: states.pressed || states.checked ?
                        "button-simple-checked" :
                     states.hovered && !states.disabled ?
                        "button-simple-hovered" : "button-simple",
		  textColor: "text-button"
        };
      }
	},
	
	"button-frame" :
    {
      alias : "atom",

      style : function(states)
      {
        var decorator, textColor;

        if (states.checked && states.focused && !states.inner)
        {
          decorator = "button-checked";
          textColor = undefined;
        }
        else if (states.disabled)
        {
          decorator = "button-disabled";
          textColor = undefined;
        }
        else if (states.pressed)
        {
          decorator = "button-checked";
          textColor = "text-hovered";
        }
        else if (states.checked)
        {
          decorator = "button-checked";
          textColor = undefined;
        }
        else if (states.hovered)
        {
          decorator = "button-hovered";
          textColor = "text-hovered";
        }
        else if (states.preselected && states.focused && !states.inner)
        {
          decorator = "button-hovered";
          textColor = "text-hovered";
        }
        else if (states.preselected)
        {
          decorator = "button-hovered";
          textColor = "text-hovered";
        }
        else if (states.focused && !states.inner)
        {
          decorator = "button";
          textColor = undefined;
        }
        else
        {
          decorator = "button";
          textColor = undefined;
        }

        return {
          decorator : decorator,
          textColor : textColor,
          shadow : states.invalid && !states.disabled ? "button-invalid-shadow" : undefined
        }
      }
    },
	
	"hover-button":
    {
      alias: "button",
      include: "button",

      style: function(states)
      {
        return {
          decorator : states.hovered ? "button-checked": "button",
          textColor : states.hovered ? "text-selected" : undefined
        }
      }
    },
	
	/*
    ---------------------------------------------------------------------------
      COMBOBOX
    ---------------------------------------------------------------------------
    */
	
	"combobox/button":
    {
      include: "button-simple",
      alias: "button-simple",

      style : function(states)
      {
        var ret = {
          icon: "darktheme/decoration/arrows/down-invert.png",
          padding: 2
        };

        return ret;
      }
    },
	
	"combobox/popup" : "popup",
	
	/*
    ---------------------------------------------------------------------------
      DATE CHOOSER
    ---------------------------------------------------------------------------
    */
	
	"datechooser" :
    {
      style : function(states)
      {
        var decorator;

        var focused = !!states.focused;
        var invalid = !!states.invalid;
        var disabled = !!states.disabled;

        if (focused && invalid && !disabled) {
          decorator = "input-focused-invalid";
        } else if (focused && !invalid && !disabled) {
          decorator = "input-focused";
        } else if (disabled) {
          decorator = "input-disabled";
        } else if (!focused && invalid && !disabled) {
          decorator = "border-invalid";
        } else {
          decorator = "group";
        }

        return {
          padding : 2,
          decorator : decorator,
          backgroundColor : "background-datechooser"
        };
      }
    },
	
	"datechooser/nav-button"  :
    {
      include : "button-simple",
      alias : "button-simple",

      style : function(states)
      {
        var result = {
          padding : [ 2, 4 ],
          shadow : undefined
        };

        if (states.lastYear) {
          result.icon = "darktheme/decoration/arrows/rewind-invert.png";
          result.marginRight = 1;
        } else if (states.lastMonth) {
          result.icon = "darktheme/decoration/arrows/left-invert.png";
        } else if (states.nextYear) {
          result.icon = "darktheme/decoration/arrows/forward-invert.png";
          result.marginLeft = 1;
        } else if (states.nextMonth) {
          result.icon = "darktheme/decoration/arrows/right-invert.png";
        }

        return result;
      }
    },
	
	"datechooser/date-pane" :
    {
      style : function(states)
      {
        return {
          textColor: states.disabled ? "text-disabled" : undefined,
          marginTop : 2
        };
      }
    },
	
	"datechooser/day" :
    {
      style : function(states)
      {
        return {
          textAlign : "center",
          decorator : states.disabled ? undefined : states.selected ? "selected" : undefined,
          textColor : states.disabled ? "text-disabled" : states.selected ? "text-label" : states.otherMonth ? "text-inactive" : undefined,
          font      : states.today ? "bold" : undefined,
          padding   : [ 2, 4 ]
        };
      }
    },
	
	"datechooser/week" :
    {
      style : function(states)
      {
        return {
          textAlign : "center",
          padding   : [ 2, 4 ],
          backgroundColor : "background-light"
        };
      }
    },
	
	"datechooser/weekday" :
    {
      style : function(states)
      {
        return {
          textColor : states.disabled ? "text-disabled" : states.weekend ? "text-inactive" : undefined,
          textAlign : "center",
          paddingTop : 2,
          backgroundColor : "background-light"
        };
      }
    },
	
	/*
    ---------------------------------------------------------------------------
      GROUPBOX
    ---------------------------------------------------------------------------
    */

	"groupbox/frame" :
    {
      style : function(states)
      {
        return {
          padding   : 6,
          decorator : "group"
        };
      }
    },
	
	/*
    ---------------------------------------------------------------------------
      HTMLAREA
    ---------------------------------------------------------------------------
    */

    "htmlarea" :
    {
      "include" : "widget",

      style : function(states)
      {
        return {
          backgroundColor : "white",
		  decorator: "input"
        }
      }
    },
	
	/*
    ---------------------------------------------------------------------------
      LIST
    ---------------------------------------------------------------------------
    */
	
	"listitem" :
    {
      style : function(states)
      {
        var decorator;
        if (states.dragover) {
          decorator = states.selected ? "selected-dragover" : "dragover";
        } else {
          decorator = states.selected ? "menu-button-selected" : undefined;
        }

        return {
          padding   : states.dragover ? [5, 5, 3, 5] : 3,
          textColor : states.selected ? "text-selected" : undefined,
          decorator : decorator
        };
      }
    },
	
	/*
    ---------------------------------------------------------------------------
      MENU
    ---------------------------------------------------------------------------
    */
	"menubar" :
    {
      style : function(states)
      {
        return {
          decorator : "menubar",
		  shadow: "shadow-window"
        }
      }
    },
	
	"menu-button":
    {
      alias : "atom",

      style : function(states)
      {
        return {
          textColor: "text-button",
		  decorator: states.selected ? "menu-button-selected" : undefined,
          padding   : [ 3, 5 ]
        };
      }
    },
	
	"menu-button/arrow" :
    {
      include : "image",

      style : function(states)
      {
        return {
          source : "darktheme/decoration/arrows/right-invert.png",
          alignY : "middle"
        };
      }
    },
	
	"menu-checkbox":
    {
      alias: "menu-button",
      include: "menu-button",

      style: function(states)
      {
        return {
          icon : states.checked ? "decoration/menu/checkbox-invert.gif" : undefined
        }
      }
    },
	
	"menu-radiobutton" :
    {
      alias : "menu-button",
      include : "menu-button",

      style : function(states)
      {
        return {
          icon : states.checked ? "decoration/menu/radiobutton-invert.gif" : undefined
        }
      }
    },
	
	"menubar-button" :
    {
      alias : "atom",

      style : function(states)
      {
        return {
          decorator : states.pressed || states.hovered ? "menubar-selected" : undefined,
          textColor : states.pressed || states.hovered ? "text-selected" : "text-button",
          padding   : [ 3, 8 ]
        }
      }
    },
	
	/*
    ---------------------------------------------------------------------------
      POPUP
    ---------------------------------------------------------------------------
    */
	"popup": 
    {
      style: function(states)
      {
        return {
          decorator: "menu",
		  textColor: "white",
		  backgroundColor: "background-light",
          shadow: "shadow-window"
        }
      }
    },
	
	/*
    ---------------------------------------------------------------------------
      ROOT
    ---------------------------------------------------------------------------
    */
	"root" :
    {
      style : function(states)
      {
        return {
          backgroundColor : "background-application",
          textColor       : "text-label",
          font            : "default"
        };
      }
    },
	
	/*
    ---------------------------------------------------------------------------
      SCROLLBAR
    ---------------------------------------------------------------------------
    */
	"scrollbar/slider/knob" :
    {
      include : "button-frame",

      style : function(states)
      {
        var decorator;
		if (states.horizontal) 
		{
		  if (states.hovered) 
		  {
		    decorator = "scrollbar-slider-horizontal-hovered";
		  } else {
		    decorator = "scrollbar-slider-horizontal";
		  }
		} else {
		  if (states.hovered) 
		  {
            decorator = "scrollbar-slider-vertical-hovered";
		  } else {
		    decorator = "scrollbar-slider-vertical";
		  }
		}
        return {
          decorator : decorator,
          minHeight : states.horizontal ? undefined : 9,
          minWidth  : states.horizontal ? 9 : undefined
        };
      }
    },
	
	"scrollbar/button" :
    {
      style : function(states)
      {
        var icon = "darktheme/decoration/scrollbar/";
		var decorator;
        if (states.left) {
          icon += "left";
		  decorator = "scrollbar-slider-horizontal";
        } else if (states.right) {
          icon += "right";
		  decorator = "scrollbar-slider-horizontal";
        } else if (states.up) {
          icon += "up";
		  decorator = "scrollbar-slider-vertical";
        } else {
          icon += "down";
		  decorator = "scrollbar-slider-vertical";
        }
		icon += "-invert.png";
		
		if (states.hovered)
		{
		  decorator += "-hovered";
		}

        if (states.left || states.right)
        {
          return {
		    decorator: decorator,
            padding : [0, 0, 0, states.left ? 3 : 4],
            icon : icon,
            width: 15,
            height: 14
          }
        }
        else
        {
          return {
		    decorator: decorator,
            padding : [0, 0, 0, 2],
            icon : icon,
            width: 14,
            height: 15
          }
        }
      }
    },
	
	/*-------------------------------------------------------------------------
      SELECTBOX
    ---------------------------------------------------------------------------
    */
	"selectbox/arrow" :
    {
      include : "image",

      style : function(states)
      {
        return {
          source : "darktheme/decoration/arrows/down-small-invert.png",
          paddingLeft : 5
        };
      }
    },
	
	/*-------------------------------------------------------------------------
      SLIDEBAR
    ---------------------------------------------------------------------------
    */

    "slidebar" : {},
    "slidebar/scrollpane" : {},
    "slidebar/content" : {},

    "slidebar/button-forward" :
    {
      alias : "button-simple",
      include : "button-simple",

      style : function(states)
      {
        return {
          padding : 5,
          center : true,
          icon : states.vertical ?
            "darktheme/decoration/arrows/down-invert.png" :
            "darktheme/decoration/arrows/right-invert.png"
        };
      }
    },

    "slidebar/button-backward" :
    {
      alias : "button-simple",
      include : "button-simple",

      style : function(states)
      {
        return {
          padding : 5,
          center : true,
          icon : states.vertical ?
            "darktheme/decoration/arrows/up-invert.png" :
            "darktheme/decoration/arrows/left-invert.png"
        };
      }
    },
	
	/*-------------------------------------------------------------------------
      SLIDER
    ---------------------------------------------------------------------------
    */
	
	"slider" :
    {
      style : function(states)
      {
        var decorator;

        var focused = !!states.focused;
        var invalid = !!states.invalid;
        var disabled = !!states.disabled;

        if (focused && invalid && !disabled) {
          decorator = "input-focused-invalid";
        } else if (focused && !invalid && !disabled) {
		  decorator = "group-focused";
        } else if (disabled) {
          decorator = "input-disabled";
        } else if (!focused && invalid && !disabled) {
          decorator = "border-invalid";
        } else {
		  decorator = "group";
        }

        return {
          decorator : decorator
        }
      }
    },
	
	"slider/knob" :
    {
      include : "button-simple",

      style : function(states)
      {
        return {
          decorator : states.disabled ? "scrollbar-slider-horizontal-disabled" :
					  states.pressed ? "button-simple-hovered" : "button-simple",
          shadow: undefined,
          height : 14,
          width : 14
        };
      }
    },
	/*
    ---------------------------------------------------------------------------
      SPINNER
    ---------------------------------------------------------------------------
    */
	"spinner/upbutton" :
    {
      alias : "button-simple",
      include : "button-simple",

      style : function(states)
      {
        return {
          icon : "darktheme/decoration/arrows/up-small-invert.png",
          padding : states.pressed ? [2, 2, 0, 4] : [1, 3, 1, 3],
          shadow: undefined
        }
      }
    },

    "spinner/downbutton" :
    {
      alias : "button-simple",
      include : "button-simple",

      style : function(states)
      {
        return {
          icon : "darktheme/decoration/arrows/down-small-invert.png",
          padding : states.pressed ? [2, 2, 0, 4] : [1, 3, 1, 3],
          shadow: undefined
        };
      }
    },
	
	"spinner/textfield" :
    {
      style : function(states)
      {
        return {
          marginRight: 2,
          padding: [2, 4, 1],
          textColor: states.disabled ? "text-disabled" : "text-textfield"
        };
      }
    },
	
	/*
    ---------------------------------------------------------------------------
      SPLITBUTTON
    ---------------------------------------------------------------------------
    */
	"splitbutton/button": 
    {
      alias: "atom",

      style: function(states)
      {
        return {
		  padding: states.pressed || states.checked 
		           || (states.checked && states.disabled) ? [4, 2, 2, 4] : [3], 
          decorator: states.pressed || states.checked ?
                        "splitbutton-checked" :
                     states.hovered && !states.disabled ?
                        "splitbutton-hovered" : "splitbutton",
		  textColor: "text-button",
		  center: true
        };
      }
	},
	
	"splitbutton/arrow":
    {
      alias : "button",
      include : "button",

      style : function(states)
      {
        return {
          icon : "darktheme/decoration/arrows/down-small-invert.png",
		  decorator: states.pressed || states.checked ?
                        "splitbutton-right-checked" :
                     states.hovered && !states.disabled ?
                        "splitbutton-right-hovered" : "splitbutton-right",
          // padding : 2,
          marginLeft : 1,
		  marginRight: 1
        };
      }
    },
	
	/*
    ---------------------------------------------------------------------------
      SPLITPANE
    ---------------------------------------------------------------------------
    */
	"splitpane/splitter/knob" :
    {
      style : function(states)
      {
        return {
          source : states.horizontal ? "darktheme/decoration/splitpane/knob-horizontal.png" : "darktheme/decoration/splitpane/knob-vertical.png"
        };
      }
    },
	
	/*
    ---------------------------------------------------------------------------
      TABLE
    ---------------------------------------------------------------------------
    */
	
	"table":
    {
      alias: "widget",

      style: function(states)
      {
        return {
          decorator: "input"
        };
      }
    },
	
	"table/column-button" :
    {
      alias : "button-frame",

      style : function(states)
      {
        return {
          decorator : "table-header-cell",
          padding   : 3,
          icon      : "darktheme/decoration/table/select-column-order-invert.png"
        };
      }
    },
	
	"table-header-cell" :
    {
      alias : "atom",
	  
      style : function(states)
      {
        return {
          minWidth  : 13,
          minHeight : 20,
          padding   : states.hovered ? [ 3, 4, 3, 4 ] : [ 3, 4 ],
          decorator : states.hovered ? "table-header-cell-selected" : "table-header-cell",
          sortIcon  : states.sorted ?
              (states.sortedAscending ? "darktheme/decoration/arrows/down-invert.png" : "darktheme/decoration/arrows/up-invert.png")
              : undefined
        }
      }
    },
   
    /*
    ---------------------------------------------------------------------------
      TABVIEW
    ---------------------------------------------------------------------------
    */

    "tabview" :
    {
      style : function(states)
      {
        return {
          contentPadding : 5
        }
      }
    },

    "tabview/bar" :
    {
      alias : "slidebar",

      style : function(states)
      {
        var result =
        {
          marginBottom : states.barTop ? -1 : 0,
          marginTop : states.barBottom ? -4 : 0,
          marginLeft : states.barRight ? -3 : 0,
          marginRight : states.barLeft ? -1 : 0,
          paddingTop : 0,
          paddingRight : 0,
          paddingBottom : 0,
          paddingLeft : 0
        }

        if (states.barTop || states.barBottom)
        {
          result.paddingLeft = 5;
          result.paddingRight = 7;
        }
        else
        {
          result.paddingTop = 5;
          result.paddingBottom = 7;
        }

        return result;
      }
    },

    "tabview/bar/button-forward" :
    {
      include : "slidebar/button-forward",
      alias : "slidebar/button-forward",

      style : function(states)
      {
        if (states.barTop)
        {
          return {
            marginTop: 4
          }
        }
		else if (states.barBottom)
        {
		  return {
            marginBottom: 4
          }
        }
		else if (states.barLeft)
        {
		  return {
            marginLeft: 4,
			marginTop: 2
          }
        }
        else
        {
          return {
            marginRight: 4,
			marginTop: 2
          }
        }
      }
    },

    "tabview/bar/button-backward" :
    {
      include : "slidebar/button-backward",
      alias : "slidebar/button-backward",

      style : function(states)
      {
        if (states.barTop)
        {
          return {
            marginTop: 4
          }
        }
		else if (states.barBottom)
        {
		  return {
            marginBottom: 4
          }
        }
		else if (states.barLeft)
        {
		  return {
            marginLeft: 4,
			marginBottom: 2
          }
        }
        else
        {
          return {
            marginRight: 4,
			marginBottom: 2
          }
        }
      }
    },

    "tabview/bar/scrollpane" : {},

    "tabview/pane" :
    {
      style : function(states)
      {
        return {
		  decorator : "group",
          minHeight : 100,
          marginBottom : states.barBottom ? -1 : 0,
          marginTop : states.barTop ? -1 : 0,
          marginLeft : states.barLeft ? -1 : 0,
          marginRight : states.barRight ? -1 : 0
        };
      }
    },

    "tabview-page/button" :
    {
      alias : "atom",

      style : function(states)
      {
        var decorator, padding=0;
        var marginTop=0, marginBottom=0, marginLeft=0, marginRight=0;

        if (states.checked)
        {
		  padding = [ 4, 12 ];
          if (states.barTop)
          {
		    decorator = "tabview-button-top-hovered";
            marginLeft = states.firstTab ? 0 : -4;
            marginRight = states.lastTab ? 0 : -4;
          }
          else if (states.barBottom)
          {
		    decorator = "tabview-button-bottom-hovered";
            marginLeft = states.firstTab ? 0 : -4;
            marginRight = states.lastTab ? 0 : -4;
          }
          else if (states.barRight)
          {
		    decorator = "tabview-button-right-hovered";
          }
          else
          {
		    decorator = "tabview-button-left-hovered";
          }
        }
		else
        {
          if (states.barTop)
          {
		    if (states.hovered)
			{
		      decorator = "tabview-button-top-hovered";
			}
            else if (states.disabled)
			{
			  decorator = "tabview-button-top-disabled";
			}			
			else 
			{
		      decorator = "tabview-button-top";
			}
		    padding = [ 2, 8 ];
            marginTop = 4;
            marginLeft = states.firstTab ? 4 : 0;
          }
          else if (states.barBottom)
          {
		    if (states.hovered)
			{
		      decorator = "tabview-button-bottom-hovered";
			}
            else if (states.disabled)
			{
			  decorator = "tabview-button-bottom-disabled";
			}			
			else 
			{
		      decorator = "tabview-button-bottom";
			}
		    padding = [ 2, 8 ];
            marginBottom = 4;
            marginLeft = states.firstTab ? 4 : 0;
          }
          else if (states.barRight)
          {
		    if (states.hovered)
			{
		      decorator = "tabview-button-right-hovered";
			}
			else if (states.disabled)
			{
			  decorator = "tabview-button-right-disabled";
			}
			else 
			{
		      decorator = "tabview-button-right";
			}
		    padding = [ 4, 12 ];
            marginRight = 5;
          }
          else
          {
		    if (states.hovered)
			{
		      decorator = "tabview-button-left-hovered";
			}
            else if (states.disabled)
			{
			  decorator = "tabview-button-left-disabled";
			}
			else 
			{
		      decorator = "tabview-button-left";
			}
		    padding = [ 4, 12 ];
            marginLeft = 5;
          }
        }
		
        return {
          zIndex : states.checked ? 10 : 5,
          decorator : decorator,
          padding   : padding,
          marginTop : marginTop,
          marginBottom : marginBottom,
          marginLeft : marginLeft,
          marginRight : marginRight,
		  textColor: states.disabled ? "text-inactive" : "text-label"
        };
      }
    },

    "tabview-page/button/label" :
    {
      alias : "label",

      style : function(states)
      {
        return {
          padding : [0, 1, 0, 1],
          margin : states.focused ? 0 : 1,
          decorator : states.focused ? "keyboard-focus" : undefined
        };
      }
    },

    "tabview-page/button/close-button":
    {
      alias: "atom",
      style: function(states)
      {
	    var icon;
	    if (states.hovered)
		{
		  icon = "darktheme/decoration/tabview/close-button-hovered.png";
		} else {
		  icon = "darktheme/decoration/tabview/close-button.png";
		}
        return {
          icon: icon
        };
      }
    },
	
	/*
    ---------------------------------------------------------------------------
      TEXTFIELD
    ---------------------------------------------------------------------------
    */
	"textfield" :
    {
      style : function(states)
      {
        var decorator;

        var focused = !!states.focused;
        var invalid = !!states.invalid;
        var disabled = !!states.disabled;

        if (focused && invalid && !disabled) {
          decorator = "input-focused-invalid";
        } else if (focused && !invalid && !disabled) {
          decorator = "input-focused";
        } else if (disabled) {
          decorator = "input-disabled";
        } else if (!focused && invalid && !disabled) {
          decorator = "border-invalid";
        } else {
          decorator = "input";
        }

        var textColor;
        if (states.disabled) {
          textColor = "text-placeholder";
        } else if (states.showingPlaceholder) {
          textColor = "text-placeholder";
        } else {
          textColor = "text-textfield";
        }

        return {
          decorator : decorator,
          padding : [ 2, 4, 1 ],
          textColor : textColor
        };
      }
    },
	
	/*
    ---------------------------------------------------------------------------
      TOOLBAR
    ---------------------------------------------------------------------------
    */
	"toolbar/part": "widget",
	
	"toolbar/part/handle":
    {
      style: function(states)
      {
        return {
          source: "darktheme/decoration/toolbar/toolbar-handle-knob.png",
          marginLeft: 3,
          marginRight: 3
        };
      }
    },
	
	"toolbar-button":
    {
      alias: "atom",

      style: function(states)
      {
        return {
		  padding: states.pressed || states.checked ? [6, 4, 4, 6] : [6, 6, 6, 6],
		  margin: states.pressed || states.checked ? [0] : [0],
          decorator: states.pressed || states.checked ?
                        "toolbar-button-checked" :
                      states.hovered && !states.disabled ?
                        "toolbar-button-hovered" : undefined,
		  textColor: "text-button"
        };
      }
    },
	
	"toolbar-button-light":
    {
      alias: "atom",

      style: function(states)
      {
        return {
		  padding: states.pressed || states.checked ? [6, 4, 4, 6] : [6, 6, 6, 6],
		  margin: states.pressed || states.checked ? [0] : [0],
          decorator: states.pressed || states.checked ?
                        "toolbar-button-light-checked" :
                      states.hovered && !states.disabled ?
                        "toolbar-button-light-hovered" : undefined,
		  textColor: "text-button"
        };
      }
    },
	
	"toolbar-menubutton/arrow" :
    {
      alias : "image",
      include : "image",

      style : function(states)
      {
        return {
          source : "darktheme/decoration/arrows/down-small-invert.png"
        };
      }
    },
	
	"toolbar-splitbutton/button" :
    {
      style : function(states)
      {
        return {
		  padding: states.pressed || states.checked ? [6, 5, 4, 6] : [6, 6, 6, 6],
		  decorator: states.pressed || (states.checked && !states.hovered) || (states.checked && states.disabled) ?
                        "toolbar-button-checked" :
                      states.hovered && !states.disabled ?
                        "toolbar-splitbutton-hovered" : undefined
        };
      }
    },
	
	"toolbar-splitbutton/arrow" :
    {
      style : function(states)
      {
        return {
          icon : "darktheme/decoration/arrows/down-invert.png",
		  padding: states.pressed || states.checked ? [6, 4, 4, 5] :
		           states.hovered && !states.disabled ? [6, 5, 6, 5] : [6, 5, 6, 7],
		  decorator: states.pressed || (states.checked && !states.hovered) || (states.checked && states.disabled) ?
                        "toolbar-button-checked" :
                      states.hovered && !states.disabled ?
                        "toolbar-splitbutton-arrow-hovered" : undefined
        };
      }
    },
	
	"toolbar-splitbutton-light/button" :
    {
      style : function(states)
      {
        return {
		  padding: states.pressed || states.checked ? [6, 5, 4, 6] : [6, 6, 6, 6],
		  decorator: states.pressed || (states.checked && !states.hovered) || (states.checked && states.disabled) ?
                        "toolbar-button-light-checked" :
                      states.hovered && !states.disabled ?
                        "toolbar-splitbutton-light-hovered" : undefined
        };
      }
    },
	
	"toolbar-splitbutton-light/arrow" :
    {
      style : function(states)
      {
        return {
          icon : "darktheme/decoration/arrows/down-invert.png",
		  padding: states.pressed || states.checked ? [6, 4, 4, 5] :
		           states.hovered && !states.disabled ? [6, 5, 6, 5] : [6, 5, 6, 7],
		  decorator: states.pressed || (states.checked && !states.hovered) || (states.checked && states.disabled) ?
                        "toolbar-button-light-checked" :
                      states.hovered && !states.disabled ?
                        "toolbar-splitbutton-light-arrow-hovered" : undefined
        };
      }
    },
	
	/*
    ---------------------------------------------------------------------------
      TOOL TIP
    ---------------------------------------------------------------------------
    */

    "tooltip" :
    {
      style : function(states)
      {
        return {
          backgroundColor : "background-tip",
          padding : [ 1, 3, 2, 3 ],
          offset : [ 15, 5, 5, 5 ],
		  decorator: "tooltip",
		  textColor: "black"
        };
      }
    },
	
	/*
    ---------------------------------------------------------------------------
      TREE
    ---------------------------------------------------------------------------
    */
    "tree-item":
    {
      style: function(states)
      {
        return {
          padding: [ 2, 6 ],
          textColor: states.selected ? "text-selected" : "text-tree",
          decorator: states.selected ? "selected" : undefined
        }
      }
    },
	
	/*
    ---------------------------------------------------------------------------
      WINDOW
    ---------------------------------------------------------------------------
    */
	"window":
    {
      style: function(states)
      {
        return {
		  textColor: "text-window",
          decorator: "window",
          shadow: "shadow-window",
		  contentPadding : [5]
        };
      }
    },
	
	"window/pane": "widget",
	
	"window/title" :
    {
      style : function(states)
      {
        return {
          alignY: "top",
          textColor: "text-caption",
		  font: "bold",
		  paddingTop: 5,
		  paddingLeft: 8
        };
      }
    },
	
	"window/close-button":
    {
      alias: "atom",

      style: function(states)
      {
	    var icon;
		switch (true)
		{
		  case states.pressed:
		    icon = "darktheme/decoration/window/close-button-pressed.png";
			break;
			
		  case states.hovered:
		    icon = "darktheme/decoration/window/close-button-hovered.png";
			break;
			
		  default:
		    icon = "darktheme/decoration/window/close-button.png";
		}
        return {
          icon: icon,
          margin : [ 2, 2, 2, 1 ]
        };
      }
    },
	
	"window/maximize-button":
    {
      alias: "atom",

      style: function(states)
      {
	    var icon;
		switch (true)
		{
		  case states.pressed:
		    icon = "darktheme/decoration/window/maximize-button-pressed.png";
			break;
			
		  case states.hovered:
		    icon = "darktheme/decoration/window/maximize-button-hovered.png";
			break;
			
		  default:
		    icon = "darktheme/decoration/window/maximize-button.png";
		}
        return {
          icon: icon,
          margin: [ 2, 2, 2, 2 ]  
        };
      }
    },
	
	"window/minimize-button":
    {
      alias: "atom",

      style: function(states)
      {
	    var icon;
		switch (true)
		{
		  case states.pressed:
		    icon = "darktheme/decoration/window/minimize-button-pressed.png";
			break;
			
		  case states.hovered:
		    icon = "darktheme/decoration/window/minimize-button-hovered.png";
			break;
			
		  default:
		    icon = "darktheme/decoration/window/minimize-button.png";
		}
        return {
          icon: icon,
          margin : [ 2, 2, 2, 1 ] 
        };
      }
    },
	
	"window/statusbar-text": {},
	
	"window/restore-button":
    {
      alias: "atom",

      style: function(states)
      {
	    var icon;
		switch (true)
		{
		  case states.pressed:
		    icon = "darktheme/decoration/window/restore-button-pressed.png";
			break;
			
		  case states.hovered:
		    icon = "darktheme/decoration/window/restore-button-hovered.png";
			break;
			
		  default:
		    icon = "darktheme/decoration/window/restore-button.png";
		}
        return {
          icon: icon,
          margin : [ 2, 2, 2, 1 ] 
        };
      }
    }
	
  }
});