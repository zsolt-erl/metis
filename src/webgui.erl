-module(webgui).

-include("metis.hrl").

-export([start/1, stop/0]).


start(MasterNode)->
    Port=?CONF(gui, port),
    os:putenv("YAWSHOME","/tmp/yaws/"),
    Res=yaws:start_embedded("priv/www", 
			    [{port,Port},{listen,{0,0,0,0}},
			     {partial_post_size,102400}, {opaque, MasterNode}],   %% sconf, 100k post size
			    [{include_dir, ["priv/www/services"]}]),                  %% gconf
    case Res of
	ok -> {ok, Port};
	_Other -> error
    end.
	     
    

stop()->
    yaws:stop().
