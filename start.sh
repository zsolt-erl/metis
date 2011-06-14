export RIAKC=lib/riakc-1.0.1/ebin
export PROTOBUFFS=lib/protobuffs-0.5.0/ebin
export SETUID=lib/erlang-setuid/ebin
export LOG4ERL=lib/log4erl/ebin
export YAWS=lib/yaws/ebin

export SERVERNAME=`grep -v ' *#' priv/mta.conf | grep 'server:' | awk '{print $2}'`
export COOKIE=`grep 'cookie:' priv/mta.conf | awk '{print $2}'`


erl -name $SERVERNAME -setcookie $COOKIE -pa ebin/ lib/*/ebin/ deps/*/ebin -boot start_sasl -config priv/metis
