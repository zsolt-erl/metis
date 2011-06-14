ERL_INTERFACE=/usr/lib/erlang/lib/erl_interface-3.6.4
ERL_ROOT=/usr/lib/erlang
OUTPUT=../ebin/setuid_drv.so
LDFLAGS=-shared -fPIC -undefined -lei -lerl_interface -L${ERL_INTERFACE}/lib 
CFLAGS=-o ${OUTPUT} -I${ERL_ROOT}/usr/include -I${ERL_INTERFACE}/include/ -Wall -W -Wno-unused-parameter

all:
	gcc setuid_drv.c ${LDFLAGS} ${CFLAGS} -O2

debug:
	gcc setuid_drv.c ${LDFLAGS} ${CFLAGS} -ggdb -O0
