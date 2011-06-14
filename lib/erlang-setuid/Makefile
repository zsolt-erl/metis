
all: compile

compile:
	test -d ebin || mkdir ebin
	/usr/bin/erl -make
	cd priv && make

debug:
	/usr/bin/erl -make
	cd priv && make debug

clean:
	rm -rf ./ebin/*.beam
	rm -rf ./ebin/*.so
	rm -rf ./*.beam

run:
	/usr/bin/erl -pa ./ebin 

docs:
	/usr/bin/erl -noshell -run edoc_run application "'setuid'" '"."' '[{title, "Erlang binding for "}, {hidden, false}, {private, false}]' -s erlang halt
