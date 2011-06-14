
all: deps compile

compile:
	./rebar compile

deps:
	cd deps/log4erl && make
	cd deps/riakc && make compile
	cd deps/yaws && ./configure && make
	cd lib/erlang-setuid && ./configure && make

clean:
	./rebar clean
	rm -f priv/logs/*.log
	cd priv/logs/sasl_server && rm -f 1 2 3 4 5 index
	cd priv/logs/sasl_client && rm -f 1 2 3 4 5 index
	rm -f *~ 
	rm -f src/*~ 
	rm -f *.access
	rm -f *.log

	cd deps/log4erl      && make clean
	cd deps/riakc        && make clean
	cd deps/yaws         && make clean
	cd lib/erlang-setuid && make clean


archive: BRANCH = $(shell git branch|grep '*'|cut -d' ' -f2)
archive: DATE   = $(shell date +%d%b%y)
archive:
	git archive HEAD | gzip > ../metis-branch-$(BRANCH)-$(DATE).tar.gz

gui:
	cd ./priv/www/mtagui/ && ./generate.py source-all && ./generate.py build && cp -r ./build ../ && cd ../ && rm -rf ./gui && mv ./build ./gui

install: TARGET = /home/metis/metis
install: PRIVFILES = mta.conf.example aliases.conf domain.conf log4erl.conf reroute.conf metis.config metis_client.config stdin_forcer metis_drv.so
install:
	useradd -m -s /bin/bash metis

	mkdir -p $(TARGET)/priv/queue $(TARGET)/priv/logs/sasl $(TARGET)/priv/logs/sasl_client $(TARGET)/priv/logs/error_logger $(TARGET)/priv/www/

	cp ./metis ./start.sh $(TARGET)
	cp -r ./ebin $(TARGET)
	cp -r ./lib $(TARGET)
	cp -r ./deps $(TARGET)
	cp -r priv/www/gui $(TARGET)/priv/www/
	cp -r priv/www/services  $(TARGET)/priv/www/
	cd priv/ && cp $(PRIVFILES)  $(TARGET)/priv/

	chown -R metis:metis $(TARGET)

client: TARGET = /home/metis/metis
client:
	useradd -m -U -s /bin/bash metis

	mkdir -p $(TARGET)/priv/queue $(TARGET)/priv/logs/sasl  $(TARGET)/ebin $(TARGET)/lib

	cp ./start.sh $(TARGET)
	cd ebin/ && cp client_boot.beam client_sup.beam misc.beam queue_mngr.beam queue_smtp_client.beam socket.beam smtp_util.beam conn_mngr.beam \
		$(TARGET)/ebin
	cp -r lib/log4erl $(TARGET)/lib/
	cd priv/ && cp aliases.conf domain.conf log4erl.conf reroute.conf sasl.config stdin_forcer metis_drv.so  $(TARGET)/priv/

	chown -R metis:metis $(TARGET)

keys:
	ssh-keygen -q -f priv/id_rsa
	chown metis:metis priv/id_rsa*

	if [ ! -d "/home/metis/.ssh/" ]; then mkdir /home/metis/.ssh && chown metis:metis /home/metis/.ssh; fi;
	mv priv/id_rsa /home/metis/.ssh/
	mv priv/id_rsa.pub /home/metis/metis/priv/

	@echo -e '\n\nCopy the public key (/home/metis/metis/priv/id_rsa.pub) into /home/metis/.ssh/authorized_keys file on each client!'

.PHONY: all compile deps clean pack gui archive install client keys
