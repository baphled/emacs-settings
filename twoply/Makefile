PORT=6513
APP_NAME=twoply

all: dev compile

dev: src/twoply.hrl ebin/twoply.app

clean:
	rm -rf ebin
	rm src/twoply.hrl

ebin:
	mkdir ebin

compile: ebin
	erl -make

src/twoply.hrl:
	sed 's/__APP_NAME__/${APP_NAME}/' < src/twoply.hrl.in > src/twoply.hrl

ebin/twoply.app: ebin
	sed 's/__PORT__/${PORT}/' < src/twoply.app.in > ebin/twoply.app
