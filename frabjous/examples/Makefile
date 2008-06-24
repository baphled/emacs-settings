# Files
ERL_FILES ?=$(wildcard src/*.erl)
BEAM_FILES ?=$(subst src/,ebin/,$(subst .erl,.beam,${ERL_FILES}))
CODEPATH ?= -pz ../ebin -pz ./ebin
ERL_LIB_ROOT=/opt/lib/erlang/lib

# Compile flags
INCLUDE = -I ./include -I ..
ERLC_FLAGS ?=+debug_info -W -o ./ebin

all: prep remote_call.erl pt_test.erl

clean:
	rm -f ebin/*.beam

prep:
	mkdir -p ebin/

remote_call.erl:
	ERL_LIBS=../.. \
	erlc ${ERLC_FLAGS} ${INCLUDE} src/remote_call.erl

pt_test.erl:
	ERL_LIBS=../.. \
	erlc ${ERLC_FLAGS} ${CODEPATH} ${INCLUDE} src/pt_test.erl
