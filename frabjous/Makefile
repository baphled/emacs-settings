# Files
ERL_FILES ?=$(wildcard src/*.erl)
BEAM_FILES ?=$(subst src/,ebin/,$(subst .erl,.beam,${ERL_FILES}))
TEST_ERL_FILES ?=$(wildcard src/test/*.erl)
TEST_BEAM_FILES ?=$(subst src/test/,ebin/,$(subst .erl,.beam,${TEST_ERL_FILES}))
CODEPATH ?= -pz ./ebin
ERL_LIB_ROOT=/opt/lib/erlang/lib
DIALYZER_APPS=${ERL_LIB_ROOT}/stdlib*/ebin ${ERL_LIB_ROOT}/crypto*/ebin ${ERL_LIB_ROOT}/kernel*/ebin ${ERL_LIB_ROOT}/mnesia*/ebin ${ERL_LIB_ROOT}/sasl*/ebin ${ERL_LIB_ROOT}/sasl*/ebin ${ERL_LIB_ROOT}/inets*/ebin ${ERL_LIB_ROOT}/compiler*/ebin

# Compile flags
INCLUDE = -I ./include -I ..
ERLC_FLAGS ?=+debug_info -W -o ./ebin

all: ${BEAM_FILES} ${TEST_BEAM_FILES}

dist: all
	rm -f ebin/test*.beam

test: all
	rm -f /tmp/*.beam
	ERL_LIBS=".." \
	erl -pz ebin -eval "test_frabjous:test()." -s init stop -noshell
clean:
	rm -f ebin/*.beam
	rm -rf Mnesia*

analyze: project.plt all
	dialyzer --plt project.plt -r ./ebin

project.plt:
	dialyzer --plt project.plt --build_plt -r ${DIALYZER_APPS}

ebin/%.beam: src/%.erl
	ERL_LIBS=".." \
	erlc ${ERLC_FLAGS} ${INCLUDE} $<

ebin/test_%.beam: src/test/test_%.erl
	ERL_LIBS=".." \
	erlc ${ERLC_FLAGS} ${INCLUDE} $<