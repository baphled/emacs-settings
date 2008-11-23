ERL=erl
ERLC=erlc
ERLC_OPT=+debug_info -W -o ebin

all: src_src

tests: src_src src_tests
	${ERL} -pa ebin -pa ebin_tests -noshell -eval 'test_suite:test(), halt().'

ebin:
	mkdir ebin

ebin_tests:
	mkdir ebin_tests

src_src: ebin
	cd src;erl -make

src_tests: ebin_tests
	cd tests;erl -make

clean:
	rm -f ebin/*
	rm -f ebin_tests/*