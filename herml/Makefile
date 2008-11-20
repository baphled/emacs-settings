ERL=erl
ERLC=erlc
ERLC_OPT=+debug_info -W -o ebin

all: compile

tests: compile compile_tests
	${ERL} -pa ebin -noshell -eval 'herml_test_suite:test(), halt().'

ebin:
	mkdir ebin

compile: ebin src/herml_lexer.xrl
	${ERLC} ${ERLC_OPT} src/*.erl

compile_tests: ebin
	${ERLC} ${ERLC_OP} tests/*.erl

clean:
	rm -f ebin/*
	rm -f src/herml_lexer.erl
	rm -f src/herml_parser.erl

leex_compile:
	${ERLC} ${ERLC_OPT} src/leex.erl

src/herml_lexer.xrl: leex_compile
	${ERL} -pa ebin -noshell -eval 'leex:file("src/herml_lexer.xrl", [{outdir, "src"}]), halt().'

src/herml_parser.yrl: leex_compile
	${ERL} -noshell -eval 'c:cd("./src"), yecc:file(herml_parser), halt().'