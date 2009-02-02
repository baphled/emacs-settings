#!/bin/sh
cd `dirname $0`
exec erl -mnesia dir '"/tmp/mnesia"' -pa $PWD/ebin $PWD/deps/*/ebin -boot start_sasl -s reloader -s tokenizer
