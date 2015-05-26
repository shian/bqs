erl -pa deps/*/ebin -pa ../bqs/ebin -boot start_sasl -eval "application:ensure_all_started(bqs)"
