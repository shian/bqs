#!/usr/bin/env bash

./rebar compile
erl -pa deps/*/ebin -pa ../bqs/ebin -boot start_sasl -eval "application:ensure_all_started(bqs)" -bqs client_dir \"PATH_TO_BROWSERQUEST_CLIENT\"
