#!/bin/bash
cd "$(dirname "$0")"
erl -pa ebin/ deps/*/ebin -s transform create -noshell $@
