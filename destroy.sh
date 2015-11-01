#!/bin/bash
cd "$(dirname "$0")"
erl -pa ebin/ deps/*/bin -s transform destroy -noshell $@
