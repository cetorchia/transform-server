#!/bin/bash
#
# Wrapper for transformer worker
#

erl -sname transformer_worker -eval 'transformer_worker:start().'
