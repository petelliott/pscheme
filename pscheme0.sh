#!/usr/bin/env bash

script_dir=$(dirname $(realpath $0))

gosh -r7 -A$script_dir/scm -- $script_dir/scm/pscheme/compiler/main.scm -I$script_dir/scm -L$script_dir/runtime/runtime.a $@
