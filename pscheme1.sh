#!/usr/bin/env bash

script_dir=$(dirname $(realpath $0))

#gdb --args $script_dir/pscheme1 -I$script_dir/scm -L$script_dir/runtime/runtime.a $@
$script_dir/pscheme1 -I$script_dir/scm -L$script_dir/runtime/runtime.a $@
