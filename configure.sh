#!/bin/sh

# configure local pschemes
script_dir=$(dirname $(realpath $0))
localconfig="(\"-I${script_dir}/scm\" \"-L${script_dir}/runtime/runtime.a\")"

echo $localconfig > .pscheme1-pscmconfig
echo $localconfig > .pscheme-pscmconfig
echo $localconfig > .pscheme-dev-pscmconfig
