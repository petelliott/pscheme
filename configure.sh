#!/bin/sh

PREFIX="/usr/local"

# configure local pschemes
script_dir=$(dirname $(realpath $0))
localconfig="(\"-I${script_dir}/scm\" \"-L${script_dir}/runtime/pscheme-runtime.a\")"

echo $localconfig > .pscheme1-pscmconfig
echo $localconfig > .pscheme-pscmconfig
echo $localconfig > .pscheme-dev-pscmconfig

# configure install pscheme

echo "(\"-I${PREFIX}/share/pscheme\" \"-L${PREFIX}/lib/pscheme-runtime.a\")" > .pscheme-prefix-pscmconfig

rm install-list
echo "./runtime/pscheme-runtime.a ${PREFIX}/lib/pscheme-runtime.a" >> install-list
echo "./pscheme ${PREFIX}/bin/pscheme" >> install-list
echo ".pscheme-prefix-pscmconfig ${PREFIX}/bin/.pscheme-pscmconfig" >> install-list

for libfile in `(cd scm && find -name '*.scm')`; do
    echo "scm/${libfile} ${PREFIX}/share/pscheme/${libfile}" >> install-list
    echo "scm/${libfile}.o ${PREFIX}/share/pscheme/${libfile}.o" >> install-list
done
