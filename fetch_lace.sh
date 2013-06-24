#!/bin/bash

set -e


cd Code
hg clone  https://hg.assembla.com/openada.8  Lace

CODE_FOLDER=`pwd`

echo
echo Lace is installed.
echo
echo "  Please append the following lines to your ~/.bashrc (or equivalent)."
echo
echo "  " export LACE=$CODE_FOLDER/Lace
echo "  " source "$"LACE/lace-gpr_paths.sh
echo