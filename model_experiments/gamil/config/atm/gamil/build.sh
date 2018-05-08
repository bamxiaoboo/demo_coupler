#!/bin/bash

export env=${1}
export exedir=${2}
export makefile=${3}
export ntasks=${4}
export nthrds=${5}
export grid=${6}

source $env

cd $exedir/gamil_demo

chmod a+x Makefile
if [ -f "gamil" ]; then
    rm -rf gamil
    make  || exit 1
else
    make  || exit 1
fi
if [ -f "../exe/gamil" ]; then
    rm ../exe/gamil
    cp gamil ../exe/
else
    cp gamil ../exe/
fi

exit 0
