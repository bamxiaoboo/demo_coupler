#!/bin/bash

export env=${1}
export exedir=${2}
export makefile=${3}
export ntasks=${4}
export nthrds=${5}
export grid=${6}

source $env

cd $exedir/licom_demo

chmod a+x Makefile
if [ -f "licom" ]; then
    rm -rf licom
    make  || exit 1
else
    make  || exit 1
fi
if [ -f "../exe/licom" ]; then
    rm ../exe/licom
    cp licom ../exe/
else
    cp licom ../exe/
fi

exit 0
