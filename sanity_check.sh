#!/bin/bash


CHECKER_DIR="../ulf_sanity_checker"

CURDIR=$(pwd)

TEMPFILE="temp.preprocessed"

cd $CHECKER_DIR

python preprocessor.py ${1} ${TEMPFILE}
./sanity-check.cl ${TEMPFILE} 
rm ${TEMPFILE}

cd $CURDIR

