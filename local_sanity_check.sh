#!/bin/bash

TEMPFILE="temp.preprocessed"

python preprocessor.py ${1} ${TEMPFILE}
./sanity-check.cl ${TEMPFILE} 
rm ${TEMPFILE}

