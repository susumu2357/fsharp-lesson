#!/bin/sh

rm -rf database/master
mkdir -p database/master
cp ../data/シラバス.csv ../data/wikipedia/*.csv ../data/tandp/*.csv database/master
ls database/master