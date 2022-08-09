#!/bin/sh

rm -rf database/master database/tandp
mkdir -p database/master database/tandp
cp ../data/シラバス.csv ../data/wikipedia/*.csv  database/master
cp ../data/tandp/*.csv database/tandp