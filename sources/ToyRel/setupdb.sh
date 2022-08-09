#!/bin/sh

rm -rf database/master database/library database/glossary
mkdir -p database/master database/library database/glossary
cp ../data/シラバス.csv ../data/wikipedia/*.csv  database/master
cp ../data/tandp/book.csv ../data/tandp/index.csv ../data/tandp/subject.csv ../data/tandp/auction.csv database/library
cp ../data/tandp/goods.csv ../data/tandp/delivery.csv ../data/tandp/stock.csv database/glossary
