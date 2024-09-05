#! /bin/sh

raco cross --version 8.13 setup -l choret
raco cross --version 8.13 scribble --html ../choret/choret-doc/choret-doc.scrbl
mv choret-doc.html index.html
