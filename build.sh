#!/bin/bash

racket make-page-tree.rkt
raco pollen render index.ptree
#raco pollen publish ./ ../build
