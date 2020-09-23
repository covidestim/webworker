#!/usr/bin/bash

gzip $1 && \
  aws s3 cp $1.gz s3://covidestim/map-demo.pack.gz \
    --acl public-read \
    --content-encoding gzip && \
  gunzip $1
