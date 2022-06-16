#!/bin/bash

export COVIDESTIM_JWT=$(cat ../../db/new-schema/AUTH_TOKEN)

./insert.R \
  --input input.csv \
  --summary summary.csv \
  --key state \
  --method method.csv \
  --run-date 1950-01-01
