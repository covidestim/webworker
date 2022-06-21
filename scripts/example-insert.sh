#!/bin/bash

export COVIDESTIM_JWT=$(cat ../../db/new-schema/AUTH_TOKEN)
export COVIDESTIM_ENDPOINT=http://localhost:3000/rpc/insert_run

./insert.R \
  --input input.csv \
  --summary summary.csv \
  --key state \
  --run-date $1 \
  --save-mapping mapping.csv \
  --metadata metadata.json
