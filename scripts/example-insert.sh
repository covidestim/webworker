#!/bin/bash

export COVIDESTIM_JWT=$(cat ../../db/new-schema/AUTH_TOKEN)

./insert.R \
  --input input.csv \
  --summary summary.csv \
  --key state \
  --method method.csv \
  --run-date $1 \
  --endpoint http://localhost:3000/rpc/insert_run \
  --save-mapping mapping.csv \
  --metadata metadata.json
