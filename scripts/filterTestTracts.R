#!/usr/bin/env Rscript

library(docopt)
library(glue)
library(cli)
suppressPackageStartupMessages( library(tidyverse) )

glue('covidestim test-track filterer

Usage:
  {name} -o <output_path> --tracts <tracts_path> --key <key> --metadata <path> --writeMetadata <path> <input_path>
  {name} (-h | --help)
  {name} --version

Options:
  -o <output_path>        Where to save the filtered input data .csv
  --tracts <tracts_path>  Path to a .csv with column "tract". FIPS codes or state names
  --key <key>             Must be "state" or "fips"
  --metadata <path>       Where to find metadata generated during the cleaning process
  --writeMetadata <path>  Where to write metadata generated during the filtering process
  <input_path>            Path to a .csv with columns date, cases, deaths, fracpos
  <summary_path>          The path to the summary .csv produced by a set of runs
  -h --help               Show this screen.
  --version               Show version.
', name = "filterTestTracts.R") -> doc

args <- docopt(doc, version = '0.1')

ps <- cli_process_start
pd <- cli_process_done

# This deals with the fact that the state and county data has different
# structure.
#
# state data (from CTP) has [state, date, cases, deaths, fracpos, volum]
# county data (from JHU) is [fips,  date, cases, deaths]
input_data_colspec <- rlang::list2(
  !!args$key := col_character(), # Trust that the user has passed "state" or "fips"
  date        = col_date(),
  .default    = col_number()
) %>% do.call(cols, .)

ps('Reading list of test tracts from {.file {args$tracts}}')

tracts <- read_csv(
  args$tracts,
  col_types = cols(tract = col_character())
)

pd()
cli_alert_info('{nrow(tracts)} specified in {.file {args$tracts}}')
ps('Reading list of input data')

input_data <- read_csv(args$input_path, col_types = input_data_colspec)

pd()
cli_alert_info('{unique(pull(input_data,args$key))%>%length} tracts in input data')

ps("Reading metadata from {.file {args$metadata}}")
metadata <- jsonlite::read_json(args$metadata, simplifyVector = T)
pd()

ps('Filtering input data')

filtered_input_data <- filter(
  input_data,
  .data[[args$key]] %in% pull(tracts, tract)
)

pd()
cli_alert_info('{unique(pull(filtered_input_data,args$key))%>%length} tracts in filtered data')
ps('Writing filtered data to {.file {args$o}}')

write_csv(filtered_input_data, args$o)

pd()

ps("Writing metadata to {.file {args$writeMetadata}}")
jsonlite::write_json(metadata, args$writeMetadata, null = "null")
pd()
