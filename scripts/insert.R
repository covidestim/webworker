#!/usr/bin/Rscript
'Covidestim DB inserter

Usage:
  insert.R --input <path> --summary <path> --method <path> --key <string> --run-date <date>
  insert.R (-h | --help)
  insert.R --version

Options:
  --input <path>    Path to a CSV, [state/fips, date, cases, deaths, RR, hospi, boost]
  --summary <path>  Path to a CSV, [state/fips, date, ...rest of Covidestim schema]
  --method <path>   Path to a CSV, [state/fips, method = sampling|optimizing]
  --key <string>    Must be "state" or "fips" - the grouping key for the data
  --run-date <string>  YYYY-MM-DD
  -h --help  Show this screen.
  --version  Show version.

' -> doc

suppressPackageStartupMessages({
  library(docopt)
  library(httr)
  library(cli)
  library(tidyr)
  library(dplyr)
  library(readr)
  library(magrittr)
})

args <- docopt(doc, version = 'insert.R 0.1')

# The column that identifies unique geographies is going to be named either
# "state" or "fips". The user tell us which by passing "state" or "fips" to
# the `--key` CLI argument. We use that information to create custom specs
# for the 3 CSV files we need to import here.
#
# `rlang::list2` is used here because it supports the nonstandard evaluation
# necessary to do this. Once the list has been produced, it's passed to
# `cols`.
input_df_colspec <- rlang::list2(

  # Trust that the user has passed "state" or "fips"
 !!args$key := col_character(), 

  date       = col_date(),
  cases      = col_number(),
  deaths     = col_number(),
  RR         = col_number(),
  hospi      = col_number(),
  boost      = col_number()
) %>% do.call(cols, .)

summary_df_colspec <- rlang::list2(
  !!args$key    := col_character(),
  date           = col_date(),
  data_available = col_logical(),
  .default       = col_number()
) %>% do.call(cols, .)

method_df_colspec <- rlang::list2(
  !!args$key := col_character(),
  method      = col_character()
) %>% do.call(cols, .)

# The database doesn't use `fips` or `state` - instead, it uses a column
# `geo_name` for both types of geographies. This functions renames
# the `state`/`fips` column to `geo_name`.
rename_to_geo_name <- function(df) rename(df, geo_name = !!args$key)

input_df   <- read_csv(args$input,   col_types = input_df_colspec)
summary_df <- read_csv(args$summary, col_types = summary_df_colspec)
method_df  <- read_csv(args$method,  col_types = method_df_colspec)

input_df   <- rename_to_geo_name(input_df)
summary_df <- rename_to_geo_name(summary_df)
method_df  <- rename_to_geo_name(method_df)

# Get a nested tibble of all the data for each geography. The result of this
# is a tibble with columns `geo_name` and `input`, where `input` is itself
# a tibble, with all columns except `geo_name` present.
input_df   <- nest(input_df,   input   = !geo_name)
summary_df <- nest(summary_df, summary = !geo_name)

# `geo_type` in the db is either "state" or "county", but `--key` passes
# "state" or "fips".
geo_type_from_key <- switch(args$key, "state" = "state", "fips" = "county")

# Produce a tibble where each row contains all the fields needed for the
# RPC call
rowwise <- summary_df %>%
  left_join(input_df,  by = "geo_name") %>%
  left_join(method_df, by = "geo_name") %>%
  mutate(
    run_date = args$run_date,
    geo_type = geo_type_from_key,
    
    # Small difference in DB schema terminology vs. `method.csv` terminology
    method = case_when(
      method == "optimizer" ~ "optimizing",
      method == "sampler"   ~ "sampling",

      # Fallback to handle the case where we adopy the DB terminology across
      # the Nextflow repo.
      TRUE                  ~ method
    )
  )
    
print(rowwise)

# JWT token that gives privileges for executing the `insert_run` function must
# be passed as an environment variable.
get_token <- function() {
  token <- Sys.getenv('COVIDESTIM_JWT')
  if (identical(token, "")) {
    stop("Please set env var COVIDESTIM_JWT to your Covidestim API JWT")
  }

  token
}

# Hardcoded test endpoint. In the future this will be accessed via an env
# variable.
endpoint <- "http://localhost:3000/rpc/insert_run"

response <- POST(
  endpoint,
  add_headers(
    Authorization = paste0("Bearer ", get_token()),

    # This allows us to call the `insert_run` function for each row of
    # `rowwise`.
    Prefer = "params=multiple-objects"
  ),
  body = rowwise,
  encode = "json" # NOTE: auto_unbox = TRUE for this... see jsonlite docs
)

print(response)
print(content(response))

# handle weird codes
# return the mapping from geo_name to run_id
