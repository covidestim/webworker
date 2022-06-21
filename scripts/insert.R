#!/usr/bin/Rscript
'Covidestim DB inserter

usage:
  insert.R --input <path> --summary <path> [--method <path>] --metadata <path> --key <string> --run-date <date> [--save-mapping <path>]
  insert.R (-h | --help)
  insert.R --version

Options:
  --input <path>    Path to a CSV, [state/fips, date, cases, deaths, RR, hospi, boost]
  --summary <path>  Path to a CSV, [state/fips, date, ...rest of Covidestim schema]
  --method <path>   Path to a CSV, [state/fips, method = sampling|optimizing]
  --metadata <path>  Path to a JSON of metadata generated earlier in the pipeline
  --key <string>    Must be "state" or "fips" - the grouping key for the data
  --run-date <string>  YYYY-MM-DD
  --save-mapping <path>  Where to save a CSV mapping [geo_name, run_id]
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

api_response_colspec <- cols(run_id = col_number())

# The database doesn't use `fips` or `state` - instead, it uses a column
# `geo_name` for both types of geographies. This functions renames
# the `state`/`fips` column to `geo_name`.
rename_to_geo_name <- function(df) rename(df, geo_name = !!args$key)

cli_alert_info("Loading {.code input_df} from {.file {args$input}}")
input_df <- read_csv(args$input, col_types = input_df_colspec)
cli_alert_info("Loading {.code summary_sf} from {.file {args$summary}}")
summary_df <- read_csv(args$summary, col_types = summary_df_colspec)
cli_alert_info("Loading {.code metadata_df} from {.file {args$metadata}}")
metadata_df <- jsonlite::read_json(args$metadata, simplifyVector = T)

input_df    <- rename_to_geo_name(input_df)
summary_df  <- rename_to_geo_name(summary_df)
metadata_df <- rename_to_geo_name(metadata_df)

if (!is.null(args$method)) {
  cli_alert_info("Loading {.code method_df} from {.file {args$method}}")
  method_df <- read_csv(args$method, col_types = method_df_colspec)
  method_df <- rename_to_geo_name(method_df)
}

# Get a nested tibble of all the data for each geography. The result of this
# is a tibble with columns `geo_name` and `input`, where `input` is itself
# a tibble, with all columns except `geo_name` present.
input_df    <- nest(input_df,    input    = !geo_name)
summary_df  <- nest(summary_df,  summary  = !geo_name)
metadata_df <- nest(metadata_df, metadata = !geo_name)

# `geo_type` in the db is either "state" or "county", but `--key` passes
# "state" or "fips".
geo_type_from_key <- switch(args$key, "state" = "state", "fips" = "county")

attach_method <- function(df) {
  if (is.null(args$method))
    return(mutate(df, method = "optimizing"))

  left_join(df, method_df, by = "geo_name")
}

# Produce a tibble where each row contains all the fields needed for the
# RPC call
one_row_per_run <- summary_df %>%
  left_join(input_df,    by = "geo_name") %>%
  left_join(metadata_df, by = "geo_name") %>%
  attach_method(.) %>%
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
    
# API endpoint must be passed as an environment variable.
get_endpoint <- function() {
  endpoint <- Sys.getenv('COVIDESTIM_ENDPOINT')
  if (identical(endpoint, "")) {
    stop("Please set env var COVIDESTIM_ENDPOINT to your Covidestim API endpoint.")
  }

  endpoint
}

# JWT token that gives privileges for executing the `insert_run` function must
# be passed as an environment variable.
get_token <- function() {
  token <- Sys.getenv('COVIDESTIM_JWT')
  if (identical(token, "")) {
    stop("Please set env var COVIDESTIM_JWT to your Covidestim API JWT")
  }

  token
}

response <- POST(
  get_endpoint(),
  add_headers(
    Authorization = paste0("Bearer ", get_token()),

    # This allows us to call the `insert_run` function for each row of
    # `one_row_per_run`.
    Prefer = "params=multiple-objects",
    Accept = "text/csv"
  ),
  user_agent("https://github.com/covidestim/webworker"),
  body = one_row_per_run,
  encode = "json" # NOTE: auto_unbox = TRUE for this... see jsonlite docs
)

if (status_code(response) != 200) {
  stop(
    sprintf(
      "Covidestim API request failed [%s]\n%s\n", 
      status_code(response),
      content(response)
    ),
    call. = FALSE
  )
}

cli_alert_success("Call to {.url {get_endpoint()}} succeeded")

if (!is.null(args$save_mapping)) {
  # save the mapping from geo_name to run_id
  cli_alert_info("Saving mapping to {.file {args$save_mapping}}")
  select(one_row_per_run, geo_name) %>%
    bind_cols(content(response, col_types = api_response_colspec)) %>%
    write_csv(args$save_mapping)
}
