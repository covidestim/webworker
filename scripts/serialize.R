#!/usr/bin/env Rscript

suppressPackageStartupMessages( library(tidyverse) )
library(glue, warn.conflicts = F)
library(cli)
library(lubridate)
library(RcppMsgPack)
library(docopt)

glue('covidestim county run serializer

Usage:
  {name} -o <output_path> --pop <pop_path> <summary_path>
  {name} (-h | --help)
  {name} --version

Options:
  -o <output_path>   Where to save the .pack file
  --pop <pop_path>   Path to a .csv with columns fips,pop
  <summary_path>     The path to the summary .csv produced by an all-county run
  -h --help          Show this screen.
  --version          Show version.
', name = "serialize.R") -> doc

args <- docopt(doc, version = '0.2')

cli_process_start("Reading {.file {args$summary_path}}")
d <- read_csv(
  args$summary_path,
  col_types = cols(
    date = col_date(format = ""),
    fips = col_character(),
    data.available = col_logical(),
    .default = col_number()
  )
)
cli_process_done()

cli_process_start("Reading population file {.file {args$pop_path}}")
pop <- read_csv(
  args$pop,
  col_types = cols_only(
    fips = col_character(),
    pop = col_number()
  )
)
cli_process_done()

cli_process_start("Computing per-capita infection rates")
d <- select(d, fips, date, Rt, 
            seroprevalence = cum.incidence, infections) %>%
  filter(date == max(date) || wday(date) == 1) %>%
  left_join(pop, by = "fips") %>%
  mutate(infectionsPC = infections * 100000 / pop,
         seroprevalence = 100*seroprevalence/pop) %>%
  select(-pop)
cli_process_done()

strategy1 <- function(d) {
  mutate(d, Rt = as.integer(round(Rt*100)))
}
 
strategy2 <- function(d) {
  strategy1(d) %>%
  mutate(
    date = map(date, ~msgpack_timestamp_encode(as.POSIXct(.)))
  )
}

cli_process_start("Serializing & writing MsgPack to {.file {args$o}}")
writeBin(msgpack_pack(strategy1(d)), args$o)
cli_process_done()
