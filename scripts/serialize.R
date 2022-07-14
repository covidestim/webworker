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

args <- docopt(doc, version = '0.3')

cli_process_start("Reading {.file {args$summary_path}}")
d <- read_csv(
  args$summary_path,
  col_types = cols(
    date           = col_date(format  = ""),
    fips           = col_character(),
    data_available = col_logical(),
    .default       = col_number()
  )
)
cli_process_done()

cli_process_start("Clipping to g.t.e {.val 2021-12-01}")
d <- filter(d, date >= as.Date('2021-12-01'))
cli_process_done()

cli_process_start("Reading population file {.file {args$pop_path}}")
pop <- read_csv(
  args$pop,
  col_types = cols_only(
    fips = col_character(),
    pop  = col_number()
  )
)
cli_process_done()

cli_process_start("Computing per-capita infection rates")
d <- select(
  d, fips, date, r_t, infections_cumulative, infections
) %>%
  left_join(pop, by = "fips") %>%
  mutate(
    infections = infections/7,
    infections_PC  = (infections/7) * (100000 / pop)
  ) %>%
  select(-pop)
cli_process_done()

strategy1 <- function(d) {
  mutate(d, r_t = as.integer(round(r_t*100)))
}
 
cli_process_start("Serializing & writing MsgPack to {.file {args$o}}")
writeBin(msgpack_pack(strategy1(d)), args$o)
cli_process_done()
