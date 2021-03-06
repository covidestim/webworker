#!/usr/bin/env Rscript

suppressPackageStartupMessages( library(tidyverse) )
library(jsonlite, warn.conflicts = F)
library(docopt)
library(glue, warn.conflicts = F)
library(RcppMsgPack)
library(cli)

glue('covidestim RtLiveConverter

Usage:
  {name} -o <output_path> --pop <pop_path> --input <input_path> [--method <csv>] <summary_path>
  {name} (-h | --help)
  {name} --version

Options:
  --method <csv>       Specify fit method (sampler/optimizer) as .csv [state,method]
  -o <output_path>     Where to save the .pack file
  --pop <pop_path>     Path to a .csv with columns fips,pop
  --input <input_path> Path to a .csv with columns date, cases, deaths, fracpos
  <summary_path>       The path to the summary .csv produced by a set of runs
  -h --help            Show this screen.
  --version            Show version.
', name = "RtLiveConvert.R") -> doc

args <- docopt(doc, version = '0.1')
ps <- cli_process_start
pd <- cli_process_done

ps("Reading summary file {.file {args$summary_path}}")
d <- read_csv(
  args$summary_path,
  col_types = cols(
    .default = col_double(),
    state = col_character(),
    date = col_date(format = ""),
    data.available = col_logical()
  )
)
pd()

ps("Reading input data file {.file {args$input}}")
d_input <- read_csv(
    args$input,
    col_types = cols(
      date = col_date(format = ""),
      state = col_character(),
      cases = col_double(),
      deaths = col_double(),
      fracpos = col_double(),
      volume = col_double()
    )
  ) %>%
  transmute(date, state,
            input_cases = cases,
            input_deaths = deaths,
            input_volume = round(input_cases / fracpos))
pd()

ps("Reading state population file {.file {args$pop}}")
d_pop <- read_csv(args$pop, col_types = cols( state = col_character(), pop = col_double()))
pd()

# By default we assume no states were optimized
optimized_states <- character()

if (!identical(args$method, FALSE)) {
  cli_alert_info("--method flag passed, will be setting fake CIs")
  ps("Reading fitting-method file {.file {args$method}}")
  method <- read_csv(
    args$method,
    col_types = cols(state = col_character(), method = col_character())
  )
  pd()

  optimized_states <- filter(method, method == 'optimizer') %>% pull(state)

  cli_alert_info("The following states were optimized and will have fake CIs")
  ulid <- cli_ul()
  walk(sort(optimized_states), cli_li)
  cli_end(ulid)
}

ps("Joining results to case/death/volume data")
d <- left_join(d, d_input, by = c('date', 'state'))
pd()

d <- filter(d, data.available == TRUE)

# Split each state into its own group, then split each group into its own df
d_split <- d %>% group_by(state) %>% group_split()

# Get a list of the statenames in the split representation. These will then
# be used to key this list of df's
d_statenames <- map_chr(d_split, ~unique(.$state))

# Key the list
d_indexed <- d_split %>% setNames(d_statenames)

# Remove unneeded information and transpose
process_state <- function(df, stateName) {

  stateWasOptimized <- stateName %in% optimized_states

  ps(
    "Processing state {stateName}{ifelse(stateWasOptimized, ' (Optimized)', '')}",
  )

  c("date"           = "date",
    "r0"             = "Rt",
    "r0_l80"         = "Rt.lo",
    "r0_h80"         = "Rt.hi",
    "cases_new"      = "input_cases",
    "corr_cases_new" = "cases.fitted",
    "tests_new"      = "input_volume",
    "deaths_new"     = "input_deaths",
    "onsets"         = "infections",
    "onsets_l95"     = "infections.lo",
    "onsets_h95"     = "infections.hi",
    "onsetsPC"       = "infections",
    "onsetsPC_l95"   = "infections.lo",
    "onsetsPC_h95"   = "infections.hi",
    "cumulative"     = "cum.incidence",
    "cumulative_l95" = "cum.incidence.lo",
    "cumulative_h95" = "cum.incidence.hi",
    "corr_cases_raw" = "input_cases"
  ) -> vars_to_keep

  if (stateWasOptimized)
    df <- mutate(
      df,
      Rt.lo            = Rt,
      Rt.hi            = Rt,
      infections.lo    = infections,
      infections.hi    = infections,
      cum.incidence.lo = cum.incidence,
      cum.incidence.hi = cum.incidence
    )

  df <- select_at(df, vars_to_keep)

  df <- mutate(df, date = format(date, '%Y-%m-%d'))
  df <- mutate_at(
    df,
    vars(starts_with("onsetsPC")),
    ~100000* . /
      d_pop[[which(d_pop$state == stateName), 'pop']]
  )
  df <- mutate_at(
    df,
    vars(starts_with("cumulative")),
    ~100 * . /
      d_pop[[which(d_pop$state == stateName), 'pop']]
  )

  result <- transpose(df)
  
  pd()

  result
}

state_abbrs <- state.abb
names(state_abbrs) <- state.name
state_abbrs = c(
  state_abbrs,
  "District of Columbia" = "DC",
  "Puerto Rico" = "PR"
)

restructure_state <- function(lst, state_name) {
  list(
    identifier = state_abbrs[state_name],
    series     = lst,
    population = d_pop[[which(d_pop$state == state_name), 'pop']]
  )
}

d_transposed      <- imap(d_indexed, process_state)
d_withtags        <- imap(d_transposed, restructure_state)  
names(d_withtags) <- state_abbrs[names(d_withtags)]

list(
  state_data = d_withtags,
  last_updated_ts = 1e3*(lubridate::now() %>% as.POSIXct %>% as.numeric),
  last_r0_date = d$date[length(d$date)] %>% format('%Y-%m-%d')
) -> final

ps("Writing Messagepack to {.file {args$o}}")
writeBin(
  msgpack_simplify(final) %>% msgpack_pack,
  con = args$o
)
pd()

cli_alert_success("Done!")
