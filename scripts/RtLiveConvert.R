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
  --pop <pop_path>     Path to a .csv with columns state, pop
  --input <input_path> Path to a .csv with columns state, date, cases, deaths, RR, hospi, boost
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
    data_available = col_logical()
  )
)
pd()

ps("Reading input data file {.file {args$input}}")
d_input <- read_csv(
    args$input,
    col_types = cols(
      state  = col_character(),
      date   = col_date(format  = ""),
      cases  = col_double(),
      deaths = col_double(),
      RR     = col_double(),
      hospi  = col_double(),
      boost  = col_double()
    )
  ) %>%
  transmute(
    date, state,
    input_cases  = cases,
    input_deaths = deaths
  )
pd()

ps("Reading state population file {.file {args$pop}}")
d_pop <- read_csv(args$pop, col_types = cols(state = col_character(), pop = col_double()))
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

  cli_alert_info("The following states were optimized and will have synthetic CIs")
  ulid <- cli_ul()
  walk(sort(optimized_states), cli_li)
  cli_end(ulid)
}

ps("Clipping results to g.t.e. {.val 2021-12-01}")
d <- filter(d, date > as.Date('2021-12-01'))
pd()

ps("Joining results to case/death/volume data")
d <- left_join(d, d_input, by = c('date', 'state'))
pd()

d <- filter(d, data_available == TRUE)

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
    "Processing state {stateName}{ifelse(stateWasOptimized, ' (Optimized w/ synthetic intervals)', '')}",
  )

  c("date"                       = "date",
    "r_t"                        = "r_t",
    "r_t_p2_5"                   = "r_t_p2_5",
    "r_t_p97_5"                  = "r_t_p97_5",
    "input_cases"                = "input_cases",
    "fitted_cases"               = "fitted_cases",
    "input_deaths"               = "input_deaths",
    "infections"                 = "infections",
    "infections_p2_5"            = "infections_p2_5",
    "infections_p97_5"           = "infections_p97_5",
    "infections_PC"              = "infections",
    "infections_PC_p2_5"         = "infections_p2_5",
    "infections_PC_p97_5"        = "infections_p97_5",
    "infections_cumulative"      = "infections_cumulative",
    "infections_cumulative_p2_5" = "infections_cumulative_p2_5",
    "infections_cumulative_p97_5"= "infections_cumulative_p97_5",
    "input_cases"                = "input_cases"
  ) -> vars_to_keep

  df <- select_at(df, vars_to_keep)

  df <- mutate(df, date = format(date, '%Y-%m-%d'))
  df <- mutate_at(
    df,
    vars(c("infections", "infections_p2_5", "infections_p97_5")),
    ~. / 7
  )
  df <- mutate_at(
    df,
    vars(starts_with("infections_PC")),
    ~100000/7* . /
      d_pop[[which(d_pop$state == stateName), 'pop']]
  )
  df <- mutate_at(
    df,
    vars(starts_with("infections_cumulative")),
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
