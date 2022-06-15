#!/usr/bin/env Rscript
suppressPackageStartupMessages( library(tidyverse) )
library(splines)
library(docopt)
library(cli)
library(glue, warn.conflicts=F)

logit <-function(p) log(p/(1-p))
invlogit <- function(q) exp(q)/(exp(q)+1)

glue('covidestim synthetic-interval generator

Usage:
  {name} -o <path> --statepop <path> [--backup <path> --writeBackup <path> --vars <vars> --minSampled <num> --metadata <path> --writeMetadata <path>] <result_path>
  {name} (-h | --help)
  {name} --version

Options:
  -o <path>               Where to output the resulting .csv
  --statepop <path>       Path of a .csv file [state,pop]
  --backup <path>         Where to locate the archived LM objects, should they be needed
  --writeBackup <path>    Where to write an archive of LM objects, for future use
  --vars <vars>           A comma-delimited list of variables to generate CIs for [default: infections,r_t,infections_cumulative]
  --minSampled <num>      The minimum number of sampled tracts needed to run LM [default: 10]
  --metadata <path>       Path to .json of metadata
  --writeMetadata <path>  Where to write metadata generated during synthetic interval process
  -h --help               Show this screen.
  --version               Show version.
', name = "makeSyntheticIntervals.R") -> doc

args <- docopt(doc, version = '0.1')

ps <- cli_process_start
pd <- cli_process_done

if (!is.null(args$metadata)) {
  ps("Reading metadata from {.file {args$metadata}}")
  metadata <- jsonlite::read_json(args$metadata, simplifyVector = T)
  pd()
} else {
  cli_alert_warning("No {.code --metadata} flag passed")
}

ps("Reading results file {.file {args$result_path}}")
d <- read_csv(
  args$result_path,
  col_types = cols(
    date = col_date(format = '%Y-%m-%d'),
    state = col_character(),
    data_available = col_logical(),
    .default = col_number()
  )
) # Load the results csv file
pd()

ps("Reading state population size file {.file {args$statepop}}")
pop <- read_csv(
  args$statepop,
  col_types = cols(state = col_character(), pop = col_number())
)
pd()

arch       <- args$backup # archive filepath
CIvars     <- str_split(args$vars, ',')[[1]]
K          <- as.numeric(args$minSampled) # minimum number of sampled states
dfree      <- 50
outfile    <- args$o
usedBackup <- FALSE # Stores whether or not an RDS LM archive was loaded

## another way to learn which states sampled and which states did not?
SampledStates <- unique((d %>% filter(!is.na(infections_p2_5)))$state)

cli_alert_info("{.val {length(SampledStates)}} sampled from this run")

lm_p97_5 <- lm_p2_5 <- NULL

Bounds     <- 300
n.ends     <- Bounds/3
fixdays    <- 7
dayknots   <- round(c(1:fixdays, seq(fixdays+1, Bounds-1, length.out = (dfree - 1 - fixdays))))

y <- d %>% 
  left_join(pop, by = "state") %>%
  mutate(
    infections_cumulative       = infections_cumulative/pop,
    infections_cumulative_p2_5  = infections_cumulative_p2_5/pop,
    infections_cumulative_p25   = infections_cumulative_p25/pop,
    infections_cumulative_p75   = infections_cumulative_p75/pop,
    infections_cumulative_p97_5 = infections_cumulative_p97_5/pop,
  ) %>%
  select(state, date, pop, starts_with(CIvars)) %>%
  group_by(state)%>%
  # replace any zeros with lowest values
  mutate_at(
    vars(starts_with(CIvars)),
    function(x) {
      if_else(x == 0, x + min(x[x > 0])/2, x)
    }
  ) %>%
  mutate(
    days = as.numeric(max(date) - date) + 1,

    # transform time: first n.ends and last n.ends
    time = if_else(
      days <= n.ends,
      days,
      if_else(
        (max(days) - days) < n.ends,
        days - max(days) + Bounds,
        n.ends + (n.ends/(max(days)-2*n.ends))*(days-n.ends)
      )
    )
  ) %>%
  ungroup() %>%
  mutate_at(vars(starts_with("infections_cumulative")),  ~logit(.)) %>%
  mutate_at(
    vars(
      -state, -date, -pop, -time, -days, -starts_with("infections_cumulative")
    ),
    log
  )

# If at least `--minSampled` states sampled, proceed with fitting
if (length(SampledStates) > K) {
  for (j in CIvars) {
    y_p97_5 <- y[[j]] - y[[paste0(j, "_p97_5")]]
    y_p2_5  <- y[[j]] - y[[paste0(j, "_p2_5")]]

    lm_p97_5[[j]] <- lm(
      y_p97_5 ~ splines::ns(
        y$time, df = dfree, Boundary.knots = c(0,Bounds), knots = dayknots
      )
    )

    lm_p2_5[[j]] <- lm(
      y_p2_5 ~ splines::ns(
        y$time, df = dfree, Boundary.knots = c(0,Bounds), knots = dayknots
      )
    )
  }

  cli_alert_info("Finished fitting _p97_5/_p2_5 LMs")

  if (!is.null(args$writeBackup)) {
    cli_alert_info("Writing LM archive to {.file {args$writeBackup}}")
    saveRDS(list("lm_p97_5" = lm_p97_5,"lm_p2_5" = lm_p2_5), args$writeBackup)
  }

} else if (!is.null(arch)) { # Otherwise, load archived LM objects, if possible
  cli_alert_warning("Too few states sampled ({.val {length(SampledStates)}}), instead loading backup LM objects from {.file {arch}}")
  res <- readRDS(arch)
  lm_p97_5 <- res$lm_p97_5
  lm_p2_5  <- res$lm_p2_5
  usedBackup <- TRUE
} else { # Otherwise, error and exit
  cli_alert_danger("Too few states sampled ({.val {length(SampledStates)}}) and no {.code --backup}. Exiting")
  quit(status=1)
}

# Compute new CIs
for (j in CIvars) {
  cli_alert_info("Computing CIs for variable {.code {j}}")
  if(j == "infections_cumulative"){
    pred_p97_5 <- y$pop * invlogit(
      y[[j]] -
        predict(
          lm_p97_5[[j]],
          data.frame(splines::ns(
            y$time, df = dfree, Boundary.knots = c(0,Bounds), knots = dayknots
          ))
        )
    )

    pred_p2_5 <- y$pop * invlogit(
      y[[j]] -
        predict(
          lm_p2_5[[j]],
          data.frame(splines::ns(
            y$time, df = dfree, Boundary.knots = c(0,Bounds), knots = dayknots
          ))
        )
    )

  } else {
    pred_p97_5 <- exp(y[[j]] -
      predict(
        lm_p97_5[[j]],
        splines::ns(
          y$time, df = dfree, Boundary.knots = c(0,300), knots = dayknots
        )
      )
    )

    pred_p2_5 <- exp(y[[j]] - 
      predict(
        lm_p2_5[[j]],
        splines::ns(
          y$time, df = dfree, Boundary.knots = c(0,300), knots = dayknots
        )
      )
    )
  }

  col_name_p97_5 <- glue("{j}_p97_5")
  col_name_p2_5  <- glue("{j}_p2_5")

  # Only replace confidence intervals if they are actually missing! Otherwise
  # all states which sampled will be overwritten by the synthetic intervals.
  d[col_name_p97_5] <- ifelse(is.na(d[[col_name_p97_5]]), pred_p97_5, d[[col_name_p97_5]])
  d[col_name_p2_5]  <- ifelse(is.na(d[[col_name_p2_5]]),  pred_p2_5,  d[[col_name_p2_5]])
}

cli_alert_success("Computed all CIs")

# All variables from covidestim output which have uncertainty interval variables
allCIVars <- colnames(d)[which(str_detect(colnames(d), '_p2_5'))] %>% str_remove('_p2_5')

# All variables which will have missing CIs if the state's results are from
# BFGS
allMissingVars <- setdiff(allCIVars, CIvars)

metadata <- mutate(
  metadata,
  # "mixed" refers to the fact that several but NOT all intervals are
  # synthetically generated (all other intervals, in a non sampled run, are 
  # still NA-valued)
  intervalType = ifelse(state %in% SampledStates, "sampled", "mixed"),
  syntheticIntervalDetails = map( # Add to the JSON object for each state
    state,
    function(state) {

       # Don't add a value to this key if the state sampled
      if (state %in% SampledStates) 
        return(NULL);

      list(
        missingIntervals = allMissingVars,

        # When using `auto_unbox` with `toJSON`, make sure all possibly-1-length
        # vectors are enclosed in I() so that they always are represented as
        # arrays.
        syntheticIntervals = I(CIvars),
        sampledIntervals = character(0),
        usedBackup = usedBackup,
        numSampled = ifelse(usedBackup, NA, length(SampledStates))
      )
    }
  )
)

if (!is.null(args$writeMetadata)) {
  ps("Writing metadata to {.file {args$writeMetadata}}")
  jsonlite::write_json(metadata, args$writeMetadata, null = "null", auto_unbox = TRUE)
  pd()
} else {
  cli_alert_warning("{.code --writeMetadata} not passed; skipping metadata write")
}

ps("Writing new summary CSV to {.file {outfile}}")
write_csv(d, outfile)
pd()
