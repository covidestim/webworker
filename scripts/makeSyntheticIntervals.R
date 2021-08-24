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
  --vars <vars>           A comma-delimited list of variables to generate CIs for [default: infections,Rt,cum.incidence]
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
    data.available = col_logical(),
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
SampledStates <- unique((d %>% filter(!is.na(infections.hi)))$state)

cli_alert_info("{.val {length(SampledStates)}} sampled from this run")

lm.hi <- lm.lo <- NULL

y <- d %>% 
  left_join(pop, by = "state") %>%
  mutate(cum.incidence = cum.incidence/pop,
         cum.incidence.hi = cum.incidence.hi/pop,
         cum.incidence.lo = cum.incidence.lo/pop) %>%
  select(state, date, starts_with(CIvars)) %>%
  group_by(state)%>%
  # replace any zeros with lowest values
  mutate_at(vars(starts_with(CIvars)), function(x){if_else(x == 0,
                                 x + 
                                   min(x[x > 0])/2,
                                 x)}) %>%
  mutate(time = as.numeric(date - min(date)) + 1,
         reltime = time / abs(min(time))) %>%
  ungroup() %>%
 mutate_at(vars(starts_with("cum.incidence")),  ~logit(.)) %>%
  mutate_at(vars(-state, -date, -starts_with("cum.incidence")), log) 


# If at least `--minSampled` states sampled, proceed with fitting
if (length(SampledStates) > K) {
  for (j in CIvars) {
    y.hi <- y[[j]] - y[[paste0(j, ".hi")]]
    y.lo <- y[[j]] - y[[paste0(j, ".lo")]]
    lm.hi[[j]] <- lm(y.hi ~ splines::ns(y$reltime, df = dfree))
    lm.lo[[j]] <- lm(y.lo ~ splines::ns(y$reltime, df = dfree))
  }
  cli_alert_info("Finished fitting .hi/.lo LMs")

  if (!is.null(args$writeBackup)) {
    cli_alert_info("Writing LM archive to {.file {args$writeBackup}}")
    saveRDS(list("lm.hi" = lm.hi,"lm.lo" = lm.lo), args$writeBackup)
  }
} else if (!is.null(arch)) { # Otherwise, load archived LM objects, if possible
  cli_alert_warning("Too few states sampled ({.val {length(SampledStates)}}), instead loading backup LM objects from {.file {arch}}")
  res <- readRDS(arch)
  lm.hi <- res$lm.hi
  lm.lo <- res$lm.lo
  usedBackup <- TRUE
} else { # Otherwise, error and exit
  cli_alert_danger("Too few states sampled ({.val {length(SampledStates)}}) and no {.code --backup}. Exiting")
  quit(status=1)
}

# Compute new CIs
for (j in CIvars) {
  cli_alert_info("Computing CIs for variable {.code {j}}")
  if(j == "cum.incidence"){
    hi.pred <- data.frame("pred" =invlogit(y[[j]] - predict(lm.hi[[j]], 
                                                            data.frame(splines::ns(y$reltime, 
                                                                                   df = dfree)))))*y$pop
    lo.pred <- data.frame("pred" = invlogit(y[[j]] - predict(lm.lo[[j]], 
                                                             data.frame(splines::ns(y$reltime, 
                                                                                    df = dfree)))))*y$pop
  } else{
  hi.pred <- exp(
    predict(lm.hi[[j]], splines::ns(y$reltime, df = dfree)) +
      y[[j]]
  )
  lo.pred <- exp(
    predict(lm.lo[[j]], splines::ns(y$reltime, df = dfree)) +
      y[[j]]
  )
}
  colNameHi <- glue("{j}.hi")
  colNameLo <- glue("{j}.lo")

  # Only replace confidence intervals if they are actually missing! Otherwise
  # all states which sampled will be overwritten by the synthetic intervals.
  d[colNameHi] <- ifelse(is.na(d[[colNameHi]]), hi.pred, d[[colNameHi]])
  d[colNameLo] <- ifelse(is.na(d[[colNameLo]]), lo.pred, d[[colNameLo]])
  
}
cli_alert_success("Computed all CIs")

# All variables from covidestim output which have a '.hi/.lo' field
allCIVars <- colnames(d)[which(str_detect(colnames(d), '\\.hi'))] %>% str_remove('\\.hi')

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
