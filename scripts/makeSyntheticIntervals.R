#!/usr/bin/env Rscript
suppressPackageStartupMessages( library(tidyverse) )
library(splines)
library(docopt)
library(cli)
library(glue, warn.conflicts=F)

glue('covidestim synthetic-interval generator

Usage:
  {name} -o <path> [--backup <path> --writeBackup <path> --vars <vars> --minSampled <num>] --metadata <path> --writeMetadata <path> <result_path>
  {name} (-h | --help)
  {name} --version

Options:
  -o <path>               Where to output the resulting .csv
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

print(args)

ps("Reading metadata from {.file {args$metadata}}")
metadata <- jsonlite::read_json(args$metadata, simplifyVector = T)
pd()

d <- read_csv(
  args$result_path,
  col_types = cols(
    date = col_date(format = '%Y-%m-%d'),
    state = col_character(),
    data.available = col_logical(),
    .default = col_number()
  )
) # Load the results csv file
arch    <- args$backup # archive filepath
CIvars  <- str_split(args$vars, ',')[[1]]
K       <- as.numeric(args$minSampled) # minimum number of sampled states
dfree   <- 50
outfile <- args$o

## another way to learn which states sampled and which states did not?
SampledStates <- unique((d %>% filter(!is.na(infections.hi)))$state)

cli_alert_info("{.val {length(SampledStates)}} sampled from this run")

lm.hi <- lm.lo <- NULL

y <- d %>%
  select(state, date, starts_with(CIvars)) %>%
  # MARCUS: What will happen here if one of `CIvars` is equal to 0?
  # While the model probably is unable to generate 0s for any of `CIvars`,
  # I don't believe we can guarantee that they won't be rounded to 0 when
  # the results are serialized to CSVs
  mutate_at(vars(-state, -date), log) %>%
  group_by(state) %>%
  mutate(time = as.numeric(date - max(date)) - 1, # Why go <0 here?
         reltime = time / abs(min(time))) %>%
  ungroup()

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
} else { # Otherwise, error and exit
  cli_alert_danger("Too few states sampled ({.val {length(SampledStates)}}) and no {.code --backup}. Exiting")
  quit(status=1)
}

# Compute new CIs
for (j in CIvars) {
  cli_alert_info("Computing CIs for variable {.code {j}}")
  hi.pred <- exp(
    predict(lm.hi[[j]], splines::ns(y$reltime, df = dfree)) +
      y[[j]]
  )
  lo.pred <- exp(
    predict(lm.lo[[j]], splines::ns(y$reltime, df = dfree)) +
      y[[j]]
  )

  colNameHi <- glue("{j}.hi")
  colNameLo <- glue("{j}.lo")

  d[colNameHi] <- hi.pred
  d[colNameLo] <- lo.pred
}
cli_alert_success("Computed all CIs")

ps("Writing metadata to {.file {args$writeMetadata}}")
jsonlite::write_json(metadata, args$writeMetadata, null = "null")
pd()

ps("Writing new summary CSV to {.file {outfile}}")
write_csv(d, outfile)
pd()
