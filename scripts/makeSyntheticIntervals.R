library("tidyverse")
library("readr")
library("splines")

d       <- read_csv("data/estimates.csv") # csv file
arch    <- "data/oldLM.RDS" # archive filepath
CIvars  <- c("infections", "Rt", "cum.incidence")
K       <- 10 # minimum number of sampled states
dfree   <- 50
outfile <- "data/newestimates.csv"

## another way to learn which states sampled and which states did not?
SampledStates   <-  unique((d %>% filter(!is.na(infections.hi)))$state)

lm.hi <- lm.lo <- NULL

y <- d %>%
  select(state, date, starts_with(CIvars)) %>%
  mutate_at(vars(-state, -date), log) %>%
  group_by(state) %>%
  mutate(time = as.numeric(date - max(date)) - 1,
         reltime = time / abs(min(time))) %>%
  ungroup()

if (length(SampledStates) > K) {
  for (j in CIvars) {
    y.hi <- y[[j]] - y[[paste0(j, ".hi")]]
    y.lo <- y[[j]] - y[[paste0(j, ".lo")]]
    lm.hi[[j]] <- lm(y.hi ~ splines::ns(y$reltime, df = dfree))
    lm.lo[[j]] <- lm(y.lo ~ splines::ns(y$reltime, df = dfree))
  }
} else {
  res <- readRDS(arch)
  lm.hi <- res$lm.hi
  lm.lo <- res$lm.lo
  # saveRDS(list("lm.hi" = lm.hi,"lm.lo" = lm.lo),"oldLM.RDS")
}

## Compute new CI
for (j in CIvars) {
  hi.pred <- data.frame(exp(predict(lm.hi[[j]], 
                                    splines::ns(y$reltime, df = dfree)) + 
                              y[[j]]))
  lo.pred <- data.frame(exp(predict(lm.lo[[j]], 
                                    splines::ns(y$reltime, df = dfree)) + 
                              y[[j]]))
  colnames(hi.pred) <- paste0(j, ".hi.pred")
  colnames(lo.pred) <- paste0(j, ".lo.pred")
  d <- cbind(d, hi.pred, lo.pred)
  
}

write_csv(d, outfile)
