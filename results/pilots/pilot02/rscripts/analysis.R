# pilot 02
# conditionals with "stop" and "know", manipulating the QUD
# analysis

# set working directory to directory of script
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

# load required packages
library(tidyverse)
library(lme4)
library(lmerTest)
library(forcats)
library(emmeans)

# load helper functions
source('../../../../helpers.R')

# load data
d = read_csv("../data/cd.csv")
nrow(d) #74

table(d$qud) 

# reduce data to CC, set reference levels
d = d %>%
  filter(utterance=="neg-know") %>%
  mutate(qud = fct_relevel(qud, "why"))

nrow(d) #33
table(d$utterance,d$qud)

# fit a model
m = lm(responseFIRST ~ qud,data=d)
summary(m)
# qudbelPost    0.3203     0.1093   2.932  0.00629 **
