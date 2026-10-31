# evaluation of the models in the suppression LSA paper

# load required libraries
library(jsonlite)
library(tidyverse)
library(rwebppl)
library(cowplot)

# set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
theme_set(theme_bw())

source('../helpers.R')

# define relevant components ----

# define the qudBias (from context)
qudBias = c("BEL?", "CC?")
qudBias

# run the three pragmatic listeners in 1a, 1b, 1c ----

#### model for 1a ----
model <- read_file("model-1a.wppl")
model

# evaluate the model
eval_webppl <- function(command) {
  webppl(paste(model,command,sep="\n"))
}

#### call PL ----

# define the relevant utterances
utterances = c("neg-know-pos-dance", "pos-know-pos-dance")
utterances

results <- list()

# unnest each PL_tmp as a whole, collect the results in a list, 
# and combine them at the end

# each PL_tmp is processed in one go, and bind_rows stacks everything at the end
# id numbers the cgs within each utterance × qudBias run, so after unnesting 
#one can still tell which rows belong to the same cg
# A PA is uniquely identified by utterance + qudBias + id.
# pa_type is computed before unnesting, while each PA is still a 
# complete data frame. map_chr looks at the CC values inside each PA and 
# labels it "CC=1" (all 1s), "CC=0" (all 0s), or "CC=0or1" (both values occur). 

for (u in utterances) {
  print(u)
  for (q in qudBias) {
    print(q)
    PL_tmp <- eval_webppl(paste0("pragmaticListener('", u, "','", q, "')"))
    
    results[[length(results) + 1]] <- PL_tmp %>%
      rename(state.CC = CC, pa = cg) %>%
      mutate(utterance = u,
             qudBias   = q,
             id        = row_number(),          # which pa within this run
             pa_type   = map_chr(pa, ~ case_when(
               all(.x$CC == 1) ~ "CC=1",
               all(.x$CC == 0) ~ "CC=0",
               TRUE            ~ "CC=0or1"))) %>%
      unnest(pa, names_sep = ".")
  }
}

PL <- bind_rows(results) %>%
  select(utterance, qudBias, id, state.CC, prob, pa_type, pa.BEL, pa.CC)

nrow(PL) #52

write_csv(PL, file="data/PL-1a-and-10.csv")

#### model for 1b conditional ----
model <- read_file("model-1b-conditional.wppl")
model

# define the relevant utterances
utterances = c("neg-know-pos-dance")
utterances

# evaluate the model
eval_webppl <- function(command) {
  webppl(paste(model,command,sep="\n"))
}

#### call PL ----

results <- list()

# unnest each PL_tmp as a whole, collect the results in a list, 
# and combine them at the end

# each PL_tmp is processed in one go, and bind_rows stacks everything at the end
# id numbers the cgs within each utterance × qudBias run, so after unnesting 
#one can still tell which rows belong to the same cg
# A PA is uniquely identified by utterance + qudBias + id.
# pa_type is computed before unnesting, while each PA is still a 
# complete data frame. map_chr looks at the CC values inside each PA and 
# labels it "CC=1" (all 1s), "CC=0" (all 0s), or "CC=0or1" (both values occur). 

for (u in utterances) {
  print(u)
  for (q in qudBias) {
    print(q)
    PL_tmp <- eval_webppl(paste0("pragmaticListener('", u, "','", q, "')"))
    
    results[[length(results) + 1]] <- PL_tmp %>%
      rename(state.CC = CC, pa = cg) %>%
      mutate(utterance = u,
             qudBias   = q,
             id        = row_number(),          # which pa within this run
             pa_type   = map_chr(pa, ~ case_when(
               all(.x$CC == 1) ~ "CC=1",
               all(.x$CC == 0) ~ "CC=0",
               TRUE            ~ "CC=0or1"))) %>%
      unnest(pa, names_sep = ".")
  }
}

PL <- bind_rows(results) %>%
  select(utterance, qudBias, id, state.CC, prob, pa_type, pa.BEL, pa.CC)

nrow(PL) #28

write_csv(PL, file="data/PL-1b.csv")

#### model for 1c conjunction ----
model <- read_file("model-1c-conjunction.wppl")
model

# evaluate the model
eval_webppl <- function(command) {
  webppl(paste(model,command,sep="\n"))
}

#### call PL ----

results <- list()

# unnest each PL_tmp as a whole, collect the results in a list, 
# and combine them at the end

# each PL_tmp is processed in one go, and bind_rows stacks everything at the end
# id numbers the cgs within each utterance × qudBias run, so after unnesting 
#one can still tell which rows belong to the same cg
# A PA is uniquely identified by utterance + qudBias + id.
# pa_type is computed before unnesting, while each PA is still a 
# complete data frame. map_chr looks at the CC values inside each PA and 
# labels it "CC=1" (all 1s), "CC=0" (all 0s), or "CC=0or1" (both values occur). 

# only run one utterance here because the model doesn't converge 
# for some of the others
utterances = c("neg-know-pos-dance")
utterances

for (u in utterances) {
  print(u)
  for (q in qudBias) {
    print(q)
    PL_tmp <- eval_webppl(paste0("pragmaticListener('", u, "','", q, "')"))
    
    results[[length(results) + 1]] <- PL_tmp %>%
      rename(state.CC = CC, pa = cg) %>%
      mutate(utterance = u,
             qudBias   = q,
             id        = row_number(),          # which pa within this run
             pa_type   = map_chr(pa, ~ case_when(
               all(.x$CC == 1) ~ "CC=1",
               all(.x$CC == 0) ~ "CC=0",
               TRUE            ~ "CC=0or1"))) %>%
      unnest(pa, names_sep = ".")
  }
}

PL <- bind_rows(results) %>%
  select(utterance, qudBias, id, state.CC, prob, pa_type, pa.BEL, pa.CC)

nrow(PL) #4

write_csv(PL, file="data/PL-1c.csv")

# preprocessing ----

#### 1a (neg-know-pos-dance) ----
PL = read_csv("data/PL-1a-and-10.csv")
nrow(PL) #52
#view(PL)

# keep rows where state.CC=1 and the utterance is neg-know
# rename utterance, and quds
# drop columns pa.CC and pa.BEL and state.CC
PL = PL %>%
  filter(state.CC == 1) %>%
  filter(utterance == "neg-know-pos-dance") %>%
  mutate(utterance = recode(utterance, "neg-know-pos-dance" = "neg-know")) %>%
  droplevels() %>%
  rename("CC" = "state.CC") %>%
  rename("qud" = "qudBias") %>%
  mutate(qud = recode(qud, "BEL?" = "BEL? QUD", "CC?" = "CC? QUD")) %>%
  select(-c(pa.CC,pa.BEL))
PL

# remove duplicate rows
PL = PL %>%
  distinct()
PL
nrow(PL) #6

write_csv(PL, file="data/PL-1a.csv")

#### 10 (pos-know-pos-dance) ----
PL = read_csv("data/PL-1a-and-10.csv")
nrow(PL) #52
#view(PL)

# keep rows where state.CC=1 and the utterance is neg-know
# rename utterance, and quds
# drop columns pa.CC and pa.BEL and state.CC
PL = PL %>%
  filter(state.CC == 1) %>%
  filter(utterance == "pos-know-pos-dance") %>%
  mutate(utterance = recode(utterance, "neg-know-pos-dance" = "pos-know")) %>%
  droplevels() %>%
  rename("CC" = "state.CC") %>%
  rename("qud" = "qudBias") %>%
  mutate(qud = recode(qud, "BEL?" = "BEL? QUD", "CC?" = "CC? QUD")) %>%
  select(-c(pa.CC,pa.BEL))
PL

# remove duplicate rows
PL = PL %>%
  distinct()
PL
nrow(PL) #6

write_csv(PL, file="data/PL-10.csv")

#### 1b ----

# read model data
PL = read_csv("data/PL-1b.csv")
nrow(PL) #28

# keep rows where state.CC=1 and the utterance is neg-know
# rename utterance, and quds
# drop columns pa.CC and pa.BEL and state.CC
PL = PL %>%
  filter(state.CC == 1) %>%
  filter(utterance == "neg-know-pos-dance") %>%
  mutate(utterance = recode(utterance, "neg-know-pos-dance" = "neg-know")) %>%
  droplevels() %>%
  rename("CC" = "state.CC") %>%
  rename("qud" = "qudBias") %>%
  mutate(qud = recode(qud, "BEL?" = "BEL? QUD", "CC?" = "CC? QUD")) %>%
  select(-c(pa.CC,pa.BEL))
PL

# remove duplicate rows
PL = PL %>%
  distinct()
PL
nrow(PL) #4

write_csv(PL, file="data/PL-1b.csv")

#### 1c ----

# read model data
PL = read_csv("data/PL-1c.csv")
nrow(PL) #4
#view(PL)

# keep rows where state.CC=1 and the utterance is neg-know
# rename utterance, and quds
# drop columns pa.CC and pa.BEL and state.CC
PL = PL %>%
  filter(state.CC == 1) %>%
  filter(utterance == "neg-know-pos-dance") %>%
  mutate(utterance = recode(utterance, "neg-know-pos-dance" = "neg-know")) %>%
  droplevels() %>%
  rename("CC" = "state.CC") %>%
  rename("qud" = "qudBias") %>%
  mutate(qud = recode(qud, "BEL?" = "BEL? QUD", "CC?" = "CC? QUD")) %>%
  select(-c(pa.CC,pa.BEL))
PL

# remove duplicate rows
PL = PL %>%
  distinct()
PL
nrow(PL) #2

write_csv(PL, file="data/PL-1c.csv")

# Fig. 2: joint plots only BEL? QUD ----

# read model data
PL1a = read_csv("data/PL-1a.csv")
nrow(PL1a) #6
PL1b = read_csv("data/PL-1b.csv")
nrow(PL1b) #4
PL1c = read_csv("data/PL-1c.csv")
nrow(PL1c) #2
PL10 = read_csv("data/PL-10.csv")
nrow(PL10) #6

PL_agg1a <- PL1a %>%
  filter(qud == "BEL? QUD") %>%
  summarise(`CSS:1` = sum(prob[CC == 1]),
            `CSS:1 privately assumed` = sum(prob[pa_type == "CC=1"]),
            .groups = "drop") %>%
  pivot_longer(c(`CSS:1`, `CSS:1 privately assumed`),
               names_to = "measure", values_to = "prob") %>%
  mutate(example = "1a")
PL_agg1a

PL_agg1b <- PL1b %>%
  filter(qud == "BEL? QUD") %>%
  summarise(`CSS:1` = sum(prob[CC == 1]),
            `CSS:1 privately assumed` = sum(prob[pa_type == "CC=1"]),
            .groups = "drop") %>%
  pivot_longer(c(`CSS:1`, `CSS:1 privately assumed`),
               names_to = "measure", values_to = "prob") %>%
  mutate(example = "1b")
PL_agg1b

PL_agg1c <- PL1c %>%
  filter(qud == "BEL? QUD") %>%
  summarise(`CSS:1` = sum(prob[CC == 1]),
            `CSS:1 privately assumed` = sum(prob[pa_type == "CC=1"]),
            .groups = "drop") %>%
  pivot_longer(c(`CSS:1`, `CSS:1 privately assumed`),
               names_to = "measure", values_to = "prob") %>%
  mutate(example = "1c")
PL_agg1c

PL_agg10 <- PL10 %>%
  filter(qud == "BEL? QUD") %>%
  summarise(`CSS:1` = sum(prob[CC == 1]),
            `CSS:1 privately assumed` = sum(prob[pa_type == "CC=1"]),
            .groups = "drop") %>%
  pivot_longer(c(`CSS:1`, `CSS:1 privately assumed`),
               names_to = "measure", values_to = "prob") %>%
  mutate(example = "10")
PL_agg10

d = rbind(PL_agg1a,PL_agg1b,PL_agg1c,PL_agg10)
d

d <- d |> mutate(measure = factor(measure, levels = c("CSS:1", "CSS:1 privately assumed")))

ggplot(filter(d, measure %in% c("CSS:1", "CSS:1 privately assumed")),
       aes(x = example, y = prob, fill = measure)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  scale_fill_manual(values = c("CSS:1"                   = "grey70",
                               "CSS:1 privately assumed" = "steelblue")) +
  scale_x_discrete(limits = c("1a", "1b", "1c", "10")) +
  scale_y_continuous(limits = c(-.01, 1.01), expand = c(0, 0)) +
  labs(x = "example", y = "predicted probability\n of inference", fill = NULL) +
  theme(legend.position = "top") 
ggsave("graphs/predProb-by-example.pdf",width=4,height=2.5)

# run original model for "Charley speaks Spanish" -----

#### model for 1a ----
model <- read_file("model-1a.wppl")
model

# evaluate the model
eval_webppl <- function(command) {
  webppl(paste(model,command,sep="\n"))
}

#### call PL ----

# define the relevant utterances
utterances = c("pos-bare-pos-dance")
utterances

results <- list()

# unnest each PL_tmp as a whole, collect the results in a list, 
# and combine them at the end

# each PL_tmp is processed in one go, and bind_rows stacks everything at the end
# id numbers the cgs within each utterance × qudBias run, so after unnesting 
#one can still tell which rows belong to the same cg
# A PA is uniquely identified by utterance + qudBias + id.
# pa_type is computed before unnesting, while each PA is still a 
# complete data frame. map_chr looks at the CC values inside each PA and 
# labels it "CC=1" (all 1s), "CC=0" (all 0s), or "CC=0or1" (both values occur). 

for (u in utterances) {
  print(u)
  for (q in qudBias) {
    print(q)
    PL_tmp <- eval_webppl(paste0("pragmaticListener('", u, "','", q, "')"))
    
    results[[length(results) + 1]] <- PL_tmp %>%
      rename(state.CC = CC, pa = cg) %>%
      mutate(utterance = u,
             qudBias   = q,
             id        = row_number(),          # which pa within this run
             pa_type   = map_chr(pa, ~ case_when(
               all(.x$CC == 1) ~ "CC=1",
               all(.x$CC == 0) ~ "CC=0",
               TRUE            ~ "CC=0or1"))) %>%
      unnest(pa, names_sep = ".")
  }
}

PL <- bind_rows(results) %>%
  select(utterance, qudBias, id, state.CC, prob, pa_type, pa.BEL, pa.CC)

nrow(PL) #20

write_csv(PL, file="data/PL-bare.csv")

#### bare utterance  ----
PL = read_csv("data/PL-bare.csv")
nrow(PL) #20

# keep rows where state.CC=1 and the utterance is bare
# rename utterance, and quds
# drop columns pa.CC and pa.BEL and state.CC
PL = PL %>%
  filter(state.CC == 1) %>%
  filter(utterance == "pos-bare-pos-dance") %>%
  mutate(utterance = recode(utterance, "pos-bare-pos-dance" = "pos-bare")) %>%
  droplevels() %>%
  rename("CC" = "state.CC") %>%
  rename("qud" = "qudBias") %>%
  mutate(qud = recode(qud, "BEL?" = "BEL? QUD", "CC?" = "CC? QUD")) %>%
  select(-c(pa.CC,pa.BEL))
PL

# remove duplicate rows
PL = PL %>%
  distinct()
PL
nrow(PL) #8

write_csv(PL, file="data/PL-bare.csv")

# calculate CC:1 observed and CC:1 privately assumed
# for CC:1 observed: sum up the probability of CC=1 by QUDs, and
# for CC:1 privately assumed: take the mean over the pa_types
PL = read_csv("data/PL-bare.csv")
nrow(PL) #8

PL_bare <- PL %>%
  group_by(qud) %>%
  summarise(`CC:1 observed` = sum(prob[CC == 1]),
            `CC:1 privately assumed` = sum(prob[pa_type == "CC=1"]),
            .groups = "drop") %>%
  pivot_longer(c(`CC:1 observed`, `CC:1 privately assumed`),
               names_to = "measure", values_to = "prob")
PL_bare
#  qud      measure                 prob
#1 BEL? QUD CC:1 observed          1    
#2 BEL? QUD CC:1 privately assumed 0.242

# other plots ----

#### read preprocessed data ----
PL1a = read_csv("data/PL-1a.csv")
nrow(PL1a) #6
PL1b = read_csv("data/PL-1b.csv")
nrow(PL1b) #4
PL1c = read_csv("data/PL-1c.csv")
nrow(PL1c) #2

#### individual plots for each model, by QUD ----
# calculate CC:1 observed and CC:1 privately assumed
# for CC:1 observed: sum up the probability of CC=1 by QUDs, and
# for CC:1 privately assumed: take the mean over the pa_types
PL_agg1a <- PL1a %>%
  group_by(qud) %>%
  summarise(`CC:1 observed` = sum(prob[CC == 1]),
            `CC:1 privately assumed` = sum(prob[pa_type == "CC=1"]),
            .groups = "drop") %>%
  pivot_longer(c(`CC:1 observed`, `CC:1 privately assumed`),
               names_to = "measure", values_to = "prob")
PL_agg1a

ggplot(PL_agg1a, aes(x = qud, y = prob, fill = measure)) +
  geom_col(position = position_dodge(width = 0.4), width = 0.3) +
  scale_fill_manual(values = c("CC:1 observed" = "black",
                               "CC:1 privately assumed" = "gray75")) +
  scale_y_continuous(limits = c(-.01, 1.01)) +
  labs(x = "QUD", y = "Probability", fill = NULL) +
  theme_minimal() +
  theme(legend.position = "top")
ggsave("graphs/Fig1a.pdf",width=3,height=3)

# for CC:1 observed: sum up the probability of CC=1 for each QUDs, and
# for CC:1 privately assumed: take the mean over the pa_types
PL_agg1b <- PL1b %>%
  group_by(qud) %>%
  summarise(`CC:1 observed` = sum(prob[CC == 1]),
            `CC:1 privately assumed` = sum(prob[pa_type == "CC=1"]),
            .groups = "drop") %>%
  pivot_longer(c(`CC:1 observed`, `CC:1 privately assumed`),
               names_to = "measure", values_to = "prob")
PL_agg1b

ggplot(PL_agg1b, aes(x = qud, y = prob, fill = measure)) +
  geom_col(position = position_dodge(width = 0.4), width = 0.3) +
  scale_fill_manual(values = c("CC:1 observed" = "black",
                               "CC:1 privately assumed" = "gray75")) +
  scale_y_continuous(limits = c(-.01, 1.01)) +
  labs(x = "QUD", y = "Probability", fill = NULL) +
  theme_minimal() +
  theme(legend.position = "top")
ggsave("graphs/Fig1b.pdf",width=3,height=3)

# for CC:1 observed: sum up the probability of CC=1 for each QUDs, and
# for CC:1 privately assumed: take the mean over the pa_types
PL_agg1c <- PL1c %>%
  group_by(qud) %>%
  summarise(`CC:1 observed` = sum(prob[CC == 1]),
            `CC:1 privately assumed` = sum(prob[pa_type == "CC=1"]),
            .groups = "drop") %>%
  pivot_longer(c(`CC:1 observed`, `CC:1 privately assumed`),
               names_to = "measure", values_to = "prob")
PL_agg1c

ggplot(PL_agg1c, aes(x = qud, y = prob, fill = measure)) +
  geom_col(position = position_dodge(width = 0.4), width = 0.3) +
  scale_fill_manual(values = c("CC:1 observed" = "black",
                               "CC:1 privately assumed" = "gray75")) +
  scale_y_continuous(limits = c(-.01, 1.01)) +
  labs(x = "QUD", y = "Probability", fill = NULL) +
  theme_minimal() +
  theme(legend.position = "top")
ggsave("graphs/Fig1c.pdf",width=3,height=3)


# joint plots with QUD ----

make_plot <- function(data) {
  ggplot(data, aes(x = qud, y = prob, fill = measure)) +
    geom_col(position = position_dodge(width = 0.4), width = 0.3) +
    scale_fill_manual(values = c("CC:1 observed" = "black",
                                 "CC:1 privately assumed" = "gray75")) +
    scale_y_continuous(limits = c(-.01, 1.01)) +
    labs(x = "QUD", y = "Probability", fill = NULL) +
    theme_minimal() +
    theme(legend.position = "top")
}

p1 <- make_plot(PL_agg1a)
p2 <- make_plot(PL_agg1b)
p3 <- make_plot(PL_agg1c)

# extract the shared legend from one plot
legend <- get_plot_component(p1, "guide-box-top", return_all = TRUE)

# save the legend on its own (wide and short)
ggsave("graphs/pl_legend.pdf", ggdraw(legend), width = 6, height = 0.4)

# save each plot without a legend, all at the same size
plots <- list(p1, p2, p3)
for (i in seq_along(plots)) {
  ggsave(paste0("graphs/pl_panel", i, ".pdf"),
         plots[[i]] + theme(legend.position = "none"),
         width = 2.5, height = 2)
}


