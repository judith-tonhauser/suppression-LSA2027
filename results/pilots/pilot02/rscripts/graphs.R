# conditionals with "stop" and "know", manipulating the QUD
# graphs

# set working directory to directory of script
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

# load required packages
library(tidyverse)

# color-blind-friendly palette
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

theme_set(theme_bw())

# load helper functions
source('../../../../helpers.R')

# load clean data
d = read_csv("../data/cd.csv")
nrow(d) #74

names(d)
table(d$utterance,d$qud,d$item)
length(unique(d$participantID)) #74

# plot ----

table(d$qud)

meansByQUD = d %>%
  group_by(qud,utterance) %>%
  summarize(Mean = mean(responseFIRST), CILow = ci.low(responseFIRST), CIHigh = ci.high(responseFIRST)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh) %>%
  select(-c(CILow, CIHigh))
meansByQUD

meansByQUD = meansByQUD %>%
  mutate(qud = case_when(qud == "why" ~ "WHAT IF p? QUD",
                         utterance == "neg-know" ~ "BEL? QUD",
                         TRUE ~ "POST? QUD"))

d = d %>%
  mutate(qud = case_when(qud == "why" ~ "WHAT IF p? QUD",
                         utterance == "neg-know" ~ "BEL? QUD",
                         TRUE ~ "POST? QUD"))

ggplot(data=meansByQUD,aes(x=qud,y=Mean))+
  geom_violin(data=d,aes(x=qud,y=responseFIRST),position=position_dodge(.8)) +
  geom_jitter(data=d,aes(x=qud,y=responseFIRST),color="gray",alpha = 0.8, position = position_jitter(width = .2),) +
  geom_errorbar(aes(y=Mean, ymin=YMin, ymax=YMax), width=0.2, colour="black",alpha=1,position=position_dodge(.8)) +
  geom_point(position=position_dodge(.8),size=2) +
  theme(legend.position="bottom") +
  ylab("mean inference rating") +
  labs(x=NULL) +
  scale_y_continuous(limits=c(-0.01,1.01),breaks=c(0,.5,1),labels=c(0,.5,1)) +
  facet_wrap(. ~ utterance, scales = "free_x") 
ggsave("../graphs/pilot2-means-by-QUD.pdf",width=4.7,height=2)

