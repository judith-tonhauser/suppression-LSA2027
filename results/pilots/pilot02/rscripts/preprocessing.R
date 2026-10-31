# conditionals with "stop" and "know", manipulating the QUD
# preprocessing

# set working directory to directory of script
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

source('../../../../helpers.R')

# load required packages for pre-processing data
library(tidyverse)
library(readr)

theme_set(theme_bw())

# read in the raw data
d = read_csv("../data/combined.csv")
#view(d)

# keep rows "multi-slider" and "survey"
d <- d %>% filter((trial_type %in% c("multiple-slider", "survey")))

# replace participant_id by random number
length(unique(d$participant_id)) #78
d$participantID <- match(d$participant_id, unique(sort(d$participant_id)))

# how many participants?
length(unique(d$participantID)) #78

# select relevant columns
d = d %>%
  select(c(rt, condition, response, trial_type, participantID))

# unpack demographics info
dg <- d %>%
  filter(trial_type == "survey") %>%
  select(c(participantID,response))
#view(dg)

table(dg$response)
str(dg$response)

# age
dg$age = gsub("P0_Q0\":null,\"", "", dg$response) # delete everything before "age"
dg$age = gsub(",\"gender.*", "", dg$age) # delete everything after age value
# now that we can see only age values, only record numbers
dg$age = gsub("\\D", "", dg$age)
dg$age = as.numeric(dg$age)
table(dg$age)
str(dg$age)
mean(dg$age,na.rm = TRUE) #43.6

# gender
dg$gender = case_when(grepl("gender\":\"female", dg$response) ~ "female",
                      grepl("gender\":\"male", dg$response) ~ "male",
                      grepl("non-binary", dg$response) ~ "non-binary",
                      TRUE ~ "preferNoToSay")
table(dg$gender)

# language
dg$language = case_when(grepl("language\":\"yes", dg$response) ~ "English",
                        grepl("language\":\"no", dg$response) ~ "notSpeakerOfEnglish",
                        grepl("language\":\"\"", dg$response) ~ "noResponse",
                      TRUE ~ "error") 
table(dg$language) 

# American English
dg$amE = case_when(grepl("amE\":\"yes", dg$response) ~ "AmE",
                   grepl("amE\":\"no", dg$response) ~ "notAmE",
                   grepl("amE\":\"\"", dg$response) ~ "noResponse",
                    TRUE ~ "error")
table(dg$amE)

# education
dg$education = case_when(grepl("some high school", dg$response) ~ "some high school",
                         grepl("graduated high school", dg$response) ~ "graduated high school",
                         grepl("some college", dg$response) ~ "some college",
                         grepl("graduated college", dg$response) ~ "graduated college",
                         grepl("hold a higher degree", dg$response) ~ "hold a higher degree",
                         TRUE ~ "preferNoToSay")
table(dg$education)

# comments
dg$comments = gsub(".*comments", "", dg$response)
table(dg$comments)
                      
# remove response column from demographics data
dg = dg %>%
  select(-c(response))
summary(dg)

#view(d)
# remove demographics and instructions from data
d = d %>%
  filter(trial_type == "multiple-slider")

# add demographics data back to data
d = left_join(d, dg, by = "participantID")

#view(d)
# create useful columns from condition column
table(d$condition)

# item
d$item = "purse"
# d$item = case_when(grepl("petshelter", d$condition) ~ "petshelter",
#                       grepl("bike", d$condition) ~ "bike",
#                    grepl("subway", d$condition) ~ "subway",
#                       TRUE ~ "error")
table(d$item)

# qud
d$qud = case_when(grepl("qud\":\"belPost", d$condition) ~ "belPost",
                      grepl("qud\":\"why", d$condition) ~ "why",
                  TRUE ~ "error")
table(d$qud)

# utterance
d$utterance = case_when(grepl("doesn’t stop", d$condition) ~ "neg-stop",
                    grepl("doesn’t know", d$condition) ~ "neg-know",
                    #grepl("Sue know", d$condition) ~ "q-know",
                    #grepl("Charley stop", d$condition) ~ "q-stop",
                    TRUE ~ "error")
table(d$utterance)



# # prior
# d$prior = case_when(grepl("lower", d$condition) ~ "lower",
#                   grepl("higher", d$condition) ~ "higher",
#                   TRUE ~ "error")
# table(d$prior)


# remove condition column now that everything has been extracted from it
d = d %>%
  select(-c(condition))

# get the response for the first and second slider
#view(d)

# now code the responses, based on condition
table(d$response)

# first slider
d$responseFIRST = gsub("\\{\"stimFIRST\":","",d$response) # delete stimFIRST
d$responseFIRST = gsub(",\".*","",d$responseFIRST) #delete ," and everything after it
d$responseFIRST = gsub("\\}","",d$responseFIRST) #delete }
table(d$responseFIRST)

# second slider
# d$responseSECOND = gsub(".*D\":","",d$response) # delete D": (in SECOND:") and everything before it
# d$responseSECOND = gsub("\\}","",d$responseSECOND) #delete }
# table(d$responseSECOND)

# label response types

# the first slider collects ratings about PRE/CC in the ai condition, 
# and about POST/BEL in the nai condition
# d$responseFIRSTtype = case_when(d$verb == "stop" & d$qud == "ai" ~ "PRE",
#                                 d$verb == "stop" & d$qud == "nai" ~ "POST",
#                                 d$verb == "know" & d$qud == "ai" ~ "CC",
#                                 d$verb == "know" & d$qud == "nai" ~ "BEL",
#                                 TRUE ~ "error")
# table(d$responseFIRSTtype)
# 
# # the second slider collects ratings about PRE/CC in the nai condition,
# # and about POST/BEL in the ai condition
# d$responseSECONDtype = case_when(d$verb == "stop" & d$qud == "ai" ~ "POST",
#                                 d$verb == "stop" & d$qud == "nai" ~ "PRE",
#                                 d$verb == "know" & d$qud == "ai" ~ "BEL",
#                                 d$verb == "know" & d$qud == "nai" ~ "CC",
#                                 TRUE ~ "error")
# table(d$responseSECONDtype)

# remove columns not needed
d = d %>%
  select(-c(response,trial_type))
#view(d)

# make responses numeric and between 0 and 1
d$responseFIRST <- as.numeric(d$responseFIRST)
d$responseFIRST <- d$responseFIRST/100
table(d$responseFIRST)

# d$responseSECOND <- as.numeric(d$responseSECOND)
# d$responseSECOND <- d$responseSECOND/100
# table(d$responseSECOND)
# 
# # convert POST to notPOST
# d[d$responseFIRSTtype=="POST",]$responseFIRST = 1 - d[d$responseFIRSTtype=="POST",]$responseFIRST
# table(d$responseFIRST)
# 
# d[d$responseSECONDtype=="POST",]$responseSECOND = 1 - d[d$responseSECONDtype=="POST",]$responseSECOND
# table(d$responseSECOND)
# 
# d = d %>%
#   mutate(responseFIRSTtype = recode(responseFIRSTtype, "POST" = "notPOST")) %>%
#   mutate(responseSECONDtype = recode(responseSECONDtype, "POST" = "notPOST"))

# the recoding of POST to notPOST also requires the recoding of the prior
# Julian stopped taking the subway when he got promoted
# POST = Julian took the subway before he got promoted
# higher = Julian is a climate activist
# lower = Julian is a germaphobe
# POST is more likely with higher than with lower
# notPOST is 1-POST, Julian didn't take the subway before he got promoted
# which is more likely with lower than with higher

#view(d)
# can't change this here because it would also change the prior for PRE inferences
# need to do this below, when the data is in long format, not here, in wide
  
# participant info
table(d$age) #19-80 
length(which(is.na(d$age))) # 1 missing values
str(d$age)
mean(d[d$age > 3,]$age,na.rm = TRUE) 

d %>% 
  select(gender, participantID) %>% 
  unique() %>% 
  group_by(gender) %>% 
  summarize(count=n())

### exclude non-English speakers and non-American English speakers
# exclude non-English speakers
length(which(is.na(d$language))) #no missing responses
table(d$language) 

d <- d %>%
  filter(language == "English") %>%  droplevels()
length(unique(d$participantID)) #78

# exclude non-American English speakers
length(which(is.na(d$amE))) # everybody responded
table(d$amE) # 1 people not AmE

d <- d %>%
  filter(amE != "notAmE") %>%  droplevels()
length(unique(d$participantID)) #77

# check how long participants took on the main trial page
h <- hist(d$rt/1000,breaks = 100)
plot(h,xaxt="n")
axis(1, at=h$breaks,labels = round(h$breaks,2))

# median rt
median(d$rt/1000) #24

# on the SALT experiment, we excluded participants that took less than half the 
# median time on the main trial. the median was 60 seconds, so we excluded participants
# who took less than 30

sort(d$rt/1000)

threshold = median(d$rt)/2
threshold

# # only include participants who took longer than half the median seconds for the main trial
d = d[d$rt>threshold,]
length(unique(d$participantID)) #74, so 3 participants excluded

# plot the response times on the main trial page for the remaining participants
h <- hist(d$rt/1000,breaks = 100)
plot(h,xaxt="n")
axis(1, at=h$breaks,labels = round(h$breaks,2))

# age and gender of remaining participants
table(d$age) #19-80
length(which(is.na(d$age))) # 0 missing values
mean(d[d$age > 4,]$age,na.rm=TRUE) #45.6

d %>% 
  select(gender, participantID) %>% 
  unique() %>% 
  group_by(gender) %>% 
  summarize(count=n())

write_csv(d, file="../data/cd.csv")

