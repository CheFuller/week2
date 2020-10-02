#########################
# OMSBA 5112 - Week 2   #
# Data Wrangling Coding #
#                       #
# 10/01/2020            #
#                       #
# Chequala Fuller       #
#########################

#load library for tidyverse, haven, and ggplot2
library(tidyverse)
library(haven)
library(ggplot2)

#read the .dta file as nfhs from relative path
nfhs <- read_dta("omsba_5112_week2/IAHR52FL.dta")

#Q2: Create a new data frame (wlth_df) that selects all variables between
#the household id (hhid) and hv208 (both included) plus the wealth index (hv270)

wlth_df <- nfhs %>%
  select(hhid:hv208, hv270)

#Q3: Create a data frame (edu_df) that holds the household id (hhid), the individuals'
#line numbers (hvidx_01:hvidx_35), and their education (hv108_01:hv108_35) in single years

edu_df <- nfhs %>%
  select(hhid, hvidx_01:hvidx_35, hv108_01:hv108_35)

#Q4: Create a data frame (fml_df) of anthropometric (height and weight) data for 
#females (with household id) ha2_01:ha3_11

fml_df <- nfhs %>%
  select(hhid, matches("ha\\d_\\d\\d"))

#Q5: Create a data frame (ml_df) of anthropometric (heigh and weight) data for
#males (with household id) hb2_01:hb3_18

ml_df <- nfhs %>%
  select(hhid, 
         num_range("hb0_", 0:20, width = 2),
         num_range("hb1_", 0:20, width = 2),
         num_range("hb2_", 0:20, width = 2),
         num_range("hb3_", 0:20, width = 2),
         num_range("hb4_", 0:20, width = 2),
         num_range("hb5_", 0:20, width = 2),
         num_range("hb6_", 0:20, width = 2)
  )


################################################################################
# Q6: Create a code to How many observations are in this data frame?           # 
# The problem with the three data frames we have created is that they are still# 
# not tidy,and we, therefore, cannot merge them. We will start with the        #
# education data frame.The problem here is that what we need both the "hhid"   #
# variable and the rosternumber to merge on, but the roster numbers are in the #
# cells. Furthermore, there arelots of cells with NAs, and we need to remove   #
# them. All, in all, a fun logic puzzle!                                       #
#                                                                              #
# The end goal here is to get the roster line number and the years of education# 
# of eachindividual on their separate lines. To achieve this, you need to use  # 
# the number thatthe original variables include (for example, hvidx_04 and     #
# hv108_04 containinformation about the same person). There are likely multiple# 
# different ways of doingthis, but the one I used involved first making one    #
# long list (using gather), splitting thevariable (using separate), and finally#
# getting the roster number and education on thesame observation line          #
# (using spread). My recommendation is that you do this insteps, rather than   #
# try to pipe everything together from the beginning.                          #
# Once you have done, filter out the lines that have NAs in them. You should   #
# also dropany variable(s) that you will not need anymore and rename the       #
# variables to make iteasier to understand what they contain. When you finish, #
# you should have a new dataframe with three variables (hhid, the roster       #
# number, and the year of education). How many observations are in this        #
# data frame?                                                                  #
################################################################################


edu_df2 <- edu_df %>%
  gather(hh_member_id, education_yrs, -hhid) %>%
  separate(hh_member_id, c("hv108", "member_id"), sep = "_") %>%
  spread(key = hv108, value = education_yrs) %>%
  filter(!is.na(hvidx)) %>%
  select(-member_id) %>%
  rename(roster_id = hvidx, edu = "hv108")
#nrow(edu_df)

#Q7:
fml_mutate <- fml_df %>%
  select(hhid, matches("ha\\d_\\d\\d")) %>%
  gather(hh_member_id, measurements, -hhid) %>%
  separate(hh_member_id, c("anthro", "measure"), sep = "_") %>%
  spread(key = anthro, value = measurements) %>%
  filter(!is.na(ha0)) %>%
  select(-measure, -ha4, -ha5, -ha6) %>%
  rename(roster_id = ha0, age = ha1, weight = ha2, height = ha3)%>%
  mutate(female = TRUE)

#Q8:
ml_mutate <- ml_df %>%
  select(hhid, matches("hb\\d_\\d\\d")) %>%
  gather(hh_member_id, measurements, -hhid) %>%
  separate(hh_member_id, c("anthro", "measure"), sep = "_") %>%
  spread(key = anthro, value = measurements) %>%
  filter(!is.na(hb0)) %>%
  select(-measure, -hb4, -hb5, -hb6) %>%
  rename(roster_id = hb0, age = hb1, weight = hb2, height = hb3)%>%
  mutate(female = FALSE)

#Q9:

combine_fml_ml <- bind_rows(fml_mutate, ml_mutate)
merge_edu_fml_ml <- left_join(edu_df, combine_fml_ml, by = "hhid")

#female and male median age
median(fml_mutate[["age"]])
median(ml_mutate[["age"]])
