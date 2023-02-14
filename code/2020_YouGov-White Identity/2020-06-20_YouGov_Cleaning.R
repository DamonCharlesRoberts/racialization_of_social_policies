#### Title: ####
### 2020-06-20_YouGov_Cleaning.R ###

#### Notes: ####
### Need to convert from text to integers ###

#### Input: ####
#'White Identity/Data/Taubman Spring 2020/BROW0009_OUTPUT.csv'

#### Output: ####
#'White Identity/Data/Taubman Spring 2020/2020-06-20_cleaned-yougov.dta'


#### Set Up: ####
{
  library(foreign)
  library(haven)
  library(tidyverse)
  library(here)
}
setwd('C:/Users/damon/Dropbox/White Identity/')

#### Load Data: ####
ds.original <- read.csv('Data/Taubman Spring 2020/BROW0009_OUTPUT.csv')
ds.1 <- as_tibble(ds.original)

#### Change Strings to Numerics ####

# Age (18-100)
ds.1$age <- 2020-ds.1$birthyr
summary(ds.1$age)


# Female (Female == 1, Male == 0)
ds.1$female <- case_when(
  ds.1$gender == "Male" ~ 0,
  ds.1$gender == "Female" ~ 1
)
summary(ds.1$female)
tabulate(ds.1$female)

# White (White == 1, All others == 0)
ds.1$white <- NA
ds.1$white <- ifelse(ds.1$race == "White", 1, 0)
summary(ds.1$white)
tabulate(ds.1$white)

# Education (No HS == 1: Post-Grad == 6)
ds.1$educ <- NA
ds.1$educ <- case_when(
  ds.original$educ == "No HS" ~ 1,
  ds.original$educ == "High school graduate" ~ 2,
  ds.original$educ == "Some college" ~ 3,
  ds.original$educ == "2-year" ~ 4,
  ds.original$educ == "4-year" ~ 5,
  ds.original$educ == "Post-grad" ~ 6
)
summary(ds.1$educ)
tabulate(ds.1$educ)

# Marital Status (1 Married, 2 Separated, 3 Divorced, 4 Widowed, 5 Never, 6 Domestic Partnership)
ds.1$marstat <- NA
ds.1$marstat <- case_when(
  ds.original$marstat == "Married" ~ 1,
  ds.original$marstat == "Separated" ~ 2, 
  ds.original$marstat == "Divorced" ~ 3, 
  ds.original$marstat == "Widowed" ~ 4,
  ds.original$marstat == "Never married" ~ 5,
  ds.original$marstat == "Domestic / civil partnership" ~ 6
)
summary(ds.1$marstat)
tabulate(ds.1$marstat)

# Employment (1 Full, 2 part, 3 Temp Laid off, 4 Unemployed, 5 Retired, 6 Disabled, 7 Homemaker, 8 Student, 9 Other)
ds.1$employ <- NA
ds.1$employ <- case_when(
  ds.original$employ == "Full-time" ~ 1,
  ds.original$employ == "Part-time" ~ 2,
  ds.original$employ == "Temporarily laid off" ~ 3,
  ds.original$employ == "Unemployed" ~ 4,
  ds.original$employ == "Retired" ~ 5,
  ds.original$employ == "Permanently disabled" ~ 6,
  ds.original$employ == "Homemaker" ~ 7,
  ds.original$employ == "Student" ~ 8,
  ds.original$employ == "Other" ~ 9
)
summary(ds.1$employ)
tabulate(ds.1$employ)

# Family Income
ds.1$faminc_new <- NA
ds.1$faminc_new <- case_when(
  ds.original$faminc_new == "Less than $10,000" ~ 1,
  ds.original$faminc_new == "$10,000 - $19,999" ~ 2,
  ds.original$faminc_new == "$20,000 - $29,999" ~ 3,
  ds.original$faminc_new == "$30,000 - $39,999" ~ 4,
  ds.original$faminc_new == "$40,000 - $49,999" ~ 5,
  ds.original$faminc_new == "$50,000 - $59,999" ~ 6,
  ds.original$faminc_new == "$60,000 - $69,999" ~ 7,
  ds.original$faminc_new == "$70,000 - $79,999" ~ 8,
  ds.original$faminc_new == "$80,000 - $99,999" ~ 9,
  ds.original$faminc_new == "$100,000 - $119,999" ~ 10,
  ds.original$faminc_new == "$120,000 - $149,999" ~ 11,
  ds.original$faminc_new == "$150,000 - $199,999" ~ 12,
  ds.original$faminc_new == "$200,000 - $249,999" ~ 13,
  ds.original$faminc_new == "$250,000 - $349,999" ~ 14,
  ds.original$faminc_new == "$350,000 - $499,999" ~ 15,
  ds.original$faminc_new == "$500,000 or more" ~ 16
)
summary(ds.1$faminc_new)
tabulate(ds.1$faminc_new)

# Pid 3 (-1 Democrat, 0 Independent, 1 Republican)
ds.1$pid3 <- NA
ds.1$pid3[ds.original$pid3 == "Democrat"] <- -1
ds.1$pid3[ds.original$pid3 == "Republican"] <- 1
ds.1$pid3[ds.original$pid3 == "Independent" | ds.original$pid3 == "Other"] <- 0
ds.1$pid3[ds.original$pid3 == "Not Sure"] <- NA
summary(ds.1$pid3)
tabulate(ds.1$pid3)

# Pid 7 (-3 Strong Dem, -2 Weak Dem, -1 Lean Dem, 0 Independent, 1 Lean Rep, 2 Weak Rep, 3 Strong Rep)
ds.1$pid7 <- NA
ds.1$pid7[ds.original$pid7 == "Strong Democrat"] <- -3
ds.1$pid7[ds.original$pid7 == "Not very strong Democrat"] <- -2
ds.1$pid7[ds.original$pid7 == "Lean Democrat"] <- -1
ds.1$pid7[ds.original$pid7 == "Independent"] <- 0
ds.1$pid7[ds.original$pid7 == "Lean Republican"] <- 1
ds.1$pid7[ds.original$pid7 == "Not very strong Republican"] <- 2
ds.1$pid7[ds.original$pid7 == "Strong Republican"] <- 3
summary(ds.1$pid7)
tabulate(ds.1$pid7)

# Presidential Vote Post Election (1 Clinton, 2 Trump, 3 Johnson, 4 Stein, 5 McMullin, 6 Other, 7 Didn't Vote)
ds.1$presvote16post <- NA
ds.1$presvote16post <- case_when(
  ds.original$presvote16post == "Hillary Clinton" ~ 1,
  ds.original$presvote16post == "Donald Trump" ~ 2,
  ds.original$presvote16post == "Gary Johnson" ~ 3,
  ds.original$presvote16post == "Jill Stein" ~ 4,
  ds.original$presvote16post == "Evan McMullin" ~ 5, 
  ds.original$presvote16post == "Other" ~ 6,
  ds.original$presvote16post == "Did not vote for President" ~ 7
)
summary(ds.1$presvote16post)
tabulate(ds.1$presvote16post)

# Voter Registration Status (1 Yes, 0 No)
ds.1$voterregDummy <- NA
ds.1$voterregDummy <- ifelse(ds.original$votereg == "Yes", 1, 0)
summary(ds.1$votereg)
summary(ds.1$voterregDummy)
tabulate(ds.1$voterregDummy)

# Ideology (-2 Very Lib, -1 Liberal, 0 Moderate, 1 Conservative, 2 Very Conservative)
ds.1$ideo5 <- NA
ds.1$ideo5[ds.original$ideo5 == "Very liberal"] <- -2
ds.1$ideo5[ds.original$ideo5 == "Liberal"] <- -1
ds.1$ideo5[ds.original$ideo5 == "Moderate"] <- 0
ds.1$ideo5[ds.original$ideo5 == "Conservative"] <- 1
ds.1$ideo5[ds.original$ideo5 == "Very conservative"] <- 2
ds.1$ideo5[ds.original$ideo5 == "Not sure"] <- NA
summary(ds.1$ideo5)
tabulate(ds.1$ideo5)

# Political Interest (1 Hardly, 2 Only Now and Then, 3 Sometimes, 4 Most of the Time)
ds.1$newsint <- NA
ds.1$newsint[ds.original$newsint == "Most of the time"] <- 4
ds.1$newsint[ds.original$newsint == "Some of the time"] <- 3
ds.1$newsint[ds.original$newsint == "Only now and then"] <- 2
ds.1$newsint[ds.original$newsint == "Hardly at all"] <- 1
ds.1$newsint[ds.original$newsint == "Don't know"] <- NA
summary(ds.1$newsint)
tabulate(ds.1$newsint)

# Identity Treatment (1 Control, 2 White, 3 Black)
ds.1$de_treat <- NA
ds.1$de_treat <- case_when(
  ds.original$de_treat == "de control treatment" ~ 1,
  ds.original$de_treat == "de white treatment" ~ 2,
  ds.original$de_treat == "de black treatment" ~ 3
)
summary(ds.1$de_treat)
tabulate(ds.1$de_treat)

# Feel Good about Family Income (7 Strongly Agree: 1 Strongly Disagree)
ds.1$famInc <- NA
ds.1$famInc <- case_when(
  ds.original$CoL_FamInc == "Strongly agree" ~ 7,
  ds.original$CoL_FamInc == "Agree" ~ 6,
  ds.original$CoL_FamInc == "Somewhat agree" ~ 5,
  ds.original$CoL_FamInc == "Neither agree nor disagree" ~ 4, 
  ds.original$CoL_FamInc == "Somewhat disagree" ~ 3,
  ds.original$CoL_FamInc == "Disagree" ~ 2, 
  ds.original$CoL_FamInc == "Strongly disagree" ~ 1,
)
summary(ds.1$famInc)
tabulate(ds.1$famInc)

# Feel Good about job/job prospects (7 Strongly Agree: 1 Strongly Disagree)
ds.1$jobprosp <- NA
ds.1$jobprosp <- case_when(
  ds.original$CoL_job == "Strongly agree" ~ 7,
  ds.original$CoL_job == "Agree" ~ 6,
  ds.original$CoL_job == "Somewhat agree" ~ 5,
  ds.original$CoL_job == "Neither agree nor disagree" ~ 4, 
  ds.original$CoL_job == "Somewhat disagree" ~ 3,
  ds.original$CoL_job == "Disagree" ~ 2, 
  ds.original$CoL_job == "Strongly disagree" ~ 1,
)
summary(ds.1$jobprosp)
tabulate(ds.1$jobprosp)


# Feel Good about current savings ( 7 Strongly Agree: 1 Strongly Disagree)
ds.1$savings <- NA
ds.1$savings <- case_when(
  ds.original$CoL_savings == "Strongly agree" ~ 7,
  ds.original$CoL_savings == "Agree" ~ 6,
  ds.original$CoL_savings == "Somewhat agree" ~ 5,
  ds.original$CoL_savings == "Neither agree nor disagree" ~ 4, 
  ds.original$CoL_savings == "Somewhat disagree" ~ 3,
  ds.original$CoL_savings == "Disagree" ~ 2, 
  ds.original$CoL_savings == "Strongly disagree" ~ 1,
)
summary(ds.1$savings)
tabulate(ds.1$savings)

# Able to pay unforeseen expense (7 Strongly Agree: 1 Strongly Disagree)
ds.1$expenses <- NA
ds.1$expenses <- case_when(
  ds.original$CoL_expense == "Strongly agree" ~ 7,
  ds.original$CoL_expense == "Agree" ~ 6,
  ds.original$CoL_expense == "Somewhat agree" ~ 5,
  ds.original$CoL_expense == "Neither agree nor disagree" ~ 4, 
  ds.original$CoL_expense == "Somewhat disagree" ~ 3,
  ds.original$CoL_expense == "Disagree" ~ 2, 
  ds.original$CoL_expense == "Strongly disagree" ~ 1,
)
summary(ds.1$expenses)
tabulate(ds.1$expenses)

# Feel Good about Family's Dependable Income (7 Strongly Agree: 1 Strongly Disagree)
ds.1$dependInc <- NA
ds.1$dependInc <- case_when(
  ds.original$CoL_FamIncDep == "Strongly agree" ~ 7,
  ds.original$CoL_FamIncDep == "Agree" ~ 6,
  ds.original$CoL_FamIncDep == "Somewhat agree" ~ 5,
  ds.original$CoL_FamIncDep == "Neither agree nor disagree" ~ 4, 
  ds.original$CoL_FamIncDep == "Somewhat disagree" ~ 3,
  ds.original$CoL_FamIncDep == "Disagree" ~ 2, 
  ds.original$CoL_FamIncDep == "Strongly disagree" ~ 1,
)
summary(ds.1$dependInc)
tabulate(ds.1$dependInc)

# Feel Good about family's health (7 Strongly Agree: 1 Strongly Disagree)
ds.1$famHealth <- NA
ds.1$famHealth <- case_when(
  ds.original$CoL_health == "Strongly agree" ~ 7,
  ds.original$CoL_health == "Agree" ~ 6,
  ds.original$CoL_health == "Somewhat agree" ~ 5,
  ds.original$CoL_health == "Neither agree nor disagree" ~ 4, 
  ds.original$CoL_health == "Somewhat disagree" ~ 3,
  ds.original$CoL_health == "Disagree" ~ 2, 
  ds.original$CoL_health == "Strongly disagree" ~ 1,
)
summary(ds.1$famHealth)
tabulate(ds.1$famHealth)

# Job Worry (Free response)

# Drug Worry (Free Response)

# Crime worry (Free Response)

# Inequality Worry (Free Response)

# Historical discrimination makes it difficult for Blacks (5 Strongly Agree:1 Strongly Disagree)
ds.1$historicDiscrim <- NA
ds.1$historicDiscrim[ds.original$de_rr_discrim == "Strongly agree"] <- 5
ds.1$historicDiscrim[ds.original$de_rr_discrim == "Agree"] <- 4
ds.1$historicDiscrim[ds.original$de_rr_discrim == "Neither agree nor disagree"] <- 3
ds.1$historicDiscrim[ds.original$de_rr_discrim == "Disagree"] <- 2
ds.1$historicDiscrim[ds.original$de_rr_discrim == "Strongly disagree"] <- 1
summary(ds.1$historicDiscrim)
tabulate(ds.1$historicDiscrim)

# If Blacks would only try harder, they could be just as well off as whites (5 Strongly Agree: 1 Strongly Disagree)
ds.1$bootstraps <- NA
ds.1$bootstraps[ds.original$de_rr_try == "Strongly agree"] <- 5
ds.1$bootstraps[ds.original$de_rr_try == "Agree"] <- 4
ds.1$bootstraps[ds.original$de_rr_try == "Neither agree nor disagree"] <- 3
ds.1$bootstraps[ds.original$de_rr_try == "Disagree"] <- 2
ds.1$bootstraps[ds.original$de_rr_try == "Strongly disagree"] <- 1
summary(ds.1$bootstraps)
tabulate(ds.1$bootstraps)

# Other minorities overcame prejudice, Blacks should do the same (5 Strongly Agree: 1 Strongly Disagree)
ds.1$othersOvercome <- NA
ds.1$othersOvercome[ds.original$de_rr_irish == "Strongly agree"] <- 5
ds.1$othersOvercome[ds.original$de_rr_irish == "Agree"] <- 4
ds.1$othersOvercome[ds.original$de_rr_irish == "Neither agree nor disagree"] <- 3
ds.1$othersOvercome[ds.original$de_rr_irish == "Disagree"] <- 2
ds.1$othersOvercome[ds.original$de_rr_irish == "Strongly disagree"] <- 1
summary(ds.1$othersOvercome)
tabulate(ds.1$othersOvercome)

# Blacks have gotten less than they deserve (5 Strongly Agree: 1 Strongly Disagree)
ds.1$bdeserve <- NA
ds.1$bdeserve[ds.original$de_rr_deserve == "Strongly agree"] <- 5
ds.1$bdeserve[ds.original$de_rr_deserve == "Agree"] <- 4
ds.1$bdeserve[ds.original$de_rr_deserve == "Neither agree nor disagree"] <- 3
ds.1$bdeserve[ds.original$de_rr_deserve == "Disagree"] <- 2
ds.1$bdeserve[ds.original$de_rr_deserve == "Strongly disagree"] <- 1
summary(ds.1$bdeserve)
tabulate(ds.1$bdeserve)

# Blacks should stop blaming the past for their current circumstances (5 Strongly Agree: 1 Strongly Disagree)
ds.1$bexcuses <- NA
ds.1$bexcuses[ds.original$de_rr_excuse == "Strongly agree"] <- 5
ds.1$bexcuses[ds.original$de_rr_excuse == "Agree"] <- 4
ds.1$bexcuses[ds.original$de_rr_excuse == "Neither agree nor disagree"] <- 3
ds.1$bexcuses[ds.original$de_rr_excuse == "Disagree"] <- 2
ds.1$bexcuses[ds.original$de_rr_excuse == "Strongly disagree"] <- 1
summary(ds.1$bexcuses)
tabulate(ds.1$bexcuses)

# I avoid conflict (5 Strongly Agree: 1 Strongly Disagree)
ds.1$conflictavoid <- NA
ds.1$conflictavoid <- case_when(
  ds.original$ms_q11 == "Strongly disagree" ~ 1,
  ds.original$ms_q11 == "Disagree" ~ 2,
  ds.original$ms_q11 == "Somewhat disagree" ~ 3,
  ds.original$ms_q11 == "Neither agree nor disagree" ~ 4,
  ds.original$ms_q11 == "Somewhat agree" ~ 5,
  ds.original$ms_q11 == "Agree" ~ 6,
  ds.original$ms_q11 == "Strongly agree" ~ 7
)
summary(ds.1$conflictavoid)
tabulate(ds.1$conflictavoid)

# Arguments can be fun (5 Strongly Agree: 1 Strongly Disagree)
ds.1$funargue <- NA
ds.1$funargue <- case_when(
  ds.original$ms_q12 == "Strongly disagree" ~ 1,
  ds.original$ms_q12 == "Disagree" ~ 2,
  ds.original$ms_q12 == "Somewhat disagree" ~ 3,
  ds.original$ms_q12 == "Neither agree nor disagree" ~ 4,
  ds.original$ms_q12 == "Somewhat agree" ~ 5,
  ds.original$ms_q12 == "Agree" ~ 6,
  ds.original$ms_q12 == "Strongly agree" ~ 7
)
summary(ds.1$funargue)
tabulate(ds.1$funargue)

# I am sometimes reluctant to talk about politics (5 Strongly Agree: 1 Strongly Disagree)
ds.1$reluctant <- NA
ds.1$reluctant <- case_when(
  ds.original$ms_q13 == "Strongly disagree" ~ 1,
  ds.original$ms_q13 == "Disagree" ~ 2,
  ds.original$ms_q13 == "Somewhat disagree" ~ 3,
  ds.original$ms_q13 == "Neither agree nor disagree" ~ 4,
  ds.original$ms_q13 == "Somewhat agree" ~ 5,
  ds.original$ms_q13 == "Agree" ~ 6,
  ds.original$ms_q13 == "Strongly agree" ~ 7
)
summary(ds.1$funargue)
tabulate(ds.1$funargue)

# When I talk about politics I do so because I learn new things and gain a better understanding (5 Strongly Agree: 1 Strongly Disagree)
ds.1$socialexpertise <- NA
ds.1$socialexpertise <- case_when(
  ds.original$ms_q14 == "Strongly disagree" ~ 1,
  ds.original$ms_q14 == "Disagree" ~ 2,
  ds.original$ms_q14 == "Somewhat disagree" ~ 3,
  ds.original$ms_q14 == "Neither agree nor disagree" ~ 4,
  ds.original$ms_q14 == "Somewhat agree" ~ 5,
  ds.original$ms_q14 == "Agree" ~ 6,
  ds.original$ms_q14 == "Strongly agree" ~ 7
)
summary(ds.1$socialexpertise)
tabulate(ds.1$socialexpertise)

# Racial identity -- Unimportant to my sense of what kind of person I am (5 Strongly Agree: 1 Strongly Disagree)
ds.1$senseofself <- NA
ds.1$senseofself <- case_when(
  ds.original$de_id_1 == "Strongly disagree" ~ 1,
  ds.original$de_id_1 == "Disagree" ~ 2,
  ds.original$de_id_1 == "Somewhat disagree" ~ 3,
  ds.original$de_id_1 == "Neither agree nor disagree" ~ 4,
  ds.original$de_id_1 == "Somewhat agree" ~ 5,
  ds.original$de_id_1 == "Agree" ~ 6,
  ds.original$de_id_1 == "Strongly agree" ~ 7
)
summary(ds.1$senseofself)
tabulate(ds.1$senseofself)

# Racial identity -- Central to who I am as an individual (5 Strongly Agree: 1 Strongly Disagree)
ds.1$idcentral <- NA
ds.1$idcentral <- case_when(
  ds.original$de_id_2 == "Strongly disagree" ~ 1,
  ds.original$de_id_2 == "Disagree" ~ 2,
  ds.original$de_id_2 == "Somewhat disagree" ~ 3,
  ds.original$de_id_2 == "Neither agree nor disagree" ~ 4,
  ds.original$de_id_2 == "Somewhat agree" ~ 5,
  ds.original$de_id_2 == "Agree" ~ 6,
  ds.original$de_id_2 == "Strongly agree" ~ 7
)
summary(ds.1$idcentral)
tabulate(ds.1$idcentral)

# Racial Identity -- Feel Good (5 Strongly Agree: 1 Strongly Disagree)
ds.1$rigood <- NA
ds.1$rigood <- case_when(
  ds.original$de_id_3 == "Strongly disagree" ~ 1,
  ds.original$de_id_3 == "Disagree" ~ 2,
  ds.original$de_id_3 == "Somewhat disagree" ~ 3,
  ds.original$de_id_3 == "Neither agree nor disagree" ~ 4,
  ds.original$de_id_3 == "Somewhat agree" ~ 5,
  ds.original$de_id_3 == "Agree" ~ 6,
  ds.original$de_id_3 == "Strongly agree" ~ 7
)
summary(ds.1$rigood)
tabulate(ds.1$rigood)

# Support for free tuition at public colleges (7 Favor a great deal: 1 Oppose a great deal)
ds.1$freeCol <- NA
ds.1$freeCol <- case_when(
  ds.original$de_free_coll == "Oppose a great deal" ~ 1,
  ds.original$de_free_coll == "Oppose a moderate amount" ~ 2,
  ds.original$de_free_coll == "Oppose a little" ~ 3,
  ds.original$de_free_coll == "Neither favor nor oppose" ~ 4,
  ds.original$de_free_coll == "Favor a little" ~ 5,
  ds.original$de_free_coll == "Favor a moderate amount" ~ 6,
  ds.original$de_free_coll == "Favor a great deal" ~ 7
)
summary(ds.1$freeCol)
tabulate(ds.1$freeCol)

# Support for student loan debt forgiveness (7 Favor a great deal: 1 Oppose a great deal)
ds.1$stuLoan <- NA
ds.1$stuLoan <- case_when(
  ds.original$de_loans == "Oppose a great deal" ~ 1,
  ds.original$de_loans == "Oppose a moderate amount" ~ 2,
  ds.original$de_loans == "Oppose a little" ~ 3,
  ds.original$de_loans == "Neither favor nor oppose" ~ 4,
  ds.original$de_loans == "Favor a little" ~ 5,
  ds.original$de_loans == "Favor a moderate amount" ~ 6,
  ds.original$de_loans == "Favor a great deal" ~ 7
)
summary(ds.1$stuLoan)
tabulate(ds.1$stuLoan)

# Support for welfare programs (7 Favor a great deal: 1 Oppose a great deal)
ds.1$welfare <- NA
ds.1$welfare <- case_when(
  ds.original$de_welfare == "Oppose a great deal" ~ 1,
  ds.original$de_welfare == "Oppose a moderate amount" ~ 2,
  ds.original$de_welfare == "Oppose a little" ~ 3,
  ds.original$de_welfare == "Neither favor nor oppose" ~ 4,
  ds.original$de_welfare == "Favor a little" ~ 5,
  ds.original$de_welfare == "Favor a moderate amount" ~ 6,
  ds.original$de_welfare == "Favor a great deal" ~ 7
)
summary(ds.1$welfare)
tabulate(ds.1$welfare)

# Support for regulations on greenhouse emissions (7 Favor a great deal: 1 Oppose a great deal)
ds.1$hc <- NA 
ds.1$hc <- case_when(
  ds.original$de_hc == "Oppose a great deal" ~ 1,
  ds.original$de_hc == "Oppose a moderate amount" ~ 2,
  ds.original$de_hc == "Oppose a little" ~ 3,
  ds.original$de_hc == "Neither favor nor oppose" ~ 4,
  ds.original$de_hc == "Favor a little" ~ 5,
  ds.original$de_hc == "Favor a moderate amount" ~ 6,
  ds.original$de_hc == "Favor a great deal" ~ 7
)
summary(ds.1$hc)
tabulate(ds.1$hc)

# Treatment Attention Check (1 Yes, paid attention; 0 No, didn't pay attention )
ds.1$attnCheckdummy <- NA
ds.1$attnCheckdummy <- ifelse(ds.original$de_exp_check == "$29,000", 1, 0)
summary(ds.1$attnCheckdummy)
tabulate(ds.1$attnCheckdummy)

#### Save .dta file ####
write.csv(ds.1,'Data/Taubman Spring 2020/2020-06-20_cleaned-yougov-v1.csv')
