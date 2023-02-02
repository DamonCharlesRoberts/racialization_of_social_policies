#### Title: #### 
### YouGov White Identity Experiment ###

#### Set Up ####
library(foreign)
library(haven)
library(here)
library(tidyverse)
setwd('/')

#### Load in Dataset ####
ds.original <- read_dta('c:/Users/damon/Dropbox/White Identity/Data/Taubman Spring 2020/yougov_original.dta')
ds.1 <- as_tibble(ds.original)
rm(ds.original)

#### Vars of Interest ####
ds.2 <- ds.1 %>%
  rename(
    famInc = col_faminc, #Feel Good about Family's Income
    jobprosp = col_job, # Feel good about job (prospects)
    savings = col_savings,#Feel good about savings
    expenses = col_expense,#Ability to pay for unforeseen expense
    dependInc = col_famincdep,#Feel good about dependable family income
    famHealth = col_health,#Feel good about family's health
    jobworry = security_worries_2,#Worried about job loss
    drugworry = security_worries_3,#Worried about drug addiction
    crimeworry = security_worries_4,#Worried about violent crime
    ineqworry = security_worries_6,#worried about economic inequality
    historicDiscrim = de_rr_discrim,#Generations of slavery and discrimination make it difficult for Blacks
    bootstraps = de_rr_try,#If Blacks would only try harder...
    othersOvercome = de_rr_irish,#If [other] minorities overcame, Blacks should do the same
    bdeserve = de_rr_deserve,#Blacks have gotten less than they deserve
    bexcuses = de_rr_excuse,#Blacks should stop blaming the past...
    conflictavoid = ms_q11,#I avoid conflict
    funargue = ms_q12,#Arguments can be fun
    reluctant = ms_q13,#Reluctant to talk about politics
    socialexpertise = ms_q14,#Learn new things when I talk about politics
    senseofself = de_id_1,#Identity is unimportant to sense of what kind of person I am
    idcentral = de_id_2,#Identiy is central to who I am
    rigood = de_id_3, #Identity - Feel good
    free_col = de_free_coll, #Free tuition at public colleges
    stu_loan = de_loans, #Student Loan Debt
    welfare = de_welfare, #Welfare programs
    hc = de_hc, #Single-payer health care
    enviro = de_enviro,#Greenhouse emissions regulation
    attnCheck = de_exp_check, #Treatment check
  )

# Age (18-100)
ds.2$age <- 2020-ds.2$birthyr
summary(ds.2$age)

# Female (Female == 1, Male == 0)
ds.2$female <- case_when(
  ds.2$gender == 1 ~ 0,
  ds.2$gender == 2 ~ 1
)
summary.factor(ds.2$female)

# White (White == 1, All others == 0)
ds.2$white <- ifelse(ds.2$race == 1, 1, 0)
summary.factor(ds.2$white)

# Education (No HS == 1: Post-Grad == 6)
summary(ds.2$educ)
  
# Marital Status (1 Married, 2 Separated, 3 Divorced, 4 Widowed, 5 Never, 6 Domestic Partnership)
summary(ds.2$marstat)
  
# Employment (1 Full, 2 part, 3 Temp Laid off, 4 Unemployed, 5 Retired, 6 Disabled, 7 Homemaker, 8 Student, 9 Other)
summary(ds.2$employ)

# Family Income
summary(ds.2$faminc_new)

# Pid 3 (-1 Democrat, 0 Independent, 1 Republican)
ds.2$pid3[ds.1$pid3 == 1] <- -1
ds.2$pid3[ds.1$pid3 == 2] <- 1
ds.2$pid3[ds.1$pid3 == 3 | ds.1$pid3 == 4] <- 0
ds.2$pid3[ds.1$pid3 == 5] <- NA
summary(ds.2$pid3)

# Pid 7 (-3 Strong Dem, -2 Weak Dem, -1 Lean Dem, 0 Independent, 1 Lean Rep, 2 Weak Rep, 3 Strong Rep)
ds.2$pid7[ds.1$pid7 == 1] <- -3
ds.2$pid7[ds.1$pid7 == 2] <- -2
ds.2$pid7[ds.1$pid7 == 3] <- -1
ds.2$pid7[ds.1$pid7 == 4] <- 0
ds.2$pid7[ds.1$pid7 == 5] <- 1
ds.2$pid7[ds.1$pid7 == 6] <- 2
ds.2$pid7[ds.1$pid7 == 7] <- 3
summary(ds.2$pid3)

# Presidential Vote Post Election (1 Clinton, 2 Trump, 3 Johnson, 4 Stein, 5 McMullin, 6 Other, 7 Didn't Vote)
summary(ds.2$presvote16post)

# State of Residence
summary(ds.2$inputstate)

# Voter Registration Status (1 Yes, 0 No)
ds.2$voterregDummy <- NA
ds.2$voterregDummy <- ifelse(ds.2$votereg == 1, 1, 0)
summary(ds.2$votereg)
summary(ds.2$voterregDummy)

# Ideology (-2 Very Lib, -1 Liberal, 0 Moderate, 1 Conservative, 2 Very Conservative)
ds.2$ideo5[ds.1$ideo5 == 1] <- -2
ds.2$ideo5[ds.1$ideo5 == 2] <- -1
ds.2$ideo5[ds.1$ideo5 == 3] <- 0
ds.2$ideo5[ds.1$ideo5 == 4] <- 1
ds.2$ideo5[ds.1$ideo5 == 5] <- 2
ds.2$ideo5[ds.1$ideo5 == 6] <- NA
summary(ds.2$ideo5)

# Political Interest (1 Hardly, 2 Only Now and Then, 3 Sometimes, 4 Most of the Time)
ds.2$newsint[ds.1$newsint == 1] <- 4
ds.2$newsint[ds.1$newsint == 2] <- 3
ds.2$newsint[ds.1$newsint == 3] <- 2
ds.2$newsint[ds.1$newsint == 4] <- 1
ds.2$newsint[ds.1$newsint == 7] <- NA
summary(ds.2$newsint)

# Identity Treatment (1 Control, 2 White, 3 Black)
summary(ds.2$de_treat)

# Feel Good about Family Income (1 Strongly Agree: 7 Strongly Disagree)
ds.2$famInc[ds.2$famInc == 98] <- NA
summary(ds.2$famInc)

# Feel Good about job/job prospects (1 Strongly Agree: 7 Strongly Disagree)
ds.2$jobprosp[ds.2$jobprosp == 98] <- NA
summary(ds.2$jobprosp)

# Feel Good about current savings ( 1 Strongly Agree: 7 Strongly Disagree)
ds.2$savings[ds.2$savings == 98] <- NA
summary(ds.2$savings)

# Able to pay unforeseen expense (1 Strongly Agree:7 Strongly Disagree)
ds.2$expenses[ds.2$expenses == 98] <- NA
summary(ds.2$expenses)

# Feel Good about Family's Dependable Income (1 Strongly Agree:7 Strongly Disagree)
ds.2$dependInc[ds.2$dependInc == 98] <- NA
summary(ds.2$dependInc)

# Feel Good about family's health (1 Strongly Agree:7 Strongly Disagree)
ds.2$famHealth[ds.2$famHealth == 98] <- NA
summary(ds.2$famHealth)

# Job Worry (Free response)

# Drug Worry (Free Response)

# Crime worry (Free Response)

# Inequality Worry (Free Response)

# Historical discrimination makes it difficult for Blacks (1 Strongly Agree:5 Strongly Disagree)
ds.2$historicDiscrim[ds.1$de_rr_discrim == 1] <- 5
ds.2$historicDiscrim[ds.1$de_rr_discrim == 2] <- 4
ds.2$historicDiscrim[ds.1$de_rr_discrim == 3] <- 3
ds.2$historicDiscrim[ds.1$de_rr_discrim == 4] <- 2
ds.2$historicDiscrim[ds.1$de_rr_discrim == 5] <- 1
ds.2$historicDiscrim[ds.1$de_rr_discrim == 8] <- NA
summary(ds.2$historicDiscrim)

# If Blacks would only try harder, they could be just as well off as whites (1 Strongly Agree: 5 Strongly Disagree)
ds.2$bootstraps[ds.1$de_rr_try == 1] <- 5
ds.2$bootstraps[ds.1$de_rr_try == 2] <- 4
ds.2$bootstraps[ds.1$de_rr_try == 3] <- 3
ds.2$bootstraps[ds.1$de_rr_try == 4] <- 2
ds.2$bootstraps[ds.1$de_rr_try == 5] <- 1
ds.2$bootstraps[ds.1$de_rr_try == 8] <- NA
summary(ds.2$bootstraps)

# Other minorities overcame prejudice, Blacks should do the same (1 Strongly Agree: 5 Strongly Disagree)
ds.2$othersOvercome[ds.1$de_rr_irish == 1] <- 5
ds.2$othersOvercome[ds.1$de_rr_irish == 2] <- 4
ds.2$othersOvercome[ds.1$de_rr_irish == 3] <- 3
ds.2$othersOvercome[ds.1$de_rr_irish == 4] <- 2
ds.2$othersOvercome[ds.1$de_rr_irish == 5] <- 1
ds.2$othersOvercome[ds.1$de_rr_irish == 8] <- NA
summary(ds.2$othersOvercome)

# Blacks have gotten less than they deserve (1 Strongly Agree: 5 Strongly Disagree)
ds.2$bdeserve[ds.1$de_rr_deserve == 1] <- 5
ds.2$bdeserve[ds.1$de_rr_deserve == 2] <- 4
ds.2$bdeserve[ds.1$de_rr_deserve == 3] <- 3
ds.2$bdeserve[ds.1$de_rr_deserve == 4] <- 2
ds.2$bdeserve[ds.1$de_rr_deserve == 5] <- 1
ds.2$bdeserve[ds.1$de_rr_deserve == 8] <- NA
summary(ds.2$bdeserve)

# Blacks should stop blaming the past for their current circumstances
ds.2$bexcuses[ds.1$de_rr_excuse == 1] <- 5
ds.2$bexcuses[ds.1$de_rr_excuse == 2] <- 4
ds.2$bexcuses[ds.1$de_rr_excuse == 3] <- 3
ds.2$bexcuses[ds.1$de_rr_excuse == 4] <- 2
ds.2$bexcuses[ds.1$de_rr_excuse == 5] <- 1
ds.2$bexcuses[ds.1$de_rr_excuse == 8] <- NA
summary(ds.2$bexcuses)

# I avoid conflict (1 Strongly Agree: 5 Strongly Disagree)
summary(ds.2$conflictavoid)

# Arguments can be fun (1 Strongly Agree: 5 Strongly Disagree)
summary(ds.2$funargue)

# I am sometimes reluctant to talk about politics (1 Strongly Agree: 5 Strongly Disagree)
summary(ds.2$reluctant)

# When I talk about politics I do so because I learn new things and gain a better understanding (1 Strongly Agree: 5 Strongly Disagree)
summary(ds.2$socialexpertise)

# Racial identity -- Unimportant to my sense of what kind of person I am (1 Strongly Agree: 5 Strongly Disagree)
summary(ds.2$senseofself)

# Racial identity -- Central to who I am as an individual (1 Strongly Agree: 5 Strongly Disagree)
summary(ds.2$idcentral)

# Racial Identity -- Feel Good (1 Strongly Agree: 5 Strongly Disagree)
summary(ds.2$rigood) 

# Support for free tuition at public colleges (1 Favor a great deal: 7 Oppose a great deal)
ds.2$free_col[ds.2$free_col == 98] <- NA
summary(ds.2$free_col)

# Support for student loan debt forgiveness
ds.2$stu_loan[ds.2$stu_loan == 98] <- NA
summary(ds.2$stu_loan)

# Support for welfare programs
ds.2$welfare[ds.2$welfare == 98] <- NA
summary(ds.2$welfare)

# Support for regulations on greenhouse emissions
ds.2$hc[ds.2$hc == 98] <- NA
summary(ds.2$hc)

# Treatment Attention Check (1 Yes, paid attention; 0 No, didn't pay attention )
ds.2$attnCheckdummy <- NA
ds.2$attnCheckdummy <- ifelse(ds.2$attnCheck == 4, 1, 0)
