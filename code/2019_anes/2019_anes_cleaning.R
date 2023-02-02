# Title: 2019 ANES Cleaning

# Notes:
  #* Description: Script for cleaning 2019 American National Election Study
  #* Updated: 2023-01-30
  #* Updated By: dcr

# Setup:
  #* Load Libraries
box::use(
    #** Helpful functions for data cleaning
  dplyr = dplyr[mutate, case_when]
)

  #* Load data
anes_19_original <- read.csv(
  file = "data/2019-anes-pilot/anes_pilot_2019.csv"
)

# Cleaning
anes_19_cleaned <- anes_19_original |>
  mutate(
  #* Dependent Variables
    #** Free College -- Favorability toward free college
      #*** Coded as:
        #**** 1 = Favor a great deal
        #**** 2 = Favor a moderate amount
        #**** 3 = Favor a little
        #**** 4 = Neither favor nor oppose
        #**** 5 = Oppose a little
        #**** 6 = Oppose a moderate amount
        #**** 7 = Oppose a great deal
        #**** NA = Non-response
      #*** Recoded to:
        #**** 1 = Oppose a great deal
        #**** 2 = Oppose a moderate amount
        #**** 3 = Oppose a little
        #**** 4 = Neither favor nor oppose
        #**** 5 = Favor a little
        #**** 6 = Favor a moderate amount
        #**** 7 = Favor a great deal
        #**** NA = Non-response
    free_college = case_when(
      freecol == 1 ~ 7,
      freecol == 2 ~ 6,
      freecol == 3 ~ 5,
      freecol == 4 ~ 4,
      freecol == 5 ~ 3,
      freecol == 6 ~ 2,
      freecol == 7 ~ 1
    ),
    #** Loan forgiveness -- Favorability toward loan forgiveness
      #** Coded as:
        #**** 1 = Favor a great deal
        #**** 2 = Favor a moderate amount
        #**** 3 = Favor a little
        #**** 4 = Neither favor nor oppose
        #**** 5 = Oppose a little
        #**** 6 = Oppose a moderate amount
        #**** 7 = Oppose a great deal
        #**** NA = Non-response
      #*** Recoded to:
        #**** 1 = Oppose a great deal
        #**** 2 = Oppose a moderate amount
        #**** 3 = Oppose a little
        #**** 4 = Neither favor nor oppose
        #**** 5 = Favor a little
        #**** 6 = Favor a moderate amount
        #**** 7 = Favor a great deal
        #**** NA = Non-response
    loan_forgiveness = case_when(
      loans == 1 ~ 7,
      loans == 2 ~ 6,
      loans == 3 ~ 5,
      loans == 4 ~ 4,
      loans == 5 ~ 3,
      loans == 6 ~ 2,
      loans == 7 ~ 1
    ),
    #** Universal Healthcare -- Favorability toward universal health care
      #** Coded as:
        #**** 1 = Favor a great deal
        #**** 2 = Favor a moderate amount
        #**** 3 = Favor a little
        #**** 4 = Neither favor nor oppose
        #**** 5 = Oppose a little
        #**** 6 = Oppose a moderate amount
        #**** 7 = Oppose a great deal
        #**** NA = Non-response
      #*** Recoded to:
        #**** 1 = Oppose a great deal
        #**** 2 = Oppose a moderate amount
        #**** 3 = Oppose a little
        #**** 4 = Neither favor nor oppose
        #**** 5 = Favor a little
        #**** 6 = Favor a moderate amount
        #**** 7 = Favor a great deal
        #**** NA = Non-response
    universal_healthcare = case_when(
      hlthcare1 == 1 ~ 7,
      hlthcare1 == 2 ~ 6,
      hlthcare1 == 3 ~ 5,
      hlthcare1 == 4 ~ 4,
      hlthcare1 == 5 ~ 3,
      hlthcare1 == 6 ~ 2,
      hlthcare1 == 7 ~ 1
      ),
  #* Explanatory variables
    #** White -- Is respondent White?
      #*** Coded as:
        #**** 1 = White
        #**** 2 = Black
        #**** 3 = Hispanic
        #**** 4 = Asian
        #**** 5 = Native American
        #**** 6 = Mixed
        #**** 7 = Other
        #**** 8 = Middle Eastern
        #**** NA = Non-response
      #*** Recoded to:
        #**** 0 = Non-White
        #**** 1 = White
        #**** NA = Non-response
    white = ifelse(
      race == 1, 1, 0
    ),
    #** Racial Identification -- Identify with Racial group
      #*** Coded as:
        #**** 1 = Not at all important
        #**** 2 = A little important
        #**** 3 = Moderately important
        #**** 4 = Very important
        #**** 5 = Extremely important
        #**** -7 = No Answer
        #**** -1 = Inapplicable
      #*** Recoded to:
        #**** 1 = Not at all important
        #**** 2 = A little important
        #**** 3 = Moderately important
        #**** 4 = Very important
        #**** 5 = Extremely important
        #**** NA = No Answer, Inapplicable
    racial_identity = ifelse(
      raceid <= 0, NA, raceid
    ),
  #* Control variables
    #** Family income -- Household income
      #*** Coded as:
        #**** Note: *Incomplete information in codebook*
        #**** Values range from 1-12
        #**** Values greater than 27 = Missing responses
      #*** Recoded to:
        #**** Values 1-12 left alone
        #**** NA = Values greater than 27
    family_income = ifelse(
      faminc_new >= 27, NA, faminc_new
    ),
    #** Age -- Age of respondent at time of survey
      #*** Coded as:
        #**** Birth year
      #*** Recoded to:
        #**** 2019 - Birth year
    age = (
      2019 - birthyr
    ),
    age = case_when(
      age <= 17 ~ 0,
      age >= 18 & age <= 24 ~ 1,
      age >= 25 & age <= 34 ~ 2,
      age >= 35 & age <= 44 ~ 3,
      age >= 45 & age <= 54 ~ 4,
      age >= 55 & age <= 64 ~ 5,
      age >= 65 ~ 6
    ),
    #** Female -- Self-identified gender
      #*** Coded as:
        #**** 1 = Male
        #**** 2 = Female
      #*** Recoded to:
        #**** 0 = Non-Female
        #**** 1 = Female
    female = case_when(
      gender == 1 ~ 0,
      gender == 2 ~ 1
    ),
            #** education -- education level of respondent
            #*** Coded as:
                #**** 1 = Didn't complete HS
                #**** 2 = HS Graduate
                #**** 3 = Some college
                #**** 4 = Associates
                #**** 5 = Bachelors
                #**** 6 = Post-Grad
            #*** Recoded to:
                #**** 1 = Didn't complete HS
                #**** 2 = HS Graduate
                #**** 3 = Some College
                #**** 4 = Associates
                #**** 5 = Bachelors
                #**** 6 = Post-Grad
        education = ifelse(
            educ >= 1 & educ <= 6, educ, NA
        ),
    #** PID 7 -- 7-item Partisan identification
      #*** Coded as:
        #**** 1 = Strong Democrat
        #**** 2 = Not very strong Democrat
        #**** 3 = Independent, closer to Democrat
        #**** 4 = Independent
        #**** 5 = Independent, closer to Republican
        #**** 6 = Not very strong Republican
        #**** 7 = Strong Republican
        #**** -7 = No answer
      #*** Recoded to:
        #**** 1 = Strong Republican
        #**** 2 = Not very strong Republican
        #**** 3 = Independent, closer to Republican
        #**** 4 = Independent
        #**** 5 = Independent, closer to Democrat
        #**** 6 = Not very strong Democrat
        #**** 7 = Strong Democrat
        #**** NA = No answer
    pid_seven = case_when(
      pid7x == 1 ~ 7,
      pid7x == 2 ~ 6,
      pid7x == 3 ~ 5,
      pid7x == 4 ~ 4,
      pid7x == 5 ~ 3,
      pid7x == 6 ~ 2,
      pid7x == 7 ~ 1
    ),
    #** PID 3 -- 3- Item Partisan identification
      #*** Coded as:
        #**** 1 = Strong Democrat
        #**** 2 = Not very strong Democrat
        #**** 3 = Independent, closer to Democrat
        #**** 4 = Independent
        #**** 5 = Independent, closer to Republican
        #**** 6 = Not very strong Republican
        #**** 7 = Strong Republican
        #**** -7 = No answer
      #*** Recoded to:
        #**** -1 = Republican
        #**** 0 = Independent
        #**** 1 = Democrat
        #**** NA = No answer
    pid_three = case_when(
      pid7x == 1 ~ 1,
      pid7x == 2 ~ 1,
      pid7x == 3 ~ 1,
      pid7x == 4 ~ 0,
      pid7x == 5 ~ -1,
      pid7x == 6 ~ -1,
      pid7x == 7 ~ -1
    ),
    #** Racial resentment 1 -- Blacks should work way up too
      #*** Coded as:
        #**** 1 = Agree Strongly
        #**** 2 = Agree somewhat
        #**** 3 = Neither agree nor disagree
        #**** 4 = Disagree somewhat
        #**** 5 = Disagree strongly
      #*** Recoded to:
        #**** 1 = Disagree strongly
        #**** 2 = Disagree somewhat
        #**** 3 = Neither agree nor disagree
        #**** 4 = Agree somewhat
        #**** 5 = Agree strongly
        #**** NA = Non-response
    racial_resentment_one = case_when(
      rr1 == 1 ~ 5,
      rr1 == 2 ~ 4,
      rr1 == 3 ~ 3,
      rr1 == 4 ~ 2,
      rr1 == 5 ~ 1
    ),
    #** Racial resentment 2 -- Slavery and discrimination has made it harder
      #*** Coded as:
        #**** 1 = Agree Strongly
        #**** 2 = Agree somewhat
        #**** 3 = Neither agree nor disagree
        #**** 4 = Disagree somewhat
        #**** 5 = Disagree strongly
      #*** Recoded to:
        #**** 1 = Agree Strongly
        #**** 2 = Agree somewhat
        #**** 3 = Neither agree nor disagree
        #**** 4 = Disagree somewhat
        #**** 5 = Disagree strongly
    racial_resentment_two = case_when(
      rr2 == 1 ~ 1,
      rr2 == 2 ~ 2,
      rr2 == 3 ~ 3,
      rr2 == 4 ~ 4,
      rr2 == 5 ~ 5
    ),
    #** Racial resentment 3 -- Blacks have gotten less than they deserve
      #*** Coded as:
        #**** 1 = Agree Strongly
        #**** 2 = Agree somewhat
        #**** 3 = Neither agree nor disagree
        #**** 4 = Disagree somewhat
        #**** 5 = Disagree strongly
      #*** Recoded to:
        #**** 1 = Agree Strongly
        #**** 2 = Agree somewhat
        #**** 3 = Neither agree nor disagree
        #**** 4 = Disagree somewhat
        #**** 5 = Disagree strongly
    racial_resentment_three = case_when(
      rr3 == 1 ~ 1,
      rr3 == 2 ~ 2,
      rr3 == 3 ~ 3,
      rr3 == 4 ~ 4,
      rr3 == 5 ~ 5
    ),
    #* Racial resentment 4 -- If Blacks tried harder they would be as well off
      #** Coded as:
        #**** 1 = Agree Strongly
        #**** 2 = Agree somewhat
        #**** 3 = Neither agree nor disagree
        #**** 4 = Disagree somewhat
        #**** 5 = Disagree strongly
      #** Recoded to:
        #**** 1 = Disagree Strongly
        #**** 2 = Disagree somewhat
        #**** 3 = Neither agree nor disagree
        #**** 4 = Agree somewhat
        #**** 5 = Agree strongly
    racial_resentment_four = case_when(
      rr4 == 1 ~ 5,
      rr4 == 2 ~ 4,
      rr4 == 3 ~ 3,
      rr4 == 4 ~ 2,
      rr4 == 5 ~ 1
    ),
    #** Racial resentment -- Additive scale
        #*** Recoded to:
          #**** Additive scale of four previous racial resentment items
    racial_resentment = (
      (racial_resentment_one + racial_resentment_two + racial_resentment_three + racial_resentment_four) / 4 #nolint
    ), # 1 = Disagree strongly - 5 Agree strongly
    #** Ideology -- Self-identified ideology
      #*** Coded as:
        #**** 1 = Very liberal
        #**** 2 = Somewhat liberal
        #**** 3 = Closer to liberals
        #**** 4 = Neither liberal nor conservative
        #**** 5 = Closer to conservatives
        #**** 6 = Somewhat conservative
        #**** 7 = Very conservative
      #*** Recoded to:
        #**** 1 = Very conservative
        #**** 2 = Somewhat conservative
        #**** 3 = Closer to conservatives
        #**** 4 = Neither liberal nor conservative
        #**** 5 = Closer to liberals
        #**** 6 = Somewhat liberal
        #**** 7 = Very liberal
    ideo = case_when(
      lcself == 1 ~ 7,
      lcself == 2 ~ 6,
      lcself == 3 ~ 5,
      lcself == 4 ~ 4,
      lcself == 5 ~ 3,
      lcself == 6 ~ 2,
      lcself == 7 ~ 1
    )
  )

# Store it as a csv file
write.csv(
  anes_19_cleaned,
  file = "data/2019-anes-pilot/anes_2019_pilot_cleaned_2023-01-30.csv"
)