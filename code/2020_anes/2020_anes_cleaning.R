# Title: 2020 ANES pilot Cleaning

# Notes:
  #* Description: Script for cleaning 2020 ANES pilot
  #* Updated: 2023-01-23
  #* Updated by: dcr

# Setup
  #* Load libraries
box::use(
    #** Helpful pipe operator function
    magrittr = magrittr[`%>%`],
    #** Helpful function for loading .dta file
    haven = haven[read_dta],
    #** Helpful functions for data cleaning
    dplyr = dplyr[
        mutate,
        mutate_if,
        case_when,
        filter,
        group_by,
        summarize,
        select
    ],
    #** Helpful functions for target construction
    tidycensus = tidycensus[
        census_api_key
    ],
    #** Helpful functions for weight raking
    pewmethods = pewmethods[
        create_raking_targets,
        rake_survey
    ],
)

  #* Load data
anes_2020_original <- read.csv(
  file = "data/2020-anes-pilot/anes_pilot_2020ets_csv.csv"
)

# Cleaning

anes_2020_clean <- anes_2020_original %>%
    mutate_if(is.factor, as.numeric) %>%
    mutate(
    #* Dependent variables
        #** free college - Support for free tuition
            #*** Coded as:
                #**** 1 = Favor a great deal
                #**** 2 = Favor a moderate amount
                #**** 3 = Favor a little
                #**** 4 = Neither favor nor oppose
                #**** 5 = Oppose a little
                #**** 6 = Oppose a moderate amount
                #**** 7 = Oppose a great deal
                #**** Values above 7 = NA
            #*** Recoded to:
                #**** 1 = Oppose a great deal
                #**** 2 = Oppose a moderate amount
                #**** 3 = Oppose a little
                #**** 4 = Neither favor nor oppose
                #**** 5 = Favor a little
                #**** 6 = Favor a moderate amount
                #**** 7 = Favor a great deal
                #**** NA = values above 7
        free_college = case_when(
            freecol1 == 1 ~ 7,
            freecol1 == 2 ~ 6,
            freecol1 == 3 ~ 5,
            freecol1 == 4 ~ 4,
            freecol1 == 5 ~ 3,
            freecol1 == 6 ~ 2,
            freecol1 == 7 ~ 1,
            freecol2 == 1 ~ 7,
            freecol2 == 2 ~ 6,
            freecol2 == 3 ~ 5,
            freecol2 == 4 ~ 4,
            freecol2 == 5 ~ 3,
            freecol2 == 6 ~ 2,
            freecol2 == 7 ~ 1
        ),
        #** Student loans -- Support for cancelling all existing student loans
            #*** Coded as:
                #**** 1 = Favor a great deal
                #**** 2 = Favor a moderate amount
                #**** 3 = Favor a little
                #**** 4 = Neither favor nor oppose
                #**** 5 = Oppose a little
                #**** 6 = Oppose a moderate amount
                #**** 7 = Oppose a great deal
                #**** Values above 7 = NA
            #*** Recoded to:
                #**** 1 = Oppose a great deal
                #**** 2 = Oppose a moderate amount
                #**** 3 = Oppose a little
                #**** 4 = Neither favor nor oppose
                #**** 5 = Favor a little
                #**** 6 = Favor a moderate amount
                #**** 7 = Favor a great deal
                #**** NA = Values above 7
        loan_forgiveness = case_when(
            loans2 == 1 ~ 7,
            loans2 == 2 ~ 6,
            loans2 == 3 ~ 5,
            loans2 == 4 ~ 4,
            loans2 == 5 ~ 3,
            loans2 == 6 ~ 2,
            loans2 == 7 ~ 1
        ),
        #** Universal healthcare -- Favorability of universal healthcare
            #*** Coded as:
                #**** 1 = Favor a great deal
                #**** 2 = Favor moderately
                #**** 3 = Favor a little
                #**** 4 = Neither favor nor oppose
                #**** 5 = Oppose a little
                #**** 6 = Oppose moderately
                #**** 7 = Oppose a great deal
                #**** Values above 7 = NA
            #*** Recoded to:
                #**** 1 = Oppose a great deal
                #**** 2 = Oppose moderately
                #**** 3 = Oppose a little
                #**** 4 = Neither favor nor oppose
                #**** 5 = Favor a little
                #**** 6 = Favor moderately
                #**** 7 = Favor a great deal
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
        #** White -- Is the Respondent White?
            #*** Coded as:
                #**** 1 = White
                #**** 2 = Black
                #**** 3 = Asian
                #**** 4 = Mixed
                #**** 5 = Hispanic
                #**** 6 = American Indian or Alaska Native
                #**** 7 = Native Hawaiian, or other Pacific Islander
                #**** 9 = Refused
            #*** Recoded to:
                #**** 0 = Non-white
                #**** 1 = White, non-Hispanic
                #**** NA = Refused/Don't know
        white = case_when(
            race7 == 1 ~ 1,
            race7 > 1 & race7 < 8 ~ 0
        ),
        #** Racial identification -- How important race is to identity
            #*** Coded as:
                #**** 1 = Not at all important
                #**** 2 = A little important
                #**** 3 = Moderately important
                #**** 4 = Very important
                #**** 5 = Extremely important
                #**** 6 = Not applicable
                #**** 8 = NA
            #*** Recoded to:
                #**** 1 = Not at all important
                #**** 2 = A little important
                #**** 3 = Moderately important
                #**** 4 = Very important
                #**** 5 = Extremely important
                #**** NA = Not applicable, NA
        racial_identity = case_when(
            groupid2a_lat == 1 ~ 1,
            groupid2a_lat == 2 ~ 2,
            groupid2a_lat == 3 ~ 3,
            groupid2a_lat == 4 ~ 4,
            groupid2a_lat == 5 ~ 5,
            groupid2a_asi == 1 ~ 1,
            groupid2a_asi == 2 ~ 2,
            groupid2a_asi == 3 ~ 3,
            groupid2a_asi == 4 ~ 4,
            groupid2a_asi == 5 ~ 5,
            groupid2a_bla == 1 ~ 1,
            groupid2a_bla == 2 ~ 2,
            groupid2a_bla == 3 ~ 3,
            groupid2a_bla == 4 ~ 4,
            groupid2a_bla == 5 ~ 5,
            groupid2a_hpi == 1 ~ 1,
            groupid2a_hpi == 2 ~ 2,
            groupid2a_hpi == 3 ~ 3,
            groupid2a_hpi == 4 ~ 4,
            groupid2a_hpi == 5 ~ 5,
            groupid2a_whi == 1 ~ 1,
            groupid2a_whi == 2 ~ 2,
            groupid2a_whi == 3 ~ 3,
            groupid2a_whi == 4 ~ 4,
            groupid2a_whi == 5 ~ 5,
            groupid2a_nat == 1 ~ 1,
            groupid2a_nat == 2 ~ 2,
            groupid2a_nat == 3 ~ 3,
            groupid2a_nat == 4 ~ 4,
            groupid2a_nat == 5 ~ 5,
            groupid2b_lat == 1 ~ 1,
            groupid2b_lat == 2 ~ 2,
            groupid2b_lat == 3 ~ 3,
            groupid2b_lat == 4 ~ 4,
            groupid2b_lat == 5 ~ 5,
            groupid2b_asi == 1 ~ 1,
            groupid2b_asi == 2 ~ 2,
            groupid2b_asi == 3 ~ 3,
            groupid2b_asi == 4 ~ 4,
            groupid2b_asi == 5 ~ 5,
            groupid2b_bla == 1 ~ 1,
            groupid2b_bla == 2 ~ 2,
            groupid2b_bla == 3 ~ 3,
            groupid2b_bla == 4 ~ 4,
            groupid2b_bla == 5 ~ 5,
            groupid2b_hpi == 1 ~ 1,
            groupid2b_hpi == 2 ~ 2,
            groupid2b_hpi == 3 ~ 3,
            groupid2b_hpi == 4 ~ 4,
            groupid2b_hpi == 5 ~ 5,
            groupid2b_whi == 1 ~ 1,
            groupid2b_whi == 2 ~ 2,
            groupid2b_whi == 3 ~ 3,
            groupid2b_whi == 4 ~ 4,
            groupid2b_whi == 5 ~ 5,
            groupid2b_nat == 1 ~ 1,
            groupid2b_nat == 2 ~ 2,
            groupid2b_nat == 3 ~ 3,
            groupid2b_nat == 4 ~ 4,
            groupid2b_nat == 5 ~ 5
        ),
        #** PID 7 -- 7-item partisan identification
            #*** Coded as:
                #**** 1 = Strong Republican
                #**** 2 = Not very strong Republican
                #**** 3 = Independent-Republican
                #**** 4 = Independent
                #**** 5 = Independent-Democrat
                #**** 6 = Not very strong Democrat
                #**** 7 = Strong Democrat
                #**** 9 = Missing
            #*** Recoded to:
                #**** 1 = Strong Republican
                #**** 2 = Not very strong Republican
                #**** 3 = Independent-Republican
                #**** 4 = Independent
                #**** 5 = Independent-Democrat
                #**** 6 = Not very strong Democrat
                #**** 7 = Strong Democrat
                #**** NA = Missing
        pid_seven = ifelse(
            pid7 > 7, NA, pid7
        ),
        #** PID 3 -- 3-item party identification
            #*** Coded as:
                #**** 1 = Strong Republican
                #**** 2 = Not very strong Republican
                #**** 3 = Independent-Republican
                #**** 4 = Independent
                #**** 5 = Independent-Democrat
                #**** 6 = Not very strong Democrat
                #**** 7 = Strong Republican
                #**** 9 = Refused
            #*** Recoded to:
                #**** -1 = Republican
                #**** 0 = Independent
                #**** 1 = Democrat
        pid_three = case_when(
            pid7 == 1 ~ -1,
            pid7 == 2 ~ -1,
            pid7 == 3 ~ -1,
            pid7 == 4 ~ 0,
            pid7 == 5 ~ 1,
            pid7 == 6 ~ 1,
            pid7 == 7 ~ 1
        ),
    #* Control variables
        #** Family income -- Household income
            #*** Coded as:
                #**** Values between 1-26
                #**** Values greater than 26 = NA
            #*** Recoded to:
                #**** Values between 1-26
                #**** NA = Values greater than 26
        family_income = case_when(
            inc_anes <= 26 ~ inc_anes
        ),
        #** Age -- Age of respondent
            #*** Coded as:
                #**** Values between 1-110
            #*** Recoded to:
                #**** 1 = 18-24
                #**** 2 = 25-34
                #**** 3 = 35-44
                #**** 4 = 45-54
                #**** 5 = 55-64
                #**** 6 = 65+
        age = case_when(
            age <= 17 ~ 0,
            age >= 18 & age <= 24 ~ 1,
            age >= 25 & age <= 34 ~ 2,
            age >= 35 & age <= 44 ~ 3,
            age >= 45 & age <= 54 ~ 4,
            age >= 55 & age <= 64 ~ 5,
            age >= 65 ~ 6
        ),
        #** female -- sex of respondent
            #*** Coded as:
                #**** 1 = Male
                #**** 2 = Female
            #*** Recoded to:
                #**** 0 = Male
                #**** 1 = Female
        female = case_when(
            sex == 1 ~ 0,
            sex == 2 ~ 1
        ),
        #** education -- education level of respondent
            #*** Coded as:
                #**** 1 = Didn't complete HS
                #**** 2 = HS Graduate
                #**** 3 = Some college
                #**** 4 = Associates
                #**** 5 = Bachelors
                #**** 6 = Masters
                #**** 7 = Professional
                #**** 8 = Doctorate
            #*** Recoded to:
                #**** 1 = Didn't complete HS
                #**** 2 = HS Graduate
                #**** 3 = Some College
                #**** 4 = Associates
                #**** 5 = Bachelors
                #**** 6 = Masters
                #**** 7 = Professional
                #**** 8 = Doctorate
        education = ifelse(
            educ >= 1 & educ <= 8, educ, NA
        ),
        #** Racial resentment one -- Blacks should overcome prejudice
            #*** Coded as:
                #**** 1 = Agree strongly
                #**** 2 = Agree somewhat
                #**** 3 = Neither agree nor disagree
                #**** 4 = Disagree somewhat
                #**** 5 = Disagree Strongly
                #**** Values above 5 = NA
            #*** Recoded to:
                #**** 1 = Disagree strongly
                #**** 2 = Disagree somewhat
                #**** 3 = Neither agree nor disagree
                #**** 4 = Agree somewhat
                #**** 5 = Agree strongly
                #**** NA = Values above 5
        racial_resentment_one = case_when(
            rr1 == 1 ~ 5,
            rr1 == 2 ~ 4,
            rr1 == 3 ~ 3,
            rr1 == 4 ~ 2,
            rr1 == 5 ~ 1
        ),
        #** Racial resentment 2 -- Slavery and discrimination
            #*** Coded as:
                #**** 1 = Agree strongly
                #**** 2 = Agree somewhat
                #**** 3 = Neither agree nor disagree
                #**** 4 = Disagree somewhat
                #**** 5 = Disagree strongly
                #**** Values above 5 = NA
            #*** Recoded  to:
                #**** 1 = Agree strongly
                #**** 2 = Agree somewhat
                #**** 3 = Neither agree nor disagree
                #**** 4 = Disagree somewhat
                #**** 5 = Disagree strongly
                #**** NA = Values above 5
        racial_resentment_two = ifelse(
            rr2 >= 1 & rr2 <= 5, rr2, NA
        ),
        #** Racial resentment 3 -- Blacks have gotten less than they deserve
            #*** Coded as:
                #**** 1 = Agree strongly
                #**** 2 = Agree somewhat
                #**** 3 = Neither agree nor disagree
                #**** 4 = Disagree somewhat
                #**** 5 = Disagree strongly
                #**** Values above 5 = NA
            #*** Recoded  to:
                #**** 1 = Agree strongly
                #**** 2 = Agree somewhat
                #**** 3 = Neither agree nor disagree
                #**** 4 = Disagree somewhat
                #**** 5 = Disagree strongly
                #**** NA = Values above 5
        racial_resentment_three = ifelse(
            rr3 >= 1 & rr3 <= 5, rr3, NA
        ),
        #** Racial resentment four -- Blacks should try harder
            #*** Coded as:
                #**** 1 = Agree strongly
                #**** 2 = Agree somewhat
                #**** 3 = Neither agree nor disagree
                #**** 4 = Disagree somewhat
                #**** 5 = Disagree Strongly
                #**** Values above 5 = NA
            #*** Recoded to:
                #**** 1 = Disagree strongly
                #**** 2 = Disagree somewhat
                #**** 3 = Neither agree nor disagree
                #**** 4 = Agree somewhat
                #**** 5 = Agree strongly
                #**** NA = Values above 5
        racial_resentment_four = case_when(
            rr4 == 1 ~ 5,
            rr4 == 2 ~ 4,
            rr4 == 3 ~ 3,
            rr4 == 4 ~ 2,
            rr4 == 5 ~ 1
        ),
        #** Racial resentment -- additive scale of racial resentment
        racial_resentment = (
            (racial_resentment_one + racial_resentment_two + racial_resentment_three + racial_resentment_four) / 4 #nolint
        ),
        #** Ideology -- Self-reported ideology
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
                #**** 6 = SOmewhat liberal
                #**** 7 = Very liberal
        ideology = case_when(
            lcself == 1 ~ 7,
            lcself == 2 ~ 6,
            lcself == 3 ~ 5,
            lcself == 4 ~ 4,
            lcself == 5 ~ 3,
            lcself == 6 ~ 2,
            lcself == 7 ~ 1
        ),
    #* Survey weights
        #** Census region
        census_region = case_when(
            state == 8 ~ 3,
            state == 20 ~ 3,
            state == 48 ~ 3,
            state == 49 ~ 3,
            state == 17 ~ 3,
            state == 36 ~ 3,
            state == 46 ~ 3,
            state == 33 ~ 3,
            state == 40 ~ 3,
            state == 10 ~ 3,
            state == 9 ~ 3,
            state == 24 ~ 3,
            state == 1 ~ 3,
            state == 42 ~ 3,
            state == 4 ~ 3,
            state == 18 ~ 3,
            state == 43 ~ 3,
            state == 5 ~ 4,
            state == 37 ~ 4,
            state == 47 ~ 4,
            state == 26 ~ 4,
            state == 51 ~ 4,
            state == 12 ~ 4,
            state == 6 ~ 4,
            state == 44 ~ 4,
            state == 3 ~ 4,
            state == 31 ~ 4,
            state == 28 ~ 4,
            state == 2 ~ 4,
            state == 11 ~ 4,
            state == 34 ~ 2,
            state == 41 ~ 2,
            state == 23 ~ 2,
            state == 50 ~ 2,
            state == 15 ~ 2,
            state == 27 ~ 2,
            state == 16 ~ 2,
            state == 25 ~ 2,
            state == 13 ~ 2,
            state == 14 ~ 2,
            state == 22 ~ 2,
            state == 35 ~ 2,
            state == 19 ~ 1,
            state == 45 ~ 1,
            state == 29 ~ 1,
            state == 21 ~ 1,
            state == 39 ~ 1,
            state == 7 ~ 1,
            state == 32 ~ 1,
            state == 29 ~ 1,
            state == 38 ~ 1,
            state == 30 ~ 1
        )
  )

print("ANES DATA SUCCESSFULLY CLEANED")

# Survey weights

#* Calculating weights
    #* Loading ACS
acs_original <- read_dta("data/2020-anes-pilot/acs-clean.dta")
    #* Cleaning ACS
acs <- acs_original %>%
    mutate(
        #** Age -- Age of the population
            #*** Coded as (cont):
                #**** Continuous
            #*** Recode to (cat):
                #**** 0 = younger than 17
                #**** 1 = 18 - 24
                #**** 2 = 25 - 34
                #**** 3 = 35 - 44
                #**** 4 = 45 - 54
                #**** 5 = 55 - 64
                #**** 6 = older than 65
        age = case_when(
            AGEP <= 17 ~ 0,
            AGEP >= 18 & AGEP <= 24 ~ 1,
            AGEP >= 25 & AGEP <= 34 ~ 2,
            AGEP >= 35 & AGEP <= 44 ~ 3,
            AGEP >= 45 & AGEP <= 54 ~ 4,
            AGEP >= 55 & AGEP <= 64 ~ 5,
            AGEP >= 65 ~ 6
        ),
        #** Female -- Sex of the population
            #*** Coded as (cat):
                #**** 1 = Male
                #**** 2 = Female
            #*** Recoded to (cat):
                #**** 0 = Male
                #**** 1 = Female
        female = case_when(
            SEX == 1 ~ 0,
            SEX == 2 ~ 1
        ),
        #** Education -- Education of the population
            #*** Coded as (cat):
                #**** 1 - 24
            #*** Recoded to (cat):
                #**** 1 - 8
        education = case_when(
            SCHL == "bb" ~ 1,
            SCHL <= 15 ~ 1,
            SCHL == 16 ~ 2,
            SCHL == 17 ~ 2,
            SCHL == 18 ~ 3,
            SCHL == 19 ~ 3,
            SCHL == 20 ~ 4,
            SCHL == 21 ~ 5,
            SCHL == 22 ~ 6,
            SCHL == 23 ~ 7,
            SCHL == 24 ~ 8
        ),
        #** Race -- Racial composition of the population
            #*** Coded as (nom):
            #*** Recoded to (nom):
                #**** 1 = White
                #**** 2 = Black
                #**** 3 = Asia
                #**** 4 = Mixed
                #**** 5 = Hispanic
                #**** 6 = American indian or Alaskan Native
                #**** 7 = Pacific Islander
        race = case_when(
            RAC1P == 1 ~ 1,
            RAC1P == 2 ~ 2,
            RAC1P == 3 ~ 6,
            RAC1P == 4 ~ 6,
            RAC1P == 5 ~ 6,
            RAC1P == 6 ~ 3,
            RAC1P == 7 ~ 7,
            RAC1P == 8 ~ 5,
            RAC1P == 9 ~ 4
        )
) %>%
    #* Calculate weight variable
    group_by(age, female, education, race, REGION) %>%
    summarize(total_population = sum(PWGTP)) %>%
    mutate(
        us_population_total = 328239523,
        weight = total_population / us_population_total
) %>%
    #* filter out those below 18 years old
    filter(age >= 1) %>%
    #* add factors to weight columns
    mutate(
        age = factor(
            age,
            levels = c(1:6),
            labels = c(
                "18-24",
                "25-34",
                "35-44",
                "45-54",
                "55-64",
                "65+"
            ),
            exclude = NULL
        ),
        female = factor(
            female,
            levels = c(0:1),
            labels = c(
                "Male",
                "Female"
            ),
            exclude = NULL
        ),
        region = factor(
            REGION,
            levels = c(1:4),
            labels = c(
                "Northeast",
                "Midwest",
                "South",
                "West"
            ),
            exclude = NULL
        ),
        education = factor(
            education,
            levels = c(1:8),
            labels = c(
                "Did not complete HS",
                "HS Graduate",
                "Some college",
                "Associates",
                "Bachelors",
                "Masters",
                "Professional",
                "Doctorate"
            ),
            exclude = NULL
        ),
        race = factor(
            race,
            levels = c(1:7),
            labels = c(
                "White",
                "Black",
                "Asian",
                "Mixed",
                "Hispanic",
                "American Indian or Alaskan native",
                "Pacific Islander"
            ),
            exclude = NULL
        )
)

    #* Create raking population targets
raking_targets <- create_raking_targets(
    acs,
    vars = c(
        "age",
        "female",
        "education",
        "race",
        "region"
    ),
    wt = "weight"
)

    #* Convert sample target variables to factors for raking
anes_raking <- anes_2020_clean %>%
    mutate(
        rk_age = factor(
            age,
            levels = c(1:6),
            labels = c(
                "18-24",
                "25-34",
                "35-44",
                "45-54",
                "55-64",
                "65+"
            ),
            exclude = NULL
        ),
        rk_female = factor(
            female,
            levels = c(0:1),
            labels = c(
                "Male",
                "Female"
            ),
            exclude = NULL
        ),
        rk_education = factor(
            education,
            levels = c(1:8),
            labels = c(
                "Did not complete HS",
                "HS Graduate",
                "Some college",
                "Associates",
                "Bachelors",
                "Masters",
                "Professional",
                "Doctorate"
            ),
            exclude = NULL
        ),
        rk_race = factor(
            race7,
            levels = c(1:7),
            labels = c(
                "White",
                "Black",
                "Asian",
                "Mixed",
                "Hispanic",
                "American Indian or Alaskan native",
                "Pacific Islander"
            ),
            exclude = NULL
        ),
        rk_region = factor(
            census_region,
            levels = c(1:4),
            labels = c(
                "Northeast",
                "Midwest",
                "South",
                "West"
            ),
            exclude = NULL
        )
    ) %>%
    select(
        rk_age,
        rk_female,
        rk_education,
        rk_race,
        rk_region
    )

    #* Perform the raking and add the weight column\

anes_weighted <- anes_2020_clean %>%
    mutate(
        estimated_weight = rake_survey(
            anes_raking,
            pop_margins = raking_targets
        )
    )

print("RAKING SUCCESSFULLY COMPLETED")

# Store cleaned data
write.csv(
    anes_weighted,
    file = "data/2020-anes-pilot/anes_2020_pilot_cleaned_2023-01-31.csv"
)

print("ANES WITH WEIGHTS FILE SUCCESSFULLY GENERATED")