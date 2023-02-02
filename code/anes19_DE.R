library(lavaan)
library(ltm)
library(stargazer)

source("~/Dropbox/Data/ANES/2019 pilot/anes19_cleaning.R")

setwd("~/dropbox/projects/white identity/")

# Measure Dimensionality --------------------------------------------------
m <- '
id =~ NA*race_id + race_coop
rr =~ NA*specfavr + pdisc + dless + thard

id ~~ 1*id
rr ~~ 1*rr

pdisc ~~ dless
'
fit_idrr <- cfa(m, subset(anes19, white == 1), mimic = "Mplus")
summary(fit_idrr, fit.measures = T, standardized = T)

DVS <- c("hc_supp", "stdloans_supp", "freecoll_supp", "prek_supp", "tax_rich", "reg_ghg")

cor(subset(anes19, white == 1, select = c("race_conscious_id", "rr_sc",
                                          DVS)),
    use = "complete.obs")


fit_idrr <- cfa(m, subset(anes19, white == 1 & pid3 == -1), mimic = "Mplus")
summary(fit_idrr, fit.measures = T, standardized = T)


# Analyses ----------------------------------------------------------------
RHS <- c("pid7_sc", "polsoph", "age_sc", "fem", "inc_wDK", "inc_DK", "educ_sc")
IV <- c("race_conscious_id", "rr_sc")

RHS_int <- paste(RHS, "black", sep = "*", collapse = "+")
IV_int <- paste(IV, "black", sep = "*", collapse = "+")

RHS <- paste(RHS, collapse = "+")
IV <- paste(IV, collapse = "+")

# * Health Care -----------------------------------------------------------
DV <- "hc_supp"

m_hc_wht <- lm(as.formula(paste0(DV, "~", IV, "+", RHS)),
                data = anes19, subset = white == 1, weights = weight)
summary(m_hc_wht)

m_hc_blk <- lm(as.formula(paste0(DV, "~", IV, "+", RHS)),
               data = anes19, subset = black == 1 | latino == 1, weights = weight)
summary(m_hc_blk)

m_hc_whtblk <- lm(as.formula(paste0(DV, "~", IV_int, "+", RHS_int)),
               data = anes19, subset = black == 1 | white == 1, weights = weight)
summary(m_hc_whtblk)

# * Student Loan ----------------------------------------------------------
DV <- "stdloans_supp"

m_loan_wht <- lm(as.formula(paste0(DV, "~", IV, "+", RHS)),
               data = anes19, subset = white == 1, weights = weight)
summary(m_loan_wht)

m_loan_blk <- lm(as.formula(paste0(DV, "~", IV, "+", RHS)),
               data = anes19, subset = black == 1 | latino == 1, weights = weight)
summary(m_loan_blk)

m_loan_whtblk <- lm(as.formula(paste0(DV, "~", IV_int, "+", RHS_int)),
                  data = anes19, subset = black == 1 | white == 1, weights = weight)
summary(m_loan_whtblk)

# * College Debt ----------------------------------------------------------
DV <- "freecoll_supp"

m_fcol_wht <- lm(as.formula(paste0(DV, "~", IV, "+", RHS)),
               data = anes19, subset = white == 1, weights = weight)
summary(m_fcol_wht)

m_fcol_blk <- lm(as.formula(paste0(DV, "~", IV, "+", RHS)),
               data = anes19, subset = black == 1 | latino == 1, weights = weight)
summary(m_fcol_blk)

m_fcol_whtblk <- lm(as.formula(paste0(DV, "~", IV_int, "+", RHS_int)),
                  data = anes19, subset = black == 1 | white == 1, weights = weight)
summary(m_fcol_whtblk)


# * Pre-K -----------------------------------------------------------------
DV <- "prek_supp"

m_prek_wht <- lm(as.formula(paste0(DV, "~", IV, "+", RHS)),
                 data = anes19, subset = white == 1, weights = weight)
summary(m_prek_wht)

m_prek_blk <- lm(as.formula(paste0(DV, "~", IV, "+", RHS)),
                 data = anes19, subset = black == 1 | latino == 1, weights = weight)
summary(m_prek_blk)

m_prek_whtblk <- lm(as.formula(paste0(DV, "~", IV_int, "+", RHS_int)),
                    data = anes19, subset = black == 1 | white == 1, weights = weight)
summary(m_prek_whtblk)

# * Tax Rich --------------------------------------------------------------
DV <- "tax_rich"

m_taxrich_wht <- lm(as.formula(paste0(DV, "~", IV, "+", RHS)),
                 data = anes19, subset = white == 1, weights = weight)
summary(m_taxrich_wht)

m_taxrich_blk <- lm(as.formula(paste0(DV, "~", IV, "+", RHS)),
                 data = anes19, subset = black == 1 | latino == 1, weights = weight)
summary(m_taxrich_blk)

m_taxrich_whtblk <- lm(as.formula(paste0(DV, "~", IV_int, "+", RHS_int)),
                    data = anes19, subset = black == 1 | white == 1, weights = weight)
summary(m_taxrich_whtblk)


# * GHG Emissions ---------------------------------------------------------
DV <- "reg_ghg"

m_ghg_wht <- lm(as.formula(paste0(DV, "~", IV, "+", RHS)),
                 data = anes19, subset = white == 1, weights = weight)
summary(m_ghg_wht)

m_ghg_blk <- lm(as.formula(paste0(DV, "~", IV, "+", RHS)),
                 data = anes19, subset = black == 1 | latino == 1, weights = weight)
summary(m_ghg_blk)

m_ghg_whtblk <- lm(as.formula(paste0(DV, "~", IV_int, "+", RHS_int)),
                    data = anes19, subset = black == 1 | white == 1, weights = weight)
summary(m_ghg_whtblk)

