source("~/Dropbox/Data/DfP/recontact/dfp_recontact_cleaning.R")
library(ggplot2)
library(ggthemes)
library(lmtest)
library(sandwich)
library(dotwhisker)
library(dplyr)
library(stargazer)
library(interplot)
library(patchwork)

setwd("~/Dropbox/Projects/White Identity/")


# Helper Functions --------------------------------------------------------
source("~/Dropbox/Misc code examples/AA_function.R")
source("~/Dropbox/Misc code examples/King and Roberts/bootstrapIM.normal.R")

'%!in%' <- function(x,y)!('%in%'(x,y))

print_robust <- function(model, dec = 3, type = "HC0"){
  cov.m1 <- vcovHC(model, type=type)
  std.err <- sqrt(diag(cov.m1))
  r.est <- cbind(Estimate= coef(model), "Robust SE" = std.err,
                 "Pr(>|z|)" = 2*pnorm(abs(coef(model)/std.err), lower.tail=FALSE),
                 LL = coef(model) - 1.96 * std.err,
                 UL = coef(model) + 1.96 * std.err)
  print(round(r.est, dec))
}

# Defining RHS ------------------------------------------------------------
demog_rhs <- c("fem + south + age_sc + educ_sc + inc_sc + inc_dk + newsint_sc + pol_acts")
socid_interact_rhs <- c("race_ident_sc*white + gend_ident_sc*fem + pid_ident_sc + ideo_sc")
socid_interact_rhs <- c("race_ident_sc*white + lfate_sc*white + gend_ident_sc*fem + pid_ident_sc")

# Models ------------------------------------------------------------------
m_college <- lm(as.formula(paste0("free_college ~", socid_interact_rhs, "+", demog_rhs)),
            data = dfp_dat, weights = weight_fullsample)
summary(m_college)

m_ubi <- lm(as.formula(paste0("ubi ~", socid_interact_rhs, "+", demog_rhs)),
                data = dfp_dat, weights = weight_fullsample)
summary(m_ubi)

m_m4a <- lm(as.formula(paste0("m4a ~", socid_interact_rhs, "+", demog_rhs)),
            data = dfp_dat, weights = weight_fullsample)
summary(m_m4a)

m_green <- lm(as.formula(paste0("gndeal ~", socid_interact_rhs, "+", demog_rhs)),
            data = dfp_dat, weights = weight_fullsample)
summary(m_green)


m_college <- lm(as.formula(paste0("free_college ~", socid_interact_rhs, "+", demog_rhs)),
                data = dfp_dat, subset = white == 1, weights = weight_fullsample)
summary(m_college)

m_ubi <- lm(as.formula(paste0("ubi ~", socid_interact_rhs, "+", demog_rhs)),
            data = dfp_dat,  subset = white == 1, weights = weight_fullsample)
summary(m_ubi)

m_m4a <- lm(as.formula(paste0("m4a ~", socid_interact_rhs, "+", demog_rhs)),
            data = dfp_dat,  subset = white == 1, weights = weight_fullsample)
summary(m_m4a)

m_green <- lm(as.formula(paste0("gndeal ~", socid_interact_rhs, "+", demog_rhs)),
              data = dfp_dat,  subset = white == 1, weights = weight_fullsample)
summary(m_green)
