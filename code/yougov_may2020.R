library(haven)
library(psych)
library(semTools)
library(lavaan)
library(semTable) # maybe?
# library(irtoys)
# library(ltm)
library(interplot)
library(foreign)

df <- read.spss("~/Dropbox/Projects/White Identity/Data/Taubman Spring 2020/BROW0009_OUTPUT.sav",
                to.data.frame = T, use.value.labels = F)
setwd("~/Dropbox/Projects/White Identity/")

# Cleaning ----------------------------------------------------------------
# ** Demos -------------------------------------------------------------------
df$white <- ifelse(df$race == 1, 1, 0)
df$black <- ifelse(df$race == 2, 1, 0)

df$age <-  2020 - df$birthyr

df$fem <- ifelse(df$gender == 2, 1, 0)

df$coll <- ifelse(df$educ == 5 | df$educ == 6, 1, 0)
df$hs <- ifelse(df$educ <= 2, 1, 0)

df$pid7_r <- ifelse(df$pid7 < 8, df$pid7, 4)
df$pid7_sc <- (df$pid7_r-1)/6
df$pid3 <- NA
df$pid3[which(df$pid7_r < 4)] <- -1
df$pid3[which(df$pid7_r == 4)] <- 0
df$pid3[which(df$pid7_r > 4)] <- 1

df$dem <- ifelse(df$pid3 == -1, 1, 0)
df$rep <- ifelse(df$pid3 == 1, 1, 0)
df$ind <- ifelse(df$pid3 == 0, 1, 0)

df$lib <- ifelse(df$ideo5 < 3, 1, 0)
df$mod <- ifelse(df$ideo5 == 3, 1, 0)
df$con <- ifelse(df$ideo5 == 4 | df$ideo5 == 5, 1, 0)

# ** Experiment --------------------------------------------------------------
df$de_treat
df$treat_control <- ifelse(df$de_treat == 1, 1, 0)
df$treat_white <- ifelse(df$de_treat == 2, 1, 0)
df$treat_black <- ifelse(df$de_treat == 3, 1, 0)

df$de_manip_correct <- ifelse(df$de_exp_check == 4, 1, 0)


# ** DVs ---------------------------------------------------------------------
# higher values denote more support
df$college <- df$de_free_coll*-1 + 7
df$loans <- df$de_loans*-1 + 7
df$welfare <- df$de_welfare*-1 + 7
df$hc <- df$de_hc*-1 + 7
df$enviro <- df$de_enviro*-1 + 7


# ** Identity ----------------------------------------------------------------
# higher values denote agreement
df$rac_id_import <- df$de_id_1-1
df$rac_id_central <- df$de_id_2*-1 + 7
df$rac_id_good <- df$de_id_3*-1 + 7

df$rac_id_sc <- (df$rac_id_import + df$rac_id_central + df$rac_id_good)/18
psych::alpha(subset(df, white == 1, select = c("rac_id_import", "rac_id_central", "rac_id_good")))
cor(subset(df, white == 1, 
           select = c("rac_id_import", "rac_id_central", "rac_id_good")), 
           use = "complete.obs")


# ** RR ----------------------------------------------------------------------
df$rr_irish <- df$de_rr_irish - 1
df$rr_try <- df$de_rr_try - 1
df$rr_pdisc <- df$de_rr_discrim*-1 + 5
df$rr_dless <- df$de_rr_deserve*-1 + 5
df$rr_excuse <- df$de_rr_excuse - 1

df$rr_sc <- (df$rr_irish + df$rr_try + df$rr_pdisc + df$rr_dless + df$rr_excuse)/20
df$rr4_sc <- (df$rr_irish + df$rr_try + df$rr_pdisc + df$rr_dless)/16



# Decriptives -------------------------------------------------------------
# * Manipulation Check ----------------------------------------------------
prop.table(table(df$de_manip_correct[which(df$white == 1)]))

# * DVs -------------------------------------------------------------------
dvs <- c("college", "loans", "welfare", "hc", "enviro")
dv_mean_cont <- round(apply(subset(df, white == 1 & treat_control == 1, select = dvs), 2, mean), 2)
dv_mean_wht <- round(apply(subset(df, white == 1 & treat_white == 1, select = dvs), 2, mean), 2)
dv_mean_blk <- round(apply(subset(df, white == 1 & treat_black == 1, select = dvs), 2, mean), 2)

dv_means <- cbind(dv_mean_cont, dv_mean_wht, dv_mean_blk)
rownames(dv_means) <- c("Free Tuition", "Student Debt", "Welfare", "Single Payer", "Regulate Emissions")
colnames(dv_means) <- c("Control", "White Beneficiary", "Black Beneficiary")

# write.csv(dv_means, file = "./tables and figures/dv_means.csv")

# Treatment Differences
m_col <- lm(college ~ treat_white + treat_black, data = df, subset = white == 1)
summary(m_col)
linearHypothesis(m_col, "treat_white = treat_black")

m_loan <- lm(loans ~ treat_white + treat_black, data = df, subset = white == 1)
summary(m_loan)
linearHypothesis(m_loan, "treat_white = treat_black")

m_welf <- lm(welfare ~ treat_white + treat_black, data = df, subset = white == 1)
summary(m_welf)
linearHypothesis(m_welf, "treat_white = treat_black")

m_hc <- lm(hc ~ treat_white + treat_black, data = df, subset = white == 1)
summary(m_hc)
linearHypothesis(m_hc, "treat_white = treat_black")

m_enviro <- lm(enviro ~ treat_white + treat_black, data = df, subset = white == 1)
summary(m_enviro)
linearHypothesis(m_enviro, "treat_white = treat_black")

## Correlations
lowerCor(subset(df, white == 1, select = c("college", "loans", "welfare", "hc", "enviro")))
lowerCor(subset(df, white == 1 & treat_control == 1, select = c("college", "loans", "welfare", "hc", "enviro")))
lowerCor(subset(df, white == 1 & treat_white == 1, select = c("college", "loans", "welfare", "hc", "enviro")))
lowerCor(subset(df, white == 1 & treat_black == 1, select = c("college", "loans", "welfare", "hc", "enviro")))


# Models ------------------------------------------------------------------
# ** Measurement ----------------------------------------------------------
cfa_dat <- as.data.frame(df)

mod_measurement <- '
rr =~ NA*rr_irish + rr_try + rr_pdisc + rr_dless
rr ~~ 1*rr

rr_pdisc ~~ rr_dless

id =~ NA*rac_id_central + rac_id_good + rac_id_import # loading insignificant
id ~~ 1*id

rac_id_central ~~ rac_id_import
rac_id_good ~~ 0*rac_id_good
'
fit_measurement <- cfa(mod_measurement, 
                       data = cfa_dat, 
                       mimic = "MPlus")
summary(fit_measurement, fit.measures = T, standardized = T)
# residuals(fit, type = "standardized")
# modindices(fit, minimum.value = 10, sort = T)

sem_dat <- data.frame(cfa_dat, predict(fit_measurement))

sem_dat$treat_white_id <- sem_dat$id*sem_dat$treat_white
sem_dat$treat_black_id <- sem_dat$id*sem_dat$treat_black
sem_dat$treat_white_rr <- sem_dat$rr*sem_dat$treat_white
sem_dat$treat_black_rr <- sem_dat$rr*sem_dat$treat_black

sem_dat$treat_white_id_white <- sem_dat$id*sem_dat$treat_white*sem_dat$white
sem_dat$treat_black_id_white <- sem_dat$id*sem_dat$treat_black*sem_dat$white
sem_dat$treat_white_rr_white <- sem_dat$rr*sem_dat$treat_white*sem_dat$white
sem_dat$treat_black_rr_white <- sem_dat$rr*sem_dat$treat_black*sem_dat$white

sem_dat$id_white <- sem_dat$id*sem_dat$white
sem_dat$id_white <- sem_dat$id*sem_dat$white
sem_dat$rr_white <- sem_dat$rr*sem_dat$white
sem_dat$rr_white <- sem_dat$rr*sem_dat$white

sem_dat$treat_white_white <- sem_dat$treat_white*sem_dat$white
sem_dat$treat_black_white <- sem_dat$treat_black*sem_dat$white



sem_dat$treat_white_id <- sem_dat$treat_white*sem_dat$id
sem_dat$treat_black_id <- sem_dat$treat_black*sem_dat$id
sem_dat$treat_white_rr <- sem_dat$treat_white*sem_dat$rr
sem_dat$treat_black_rr <- sem_dat$treat_black*sem_dat$rr


# *** Models -------------------------------------------------------------
# Free College
m_col_id <- lm(college ~ treat_white + treat_black +
              treat_white*id + treat_black*id +
              treat_white*rr + treat_black*rr +
              + educ + age + fem + dem + rep, 
            data = sem_dat)
summary(m_col_id)

interplot(m_col_id, "id", "treat_white", ci = .84, plot = F)
interplot(m_col_id, "id", "treat_black", ci = .84, plot = F)

m_col_rr <- lm(college ~ treat_white + treat_black +
                 treat_white*rr + treat_black*rr +
                 id +
                 + educ + age + fem + dem + rep, 
               data = sem_dat)
summary(m_col_rr)

interplot(m_col_rr, "rr", "treat_white", ci = .84, plot = F)
interplot(m_col_rr, "rr", "treat_black", ci = .84, plot = F)

# Student Debt
m_loan_id <- lm(loans ~ treat_white + treat_black +
               treat_white*id + treat_black*id +
               rr +
               + educ + age + fem + dem + rep, 
             data = sem_dat)
summary(m_loan_id)

interplot(m_loan_id, "id", "treat_white", ci = .84, plot = F)
interplot(m_loan_id, "id", "treat_black", ci = .84, plot = F)

m_loan_rr <- lm(loans ~ treat_white + treat_black +
                  treat_white*rr + treat_black*rr +
                  id +
                  + educ + age + fem + dem + rep, 
                data = sem_dat)
summary(m_loan_rr)

interplot(m_loan_rr, "rr", "treat_white", ci = .84, plot = F)
interplot(m_loan_rr, "rr", "treat_black", ci = .84, plot = F)

# Welfare
m_welf_id <- lm(welfare ~ treat_white + treat_black +
               treat_white*id + treat_black*id +
               rr +
               educ + age + fem + dem + rep, 
             data = sem_dat)
summary(m_welf_id)

interplot(m_welf_id, "id", "treat_white", ci = .84, plot = F)
interplot(m_welf_id, "id", "treat_black", ci = .84, plot = F)


m_welf_rr <- lm(welfare ~ treat_white + treat_black +
                  treat_white*rr + treat_black*rr +
                  id +
                educ + age + fem + dem + rep, 
                data = sem_dat)
summary(m_welf_rr)

interplot(m_welf_rr, "rr", "treat_white", ci = .84, plot = F)
interplot(m_welf_rr, "rr", "treat_black", ci = .84, plot = F)


# HC
m_hc_id <- lm(hc ~ treat_white + treat_black +
             treat_white*id + treat_black*id +
             rr +
             educ + age + fem + dem + rep, 
           data = sem_dat)
summary(m_hc_id)

interplot(m_hc_id, "id", "treat_white", ci = .84, plot = F)
interplot(m_hc_id, "id", "treat_black", ci = .84, plot = F)

m_hc_rr <- lm(hc ~ treat_white + treat_black +
                treat_white*rr + treat_black*rr +
                id +
              educ + age + fem + dem + rep, 
              data = sem_dat)
summary(m_hc_rr)

interplot(m_hc_rr, "rr", "treat_white", ci = .84, plot = F)
interplot(m_hc_rr, "rr", "treat_black", ci = .84, plot = F)

# Environment
m_enviro_id <- lm(enviro ~ treat_white + treat_black +
                 treat_white*id + treat_black*id +
                 rr +
                 educ + age + fem + dem + rep, 
               data = sem_dat)
summary(m_enviro_id)

interplot(m_enviro_id, "id", "treat_white", ci = .84, plot = F)
interplot(m_enviro_id, "id", "treat_black", ci = .84, plot = F)

m_enviro_rr <- lm(enviro ~ treat_white + treat_black +
                    treat_white*rr + treat_black*rr +
                    id +
                  educ + age + fem + dem + rep, 
                  data = sem_dat)
summary(m_enviro_rr)

interplot(m_enviro_rr, "rr", "treat_white", ci = .84, plot = F)
interplot(m_enviro_rr, "rr", "treat_black", ci = .84, plot = F)

# do marginal effects by condition, per DV. DVs as separate panels.
# use 84 and 95% CIs

# ** Structural -----------------------------------------------------------
mod_struc_id <- '
# Measurement
rr =~ NA*rr_irish + rr_try + rr_pdisc + rr_dless
rr ~~ 1*rr

rr_pdisc ~~ rr_dless

id =~ NA*rac_id_central + rac_id_good + rac_id_import # loading insignificant
id ~~ 1*id
rac_id_central ~~ rac_id_import
rac_id_good ~~ 0*rac_id_good

# Structural
college ~ ctw*treat_white + ctww*treat_white_white + ctb*treat_black + ctbw*treat_black_white + cwid*treat_white_id  + cwidw*treat_white_id_white + cbid*treat_black_id + cbidw*treat_black_id_white + cid*id + cidw*id_white + rr + rr_white + cw*white + educ + age + fem + dem + rep #+ lib + con
loans ~ ltw*treat_white + ltww*treat_white_white + ltb*treat_black + ltbw*treat_black_white  + lwid*treat_white_id + lwidw*treat_white_id_white + lbid*treat_black_id + lbidw*treat_black_id_white + lid*id + lidw*id_white + rr + rr_white + lw*white + educ + age + fem + dem + rep #+ lib + con
welfare ~ wtw*treat_white + wtww*treat_white_white + wtb*treat_black + wtbw*treat_black_white + wwid*treat_white_id  + wwidw*treat_white_id_white + wbid*treat_black_id + wbidw*treat_black_id_white + wid*id + widw*id_white + rr + rr_white + ww*white + educ + age + fem + dem + rep #+ lib + con
hc ~ htw*treat_white + htww*treat_white_white + htb*treat_black + htbw*treat_black_white + hwid*treat_white_id  + hwidw*treat_white_id_white + hbid*treat_black_id + hbidw*treat_black_id_white + hid*id + hidw*id_white + rr + rr_white + hw*white + educ + age + fem + dem + rep #+ lib + con
enviro ~ etw*treat_white + etww*treat_white_white + etb*treat_black + etbw*treat_black_white + ewid*treat_white_id  + ewidw*treat_white_id_white + ebid*treat_black_id + ebidw*treat_black_id_white + eid*id + eidw*id_white + rr + rr_white + ew*white + educ + age + fem + dem + rep #+ lib + con

# calculating marginal effects
## college
cIDw := (cwidw + cwid)
cIDb := (cbidw + cbid)
cDIFwbW := cIDw - cIDb
cDIFwbNW := cwid - cbid

## loans
lIDw := (lwidw + lwid)
lIDb := (lbidw + lbid)
lDIFwbW := lIDw - lIDb
lDIFwbNW := lwid - lbid

## welfare
wIDw := (wwidw + wwid)
wIDb := (wbidw + wbid)
wDIFwbW := wIDw - wIDb
wDIFwbNW := wwid - wbid

## hc
hIDw := (hwidw + hwid)
hIDb := (hbidw + hbid)
hDIFwbW := hIDw - hIDb
hDIFwbNW := hwid - hbid

## environment
eIDw := (ewidw + ewid)
eIDb := (ebidw + ebid)
eDIFwbW := eIDw - eIDb
eDIFwbNW := ewid - ebid

# DV error covariances?
# college ~~ 0*loans + 0*welfare + 0*hc + 0*enviro
# loans ~~ 0*welfare + 0*hc + 0*enviro
# welfare ~~ 0*hc + 0*enviro
# hc ~~ 0*enviro
'
fit_struct_id <- cfa(mod_struc_id, 
                  data = sem_dat,
                  mimic = "MPlus",
                  std.lv = TRUE , std.ov = TRUE)
summary(fit_struct_id, fit.measures = T)


mod_struc_rr <- '
# Measurement
rr =~ NA*rr_irish + rr_try + rr_pdisc + rr_dless
rr ~~ 1*rr

rr_pdisc ~~ rr_dless

id =~ NA*rac_id_central + rac_id_good + rac_id_import # loading insignificant
id ~~ 1*id
rac_id_central ~~ rac_id_import
rac_id_good ~~ 0*rac_id_good

# Structural
college ~ ctw*treat_white + ctww*treat_white_white + ctb*treat_black + ctbw*treat_black_white  + cwrr*treat_white_rr + cwrrw*treat_white_rr_white + cbrr*treat_black_rr + cbrrw*treat_black_rr_white + id + id_white + crr*rr + crrw*rr_white + cw*white + educ + age + fem + dem + rep #+ lib + con
loans ~ ltw*treat_white + ltww*treat_white_white + ltb*treat_black + ltbw*treat_black_white  + lwrr*treat_white_rr + lwrrw*treat_white_rr_white + lbrr*treat_black_rr + lbrrw*treat_black_rr_white + id + id_white + lrr*rr + lrrw*rr_white + lw*white + educ + age + fem + dem + rep #+ lib + con
welfare ~ wtw*treat_white + wtww*treat_white_white + wtb*treat_black + wtbw*treat_black_white  + wwrr*treat_white_rr + wwrrw*treat_white_rr_white + wbrr*treat_black_rr  + wbrrw*treat_black_rr_white + id + id_white + wrr*rr + wrrw*rr_white + ww*white + educ + age + fem + dem + rep #+ lib + con
hc ~ htw*treat_white + htww*treat_white_white + htb*treat_black + htbw*treat_black_white  + hwrr*treat_white_rr + hwrrw*treat_white_rr_white + hbrr*treat_black_rr + hbrrw*treat_black_rr_white + id + id_white + hrr*rr + hrrw*rr_white + hw*white + educ + age + fem + dem + rep #+ lib + con
enviro ~ etw*treat_white + etww*treat_white_white + etb*treat_black + etbw*treat_black_white  + ewrr*treat_white_rr + ewrrw*treat_white_rr_white + ebrr*treat_black_rr + ebrrw*treat_black_rr_white + id + id_white + err*rr + errw*rr_white + ew*white + educ + age + fem + dem + rep #+ lib + con

# calculating marginal effects
## college
cRRw := (cwrrw + cwrr)
cRRb := (cbrrw + cbrr)
cDIFwbW := cRRw - cRRb
cDIFwbNW := cwrr - cbrr

## loans
lRRw := (lwrrw + lwrr)
lRRb := (lbrrw + lbrr)
lDIFwbW := lRRw - lRRb
lDIFwbNW := lwrr - lbrr

## welfare
wRRw := (wwrrw + wwrr)
wRRb := (wbrrw + wbrr)
wDIFwbW := wRRw - wRRb
wDIFwbNW := wwrr - wbrr

## hc
hRRw := (hwrrw + hwrr)
hRRb := (hbrrw + hbrr)
hDIFwbW := hRRw - hRRb
hDIFwbNW := hwrr - hbrr

## environment
eRRw := (ewrrw + ewrr)
eRRb := (ebrrw + ebrr)
eDIFwbW := eRRw - eRRb
eDIFwbNW := ewrr - ebrr

# DV error covariances?
# college ~~ 0*loans + 0*welfare + 0*hc + 0*enviro
# loans ~~ 0*welfare + 0*hc + 0*enviro
# welfare ~~ 0*hc + 0*enviro
# hc ~~ 0*enviro
'
fit_struct_rr <- cfa(mod_struc_rr, 
                     data = sem_dat,
                     mimic = "MPlus",
                     std.lv = TRUE , std.ov = TRUE)
summary(fit_struct_rr, fit.measures = T)

# *** Table ---------------------------------------------------------------
semTable(fit_struct_id,
         paramSets = c("slopes"),
         columns = c("eststars", "se"),
         fits = c("chisq", "cfi", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper"),
         varLabels = c("White Treatment", "White Treatment*White",
                       "Black Treatment", "Black Treatment*White", 
                       "White Treatment*Identity", "White Treatment*Identity*White", 
                       "Black Treatment*Identity", "Black Treatment*Identity*White",
                       "Identity", "Identity*White", 
                       "Racial Resentment", "Racial Resentment*White", 
                       "White", "Education", "Age", "Female",
                       "Democrat", "Republican"),
         type = "html", file = "./tables and figures/study2_sem_id.html",
         print.results = FALSE)

semTable(fit_struct_rr,
         paramSets = c("slopes"),
         columns = c("eststars", "se"),
         fits = c("chisq", "cfi", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper"),
         varLabels = c("White Treatment", "White Treatment*White",
                       "Black Treatment", "Black Treatment*White", 
                       "White Treatment*Racial Resentment", "White Treatment*Racial Resentment*White", 
                       "Black Treatment*Racial Resentment", "Black Treatment*Racial Resentment*White",
                       "Identity", "Identity*White", 
                       "Racial Resentment", "Racial Resentment*White", 
                       "White", "Education", "Age", "Female",
                       "Democrat", "Republican"),
         type = "html", file = "./tables and figures/study2_sem_rr.html",
         print.results = FALSE)

# ** Structural (Old) -----------------------------------------------------------
mod_struct <- '
# Measurement
rr =~ NA*rr_irish + rr_try + rr_pdisc + rr_dless
rr ~~ 1*rr

rr_pdisc ~~ rr_dless

id =~ NA*rac_id_central + rac_id_good + rac_id_import # loading insignificant
id ~~ 1*id
rac_id_central ~~ rac_id_import

# Structural
college ~ treat_white + treat_black +  rr + id + educ + age + fem + dem + rep #+ lib + con
# college ~ treat_white + treat_black + treat_white:rr + rr + id + educ + age + fem + dem + rep #+ lib + con
# loans ~ treat_white + treat_black + rr + id + educ + age + fem + dem + rep #+ lib + con
# welfare ~ treat_white + treat_black + rr + id + educ + age + fem + dem + rep #+ lib + con
# hc ~ treat_white + treat_black + rr + id + educ + age + fem + dem + rep #+ lib + con
# enviro ~ treat_white + treat_black + rr + id + educ + age + fem + dem + rep #+ lib + con

# DV error covariances?
'
fit_struct <- cfa(mod_struct, 
                  data = subset(df, white == 1), 
                  mimic = "MPlus")
summary(fit_struct, fit.measures = T, standardized = T)

## tip suggested taking measurement model, exporting predicted values, then inputting back?
## ":" allows for interacting observed variables

# ** IRT ------------------------------------------------------------------
# RR
rr_vars <- c("rr_irish", "rr_try", "rr_pdisc", "rr_dless")

irt_dat_rr <- subset(df, 
                     white == 1, 
                     select = rr_vars)
irt_fit_rr <- grm(irt_dat_rr, IRT.param = T)
irt_fit_rr

plot(irt_fit_rr, type = "ICC")
plot(irt_fit_rr, type = "IIC", legend = T, cx = "topright", lwd = 2)
plot(irt_fit_rr, type = "IIC", items = 0, lwd = 2)

# persons
irt_rr_person <- factor.scores(irt_fit_rr, method = "EB", resp.patterns = irt_dat_rr)
head(irt_rr_person$score.dat)

irt_dat_rr$rr_irt <- irt_rr_person$score.dat$z1
irt_dat_rr$rr_irt_se <- irt_rr_person$score.dat$se.z1

# Identity
id_vars <- c("rac_id_central", "rac_id_good", "rac_id_import")

irt_dat_id <- subset(df, 
                     white == 1, 
                     select = id_vars)
irt_fit_id <- grm(irt_dat_id, IRT.param = T)
irt_fit_id

plot(irt_fit_id, type = "ICC")
plot(irt_fit_id, type = "IIC", legend = T, cx = "topright", lwd = 2)
plot(irt_fit_id, type = "IIC", items = 0, lwd = 2)

# persons
irt_id_person <- factor.scores(irt_fit_id, method = "EB", resp.patterns = irt_dat_id)
head(irt_id_person$score.dat)

irt_dat_id$id_irt <- irt_id_person$score.dat$z1
irt_dat_id$id_irt_se <- irt_id_person$score.dat$se.z1