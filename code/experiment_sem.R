library(semTools)
library(lavan)


# Measurement Model -------------------------------------------------------
# * Whites ----------------------------------------------------------------
cfa_fit_white <- '
rac_id =~ NA*rac_id_unimport + rac_id_central + rac_id_good
rac_id ~~ 1*rac_id
rac_id_central ~~ rac_id_good

rr =~ NA*rr_irish + rr_try + rr_pdisc + rr_dless + rr_excuse
rr ~~ 1*rr

rr_pdisc ~~ rr_dless
'
white_cfa <- cfa(cfa_fit_white, data = subset(df, white == 1))
summary(white_cfa, fit.measures = T, standardized = T)
modindices(white_cfa)
residuals(white_cfa, type = "standardized")


# SEM ---------------------------------------------------------------------
# * Whites ----------------------------------------------------------------
#indProd for latent interaction

sem_fit_white <- '
# Structural
college ~ rac_id + rr + treat_white + treat_black + rac_id:treat_white + rac_id:treat_black + rr:treat_white + rr:treat_black

# Measurment
rac_id =~ NA*rac_id_unimport + rac_id_central + rac_id_good
rac_id ~~ 1*rac_id
rac_id_central ~~ rac_id_good

rr =~ NA*rr_irish + rr_try + rr_pdisc + rr_dless + rr_excuse
rr ~~ 1*rr

rr_pdisc ~~ rr_dless
'
white_sem <- cfa(sem_fit_white, data = subset(df, white == 1))
summary(white_sem, fit.measures = T, standardized = T)


m_loans <- lm(loans ~ treat_white*rac_id_sc + treat_black*rac_id_sc + as.factor(pid3) + rr_sc, 
           df, subset = white == 1)
summary(m_loans)
interplot::interplot(m_loans, "rac_id_sc", "treat_white")
interplot::interplot(m_loans, "rac_id_sc", "treat_black")
