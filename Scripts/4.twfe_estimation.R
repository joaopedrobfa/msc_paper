#------------------------------------------------------------------------------#
# 
# SECTOR-LEVEL ESTIMATION - TWFE ESTIMATOR
# WEIGHTED USING PNADc SAMPLING WEIGHTS
# 
#------------------------------------------------------------------------------#
# For reproducibility, we will use the exact same dataframes used for BJS weighted
# estimation in Stata.
# dta_pnadc_agg_full = read_dta('Data/dta_pnadc_agg_full.dta')
# dta_pnadc_agg_cohort3 = read_dta('Data/dta_pnadc_agg_cohort3.dta')
# dta_pnadc_agg_cohort7 = read_dta('Data/dta_pnadc_agg_cohort7.dta')
#------------------------------------------------------------------------------#
# 0. Adjusting the cohort variable
#------------------------------------------------------------------------------#

dta_pnadc_agg_full_twfe = dta_pnadc_agg_full %>% 
  mutate(ever_treated = ifelse(cohort == 999, 0, 1),
         treated_post = ifelse(ever_treated == 1 & t >= cohort, 1, 0),
         rel_time = ifelse(cohort == 999, -1000, t - cohort))

dta_pnadc_agg_cohort3_twfe = dta_pnadc_agg_cohort3 %>% 
  mutate(ever_treated = ifelse(cohort == 999, 0, 1),
         treated_post = ifelse(ever_treated == 1 & t >= cohort, 1, 0),
         rel_time = ifelse(cohort == 999, -1000, t - cohort))

dta_pnadc_agg_cohort7_twfe = dta_pnadc_agg_cohort7 %>% 
  mutate(ever_treated = ifelse(cohort == 999, 0, 1),
         treated_post = ifelse(ever_treated == 1 & t >= cohort, 1, 0),
         rel_time = ifelse(cohort == 999, -1000, t - cohort))

#------------------------------------------------------------------------------#
# 1. Overall ATT
#------------------------------------------------------------------------------#

twfe_overall_att <- feols(
  informal_w ~ treated_post | v4013 + t,
  data = dta_pnadc_agg_full_twfe,
  weights = ~n_weighted,
  cluster = ~v4013
)

twfe_overall_att = tibble(
  var = NA,
  coef = twfe_overall_att$coefficients,
  stderr = twfe_overall_att$se,
  tstat = coef / stderr,
  pval = 2 * (1 - pnorm(abs(tstat))),
  ci_lower = coef - 1.96*stderr,
  ci_upper = coef + 1.96*stderr
)

saveRDS(twfe_overall_att, 'Results/twfe_overall_att.RDS')

#------------------------------------------------------------------------------#
# 2. Overall event study
#------------------------------------------------------------------------------#

twfe_overall_es = feols(
  informal_w ~ i(rel_time, ever_treated, ref = c(-1, -1000), keep = -6:9) | v4013 + t,
  data = dta_pnadc_agg_full_twfe,
  weights = ~n_weighted,
  cluster = ~v4013
)

# iplot(twfe_overall_es)

twfe_overall_es = tibble(
  var = NA,
  term = twfe_overall_es$coefficients %>% names() %>% str_extract("-?\\d+") %>% as.numeric(),
  coef = twfe_overall_es$coefficients,
  stderr = twfe_overall_es$se,
  tstat = coef / stderr,
  pval = 2 * (1 - pnorm(abs(tstat))),
  ci_lower = coef - 1.96*stderr,
  ci_upper = coef + 1.96*stderr
)

saveRDS(twfe_overall_es, 'Results/twfe_overall_es.RDS')

#------------------------------------------------------------------------------#
# 3. Cohort 3 ATT
#------------------------------------------------------------------------------#

twfe_cohort3_att <- feols(
  informal_w ~ treated_post | v4013 + t,
  data = dta_pnadc_agg_cohort3_twfe,
  weights = ~n_weighted,
  cluster = ~v4013
)

twfe_cohort3_att = tibble(
  var = NA,
  coef = twfe_cohort3_att$coefficients,
  stderr = twfe_cohort3_att$se,
  tstat = coef / stderr,
  pval = 2 * (1 - pnorm(abs(tstat))),
  ci_lower = coef - 1.96*stderr,
  ci_upper = coef + 1.96*stderr
)

saveRDS(twfe_cohort3_att, 'Results/twfe_cohort3_att.RDS')

#------------------------------------------------------------------------------#
# 4. Cohort 3 event study
#------------------------------------------------------------------------------#

twfe_cohort3_es = feols(
  informal_w ~ i(rel_time, ever_treated, ref = c(-1, -1000), keep = -6:9) | v4013 + t,
  data = dta_pnadc_agg_cohort3_twfe,
  weights = ~n_weighted,
  cluster = ~v4013
)

# iplot(twfe_cohort3_es)

twfe_cohort3_es = tibble(
  var = NA,
  term = twfe_cohort3_es$coefficients %>% names() %>% str_extract("-?\\d+") %>% as.numeric(),
  coef = twfe_cohort3_es$coefficients,
  stderr = twfe_cohort3_es$se,
  tstat = coef / stderr,
  pval = 2 * (1 - pnorm(abs(tstat))),
  ci_lower = coef - 1.96*stderr,
  ci_upper = coef + 1.96*stderr
)

saveRDS(twfe_cohort3_es, 'Results/twfe_cohort3_es.RDS')

#------------------------------------------------------------------------------#
# 5. Cohort 7 ATT
#------------------------------------------------------------------------------#

twfe_cohort7_att <- feols(
  informal_w ~ treated_post | v4013 + t,
  data = dta_pnadc_agg_cohort7_twfe,
  weights = ~n_weighted,
  cluster = ~v4013
)

twfe_cohort7_att = tibble(
  var = NA,
  coef = twfe_cohort7_att$coefficients,
  stderr = twfe_cohort7_att$se,
  tstat = coef / stderr,
  pval = 2 * (1 - pnorm(abs(tstat))),
  ci_lower = coef - 1.96*stderr,
  ci_upper = coef + 1.96*stderr
)

saveRDS(twfe_cohort7_att, 'Results/twfe_cohort7_att.RDS')

#------------------------------------------------------------------------------#
# 6. Cohort 7 event study
#------------------------------------------------------------------------------#

twfe_cohort7_es = feols(
  informal_w ~ i(rel_time, ever_treated, ref = c(-1, -1000), keep = -6:9) | v4013 + t,
  data = dta_pnadc_agg_cohort7_twfe,
  weights = ~n_weighted,
  cluster = ~v4013
)

# iplot(twfe_cohort7_es)

twfe_cohort7_es = tibble(
  var = NA,
  term = twfe_cohort7_es$coefficients %>% names() %>% str_extract("-?\\d+") %>% as.numeric(),
  coef = twfe_cohort7_es$coefficients,
  stderr = twfe_cohort7_es$se,
  tstat = coef / stderr,
  pval = 2 * (1 - pnorm(abs(tstat))),
  ci_lower = coef - 1.96*stderr,
  ci_upper = coef + 1.96*stderr
)

saveRDS(twfe_cohort7_es, 'Results/twfe_cohort7_es.RDS')

