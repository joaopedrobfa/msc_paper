#------------------------------------------------------------------------------#
# 
# SECTOR-LEVEL ESTIMATION - dCdH ESTIMATOR
# WEIGHTED USING PNADc SAMPLING WEIGHTS
# de Chaisemartin and d'Haultfoeuille (2024)
# 
#------------------------------------------------------------------------------#
# For reproducibility, we will use the exact same dataframes used for BJS weighted
# estimation in Stata.
# dta_pnadc_agg_full = read_dta('Data/dta_pnadc_agg_full.dta')
# dta_pnadc_agg_cohort3 = read_dta('Data/dta_pnadc_agg_cohort3.dta')
# dta_pnadc_agg_cohort7 = read_dta('Data/dta_pnadc_agg_cohort7.dta')
#------------------------------------------------------------------------------#
# 0. Creating the treatment variable (dCdH uses treatment instead of cohort)
#------------------------------------------------------------------------------#
dta_pnadc_agg_full_dcdh = dta_pnadc_agg_full %>% 
  mutate(treated = 1*ifelse(cohort == 999, 0, t >= cohort))

dta_pnadc_agg_cohort3_dcdh = dta_pnadc_agg_cohort3 %>% 
  mutate(treated = 1*ifelse(cohort == 999, 0, t >= cohort))

dta_pnadc_agg_cohort7_dcdh = dta_pnadc_agg_cohort7 %>% 
  mutate(treated = 1*ifelse(cohort == 999, 0, t >= cohort))

#------------------------------------------------------------------------------#
# 1. Overall - ATT and event study
#------------------------------------------------------------------------------#

dcdh_overall = did_multiplegt_dyn(
  df = dta_pnadc_agg_full_dcdh,
  outcome = "informal_w",
  group = "v4013",
  time = "t",
  treatment = "treated",
  effects = 9,
  placebo = 6,
  weight = "n_weighted",
  cluster = "v4013",
  graph_off = TRUE
)

dcdh_overall_att = dcdh_overall$results$ATE %>% 
  as_tibble() %>% 
  rename(coef = Estimate,
         stderr = SE,
         ci_lower = `LB CI`,
         ci_upper = `UB CI`) %>% 
  mutate(tstat = coef / stderr,
         pval = 2 * (1 - pnorm(abs(tstat))),
         var = NA) %>% 
  select(var, coef, stderr, tstat, pval, ci_lower, ci_upper, N)

dcdh_overall_es = bind_rows(
  # Post-treatment
  dcdh_overall$results$Effects %>% 
    as_tibble() %>% 
    rename(coef = Estimate,
           stderr = SE,
           ci_lower = `LB CI`,
           ci_upper = `UB CI`) %>% 
    mutate(var = row.names(dcdh_overall$results$Effects) %>% str_squish(),
           term = str_remove(var, "Effect_") %>% as.numeric(),
           term = term - 1, 
           .before = coef) %>% 
    select(-c(N, Switchers, N.w, Switchers.w)) %>% 
    mutate(tstat = coef / stderr,
           pval = 2 * (1 - pnorm(abs(tstat))),
           .after = stderr),
  # Pre-treatment
  dcdh_overall$results$Placebos %>% 
    as_tibble() %>% 
    rename(coef = Estimate,
           stderr = SE,
           ci_lower = `LB CI`,
           ci_upper = `UB CI`) %>% 
    mutate(var = row.names(dcdh_overall$results$Placebos) %>% str_squish(),
           term = str_remove(var, "Placebo_") %>% as.numeric(),
           term = -(term + 1),
           .before = coef) %>% 
    select(-c(N, Switchers, N.w, Switchers.w)) %>% 
    mutate(tstat = coef / stderr,
           pval = 2 * (1 - pnorm(abs(tstat))),
           .after = stderr)
  ) %>% arrange(term)

saveRDS(dcdh_overall_att, "Results/dcdh_overall_att.RDS")
saveRDS(dcdh_overall_es, "Results/dcdh_overall_es.RDS")


#------------------------------------------------------------------------------#
# 2. Cohort 3 - ATT and event study
#------------------------------------------------------------------------------#

dcdh_cohort3 = did_multiplegt_dyn(
  df = dta_pnadc_agg_cohort3_dcdh,
  outcome = "informal_w",
  group = "v4013",
  time = "t",
  treatment = "treated",
  effects = 9,
  placebo = 6,
  weight = "n_weighted",
  cluster = "v4013",
  graph_off = TRUE
)

dcdh_cohort3_att = dcdh_cohort3$results$ATE %>% 
  as_tibble() %>% 
  rename(coef = Estimate,
         stderr = SE,
         ci_lower = `LB CI`,
         ci_upper = `UB CI`) %>% 
  mutate(tstat = coef / stderr,
         pval = 2 * (1 - pnorm(abs(tstat))),
         var = NA) %>% 
  select(var, coef, stderr, tstat, pval, ci_lower, ci_upper, N)

dcdh_cohort3_es = bind_rows(
  # Post-treatment
  dcdh_cohort3$results$Effects %>% 
    as_tibble() %>% 
    rename(coef = Estimate,
           stderr = SE,
           ci_lower = `LB CI`,
           ci_upper = `UB CI`) %>% 
    mutate(var = row.names(dcdh_cohort3$results$Effects) %>% str_squish(),
           term = str_remove(var, "Effect_") %>% as.numeric(),
           term = term - 1, 
           .before = coef) %>% 
    select(-c(N, Switchers, N.w, Switchers.w)) %>% 
    mutate(tstat = coef / stderr,
           pval = 2 * (1 - pnorm(abs(tstat))),
           .after = stderr),
  # Pre-treatment
  dcdh_cohort3$results$Placebos %>% 
    as_tibble() %>% 
    rename(coef = Estimate,
           stderr = SE,
           ci_lower = `LB CI`,
           ci_upper = `UB CI`) %>% 
    mutate(var = row.names(dcdh_cohort3$results$Placebos) %>% str_squish(),
           term = str_remove(var, "Placebo_") %>% as.numeric(),
           term = -(term + 1),
           .before = coef) %>% 
    select(-c(N, Switchers, N.w, Switchers.w)) %>% 
    mutate(tstat = coef / stderr,
           pval = 2 * (1 - pnorm(abs(tstat))),
           .after = stderr)
) %>% arrange(term)

saveRDS(dcdh_cohort3_att, "Results/dcdh_cohort3_att.RDS")
saveRDS(dcdh_cohort3_es, "Results/dcdh_cohort3_es.RDS")


#------------------------------------------------------------------------------#
# 3. Cohort 7 - ATT and event study
#------------------------------------------------------------------------------#

dcdh_cohort7 = did_multiplegt_dyn(
  df = dta_pnadc_agg_cohort7_dcdh,
  outcome = "informal_w",
  group = "v4013",
  time = "t",
  treatment = "treated",
  effects = 9,
  placebo = 6,
  weight = "n_weighted",
  cluster = "v4013",
  graph_off = TRUE
)

dcdh_cohort7_att = dcdh_cohort7$results$ATE %>% 
  as_tibble() %>% 
  rename(coef = Estimate,
         stderr = SE,
         ci_lower = `LB CI`,
         ci_upper = `UB CI`) %>% 
  mutate(tstat = coef / stderr,
         pval = 2 * (1 - pnorm(abs(tstat))),
         var = NA) %>% 
  select(var, coef, stderr, tstat, pval, ci_lower, ci_upper, N)

dcdh_cohort7_es = bind_rows(
  # Post-treatment
  dcdh_cohort7$results$Effects %>% 
    as_tibble() %>% 
    rename(coef = Estimate,
           stderr = SE,
           ci_lower = `LB CI`,
           ci_upper = `UB CI`) %>% 
    mutate(var = row.names(dcdh_cohort7$results$Effects) %>% str_squish(),
           term = str_remove(var, "Effect_") %>% as.numeric(),
           term = term - 1, 
           .before = coef) %>% 
    select(-c(N, Switchers, N.w, Switchers.w)) %>% 
    mutate(tstat = coef / stderr,
           pval = 2 * (1 - pnorm(abs(tstat))),
           .after = stderr),
  # Pre-treatment
  dcdh_cohort7$results$Placebos %>% 
    as_tibble() %>% 
    rename(coef = Estimate,
           stderr = SE,
           ci_lower = `LB CI`,
           ci_upper = `UB CI`) %>% 
    mutate(var = row.names(dcdh_cohort7$results$Placebos) %>% str_squish(),
           term = str_remove(var, "Placebo_") %>% as.numeric(),
           term = -(term + 1),
           .before = coef) %>% 
    select(-c(N, Switchers, N.w, Switchers.w)) %>% 
    mutate(tstat = coef / stderr,
           pval = 2 * (1 - pnorm(abs(tstat))),
           .after = stderr)
) %>% arrange(term)

saveRDS(dcdh_cohort7_att, "Results/dcdh_cohort7_att.RDS")
saveRDS(dcdh_cohort7_es, "Results/dcdh_cohort7_es.RDS")

