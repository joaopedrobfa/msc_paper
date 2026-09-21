#------------------------------------------------------------------------------#
# 
# SECTOR-LEVEL ESTIMATION - BJS IMPUTATION ESTIMATOR
# NOT WEIGHTED BY PNADc SAMPLING WEIGHTS
# 
#------------------------------------------------------------------------------#
# pnadc_panel_aggregate = readRDS('Data/pnadc_panel_aggregate.rds')
#------------------------------------------------------------------------------#
# 0. Adjusting the data
#------------------------------------------------------------------------------#
# For some reason, the BJS estimator function requires the unit id variable to be
# of type numeric instead of character.

pnadc_panel_aggregate = pnadc_panel_aggregate %>% 
  mutate(v4013 = as.numeric(v4013))

#------------------------------------------------------------------------------#
# 1. Overall ATT
#------------------------------------------------------------------------------#

bjs_overall_att_unw = did_imputation(
  data = pnadc_panel_aggregate,
  yname = "informal",
  gname = "cohort",
  tname = "t",
  idname = "v4013",
  wname = "n",
  cluster_var = "v4013"
)

bjs_overall_att_unw = bjs_overall_att_unw %>% 
  as_tibble() %>% 
  rename(var = term, 
         coef = estimate, 
         stderr = std.error, 
         ci_lower = conf.low, 
         ci_upper = conf.high) %>% 
  mutate(tstat = coef / stderr,
         pval = 2 * (1 - pnorm(abs(tstat))),
         .after = stderr)

# Saving
saveRDS(bjs_overall_att_unw, "Results/bjs_overall_att_unw.RDS")

#------------------------------------------------------------------------------#
# 2. Aggregated event-study
#------------------------------------------------------------------------------#

bjs_overall_es_unw = did_imputation(
  data = pnadc_panel_aggregate,
  yname = "informal",
  gname = "cohort",
  tname = "t",
  idname = "v4013",
  wname = "n",
  cluster_var = "v4013",
  horizon = TRUE,
  pretrends = TRUE
)

bjs_overall_es_unw = bjs_overall_es_unw %>% 
  as_tibble() %>% 
  mutate(term = as.numeric(term),
         tstat = coef / stderr,
         pval = 2 * (1 - pnorm(abs(tstat))), .after = stderr) %>% 
  rename(coef = estimate, stderr = std.error, ci_lower = conf.low, ci_upper = conf.high)

# Saving
saveRDS(bjs_overall_es_unw, "Results/bjs_overall_es_unw.RDS")

bjs_overall_es_unw %>% 
  as_tibble() %>% 
  mutate(term = as.numeric(term)) %>% 
  ggplot(aes(x = term)) +
  theme_bw() +
  geom_hline(yintercept = 0, color = 'red', linetype = 'dashed') +
  geom_point(aes(y = estimate), size = 2.5) + 
  geom_point(aes(x = -1, y = 0), size = 2.5) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.4, linewidth = 1)+
  scale_x_continuous(breaks = -6:10)+
  theme(panel.grid.minor = element_blank()) +
  labs(y = "Estimate", x = "Period relative to treatment")

#------------------------------------------------------------------------------#
# 3. Cohort-specific ATTs
#------------------------------------------------------------------------------#

# Sept 2012 cohort ATT
# Keep never-treated + Sept 2012 cohort only
pnadc_agg_cohort3 = pnadc_panel_aggregate %>% filter(cohort %in% c(3, NA) | is.na(cohort))
bjs_cohort3_att_unw = did_imputation(
  data = pnadc_agg_cohort3,
  yname = "informal",
  gname = "cohort",
  tname = "t",
  idname = "v4013",
  wname = "n",
  cluster_var = "v4013"
)

bjs_cohort3_att_unw = bjs_cohort3_att_unw %>% 
  as_tibble() %>% 
  rename(var = term, 
         coef = estimate, 
         stderr = std.error, 
         ci_lower = conf.low, 
         ci_upper = conf.high) %>% 
  mutate(tstat = coef / stderr,
         pval = 2 * (1 - pnorm(abs(tstat))),
         .after = stderr)

# July 2013 cohort ATT
# Keep never-treated + July 2013 cohort only
pnadc_agg_cohort7 = pnadc_panel_aggregate %>% filter(cohort %in% c(7, NA) | is.na(cohort))
bjs_cohort7_att_unw = did_imputation(
  data = pnadc_agg_cohort7,
  yname = "informal",
  gname = "cohort",
  tname = "t",
  idname = "v4013",
  wname = "n",
  cluster_var = "v4013"
)

bjs_cohort7_att_unw = bjs_cohort7_att_unw %>% 
  as_tibble() %>% 
  rename(var = term, 
         coef = estimate, 
         stderr = std.error, 
         ci_lower = conf.low, 
         ci_upper = conf.high) %>% 
  mutate(tstat = coef / stderr,
         pval = 2 * (1 - pnorm(abs(tstat))),
         .after = stderr)

# Saving
saveRDS(bjs_cohort3_att_unw, "Results/bjs_cohort3_att_unw.RDS")
saveRDS(bjs_cohort7_att_unw, "Results/bjs_cohort7_att_unw.RDS")

#------------------------------------------------------------------------------#
# 4. Cohort-specific event-study
#------------------------------------------------------------------------------#

# Sept 2012 cohort ATT
# Keep never-treated + Sept 2012 cohort only
bjs_cohort3_es_unw = did_imputation(
  data = pnadc_agg_cohort3,
  yname = "informal",
  gname = "cohort",
  tname = "t",
  idname = "v4013",
  wname = "n",
  cluster_var = "v4013",
  horizon = TRUE,
  pretrends = TRUE
)

bjs_cohort3_es_unw = bjs_cohort3_es_unw %>% 
  as_tibble() %>% 
  mutate(term = as.numeric(term),
         tstat = coef / stderr,
         pval = 2 * (1 - pnorm(abs(tstat))), .after = stderr) %>% 
  rename(coef = estimate, stderr = std.error, ci_lower = conf.low, ci_upper = conf.high)


bjs_cohort3_es_unw %>% 
  as_tibble() %>% 
  mutate(term = as.numeric(term)) %>% 
  ggplot(aes(x = term)) +
  theme_bw() +
  geom_hline(yintercept = 0, color = 'red', linetype = 'dashed') +
  geom_point(aes(y = estimate), size = 2.5) + 
  geom_point(aes(x = -1, y = 0), size = 2.5) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.4, linewidth = 1) +
  scale_x_continuous(breaks = -2:9) +
  theme(panel.grid.minor = element_blank()) +
  labs(y = "Estimate", x = "Period relative to treatment")

# July 2013 cohort ATT
# Keep never-treated + July 2013 cohort only
bjs_cohort7_es_unw = did_imputation(
  data = pnadc_agg_cohort7,
  yname = "informal",
  gname = "cohort",
  tname = "t",
  idname = "v4013",
  wname = "n",
  cluster_var = "v4013",
  horizon = TRUE,
  pretrends = TRUE
)

bjs_cohort7_es_unw = bjs_cohort7_es_unw %>% 
  as_tibble() %>% 
  mutate(term = as.numeric(term),
         tstat = coef / stderr,
         pval = 2 * (1 - pnorm(abs(tstat))), .after = stderr) %>% 
  rename(coef = estimate, stderr = std.error, ci_lower = conf.low, ci_upper = conf.high)


bjs_cohort7_es_unw %>% 
  as_tibble() %>% 
  mutate(term = as.numeric(term)) %>% 
  ggplot(aes(x = term)) +
  theme_bw() +
  geom_hline(yintercept = 0, color = 'red', linetype = 'dashed') +
  geom_point(aes(y = estimate), size = 2.5) + 
  geom_point(aes(x = -1, y = 0), size = 2.5) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.4, linewidth = 1) +
  scale_x_continuous(breaks = -6:5) +
  theme(panel.grid.minor = element_blank()) +
  labs(y = "Estimate", x = "Period relative to treatment")

# Saving
saveRDS(bjs_cohort3_es_unw, "Results/bjs_cohort3_es_unw.RDS")
saveRDS(bjs_cohort7_es_unw, "Results/bjs_cohort7_es_unw.RDS")
