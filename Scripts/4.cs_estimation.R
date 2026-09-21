#------------------------------------------------------------------------------#
# 
# SECTOR-LEVEL ESTIMATION - CS ESTIMATOR
# WEIGHTED USING PNADc SAMPLING WEIGHTS
# Callaway and Sant'Anna (2021)
# 
#------------------------------------------------------------------------------#
# For reproducibility, we will use the exact same dataframes used for BJS weighted
# estimation in Stata.
# dta_pnadc_agg_full = read_dta('Data/dta_pnadc_agg_full.dta')
# dta_pnadc_agg_cohort3 = read_dta('Data/dta_pnadc_agg_cohort3.dta')
# dta_pnadc_agg_cohort7 = read_dta('Data/dta_pnadc_agg_cohort7.dta')
#------------------------------------------------------------------------------#
# 0. Adjusting the cohort variable (CS uses cohort=0 for never-treated)
#------------------------------------------------------------------------------#
dta_pnadc_agg_full_cs = dta_pnadc_agg_full %>% 
  mutate(cohort = ifelse(cohort == 999, 0, cohort))

dta_pnadc_agg_cohort3_cs = dta_pnadc_agg_cohort3 %>% 
  mutate(cohort = ifelse(cohort == 999, 0, cohort))

dta_pnadc_agg_cohort7_cs = dta_pnadc_agg_cohort7 %>% 
  mutate(cohort = ifelse(cohort == 999, 0, cohort))

#------------------------------------------------------------------------------#
# 1. Overall - ATT and event study
#------------------------------------------------------------------------------#

set.seed(123456)
cs_overall = att_gt(
  data = dta_pnadc_agg_full_cs,
  yname = "informal_w",
  idname = "v4013",
  gname = "cohort",
  tname = "t",
  clustervars = "v4013",
  weightsname = "n_weighted",
  control_group = "notyettreated",       # makes it comparable to BJS
  base_period = "universal",             # makes it comparable to BJS
  allow_unbalanced_panel = T,            # makes it comparable to BJS
  bstrap = T,
  biters = 5000
)

cs_overall_att = aggte(cs_overall, type = "simple")
cs_overall_att = tibble(
  var = NA,
  coef = cs_overall_att$overall.att,
  stderr = cs_overall_att$overall.se,
  tstat = coef / stderr,
  pval = 2 * (1 - pnorm(abs(tstat))),
  ci_lower = coef - 1.96*stderr,
  ci_upper = coef + 1.96*stderr
)

cs_overall_es = aggte(cs_overall, type = "dynamic")
cs_overall_es = tibble(
  var = NA,
  term = cs_overall_es$egt,
  coef = cs_overall_es$att.egt,
  stderr = cs_overall_es$se.egt,
  tstat = coef / stderr,
  pval = 2 * (1 - pnorm(abs(tstat))),
  ci_lower = coef - stderr*cs_overall_es$crit.val.egt,
  ci_upper = coef + stderr*cs_overall_es$crit.val.egt
) %>% filter(term != -1)

# Saving
saveRDS(cs_overall_att, "Results/cs_overall_att.RDS")
saveRDS(cs_overall_es, "Results/cs_overall_es.RDS")

#------------------------------------------------------------------------------#
# 2. Cohort 3 - ATT and event study
#------------------------------------------------------------------------------#

set.seed(123456)
cs_cohort3 = att_gt(
  data = dta_pnadc_agg_cohort3_cs,
  yname = "informal_w",
  idname = "v4013",
  gname = "cohort",
  tname = "t",
  clustervars = "v4013",
  weightsname = "n_weighted",
  control_group = "notyettreated",       # makes it comparable to BJS
  base_period = "universal",             # makes it comparable to BJS
  allow_unbalanced_panel = T,            # makes it comparable to BJS
  bstrap = T,
  biters = 5000
)

cs_cohort3_att = aggte(cs_cohort3, type = "simple")
cs_cohort3_att = tibble(
  var = NA,
  coef = cs_cohort3_att$overall.att,
  stderr = cs_cohort3_att$overall.se,
  tstat = coef / stderr,
  pval = 2 * (1 - pnorm(abs(tstat))),
  ci_lower = coef - 1.96*stderr,
  ci_upper = coef + 1.96*stderr
)

cs_cohort3_es = aggte(cs_cohort3, type = "dynamic")
cs_cohort3_es = tibble(
  var = NA,
  term = cs_cohort3_es$egt,
  coef = cs_cohort3_es$att.egt,
  stderr = cs_cohort3_es$se.egt,
  tstat = coef / stderr,
  pval = 2 * (1 - pnorm(abs(tstat))),
  ci_lower = coef - stderr*cs_cohort3_es$crit.val.egt,
  ci_upper = coef + stderr*cs_cohort3_es$crit.val.egt
) %>% filter(term != -1)

# Saving
saveRDS(cs_cohort3_att, "Results/cs_cohort3_att.RDS")
saveRDS(cs_cohort3_es, "Results/cs_cohort3_es.RDS")

#------------------------------------------------------------------------------#
# 3. Cohort 7 - ATT and event study
#------------------------------------------------------------------------------#

set.seed(123456)
cs_cohort7 = att_gt(
  data = dta_pnadc_agg_cohort7_cs,
  yname = "informal_w",
  idname = "v4013",
  gname = "cohort",
  tname = "t",
  clustervars = "v4013",
  weightsname = "n_weighted",
  control_group = "notyettreated",       # makes it comparable to BJS
  base_period = "universal",             # makes it comparable to BJS
  allow_unbalanced_panel = T,            # makes it comparable to BJS
  bstrap = T,
  biters = 5000
)

cs_cohort7_att = aggte(cs_cohort7, type = "simple")
cs_cohort7_att = tibble(
  var = NA,
  coef = cs_cohort7_att$overall.att,
  stderr = cs_cohort7_att$overall.se,
  tstat = coef / stderr,
  pval = 2 * (1 - pnorm(abs(tstat))),
  ci_lower = coef - 1.96*stderr,
  ci_upper = coef + 1.96*stderr
)

cs_cohort7_es = aggte(cs_cohort7, type = "dynamic")
cs_cohort7_es = tibble(
  var = NA,
  term = cs_cohort7_es$egt,
  coef = cs_cohort7_es$att.egt,
  stderr = cs_cohort7_es$se.egt,
  tstat = coef / stderr,
  pval = 2 * (1 - pnorm(abs(tstat))),
  ci_lower = coef - stderr*cs_cohort7_es$crit.val.egt,
  ci_upper = coef + stderr*cs_cohort7_es$crit.val.egt
) %>% filter(term != -1)

# Saving
saveRDS(cs_cohort7_att, "Results/cs_cohort7_att.RDS")
saveRDS(cs_cohort7_es, "Results/cs_cohort7_es.RDS")

