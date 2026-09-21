#------------------------------------------------------------------------------#
# 
# SECTOR-LEVEL ESTIMATION - BJS IMPUTATION ESTIMATOR
# WEIGHTED USING PNADc SAMPLING WEIGHTS
# Borusyak, Jaravel, and Spiess (2021)
# 
#------------------------------------------------------------------------------#
# pnadc_panel_aggregate = readRDS('Data/pnadc_panel_aggregate.rds')
#------------------------------------------------------------------------------#
# For some reason, the estimations proposed in this step -- which corrects the way
# I was weighting before (now we use the PNADc sampling weights) -- do not work
# with the R version of the did_imputation function.
# In order to overcome this issue and get the correctly weighted estimates, I will
# use this script to adjust the variables in the main aggregated dataset, create 
# the datasets needed for each estimation, then go to Stata to estimate the BJS
# models, and return to this script with the results to generate the figures.
#------------------------------------------------------------------------------#
# 1. Adjusting variables and creating the estimation-specific datasets
#------------------------------------------------------------------------------#

# Full dataset (controls and both cohorts)
dta_pnadc_agg_full = pnadc_panel_aggregate %>% 
  mutate(v4013 = as.numeric(v4013)) %>% 
  mutate(cohort = ifelse(is.na(cohort), 999, cohort)) # for Stata, replace NA with 999

write_dta(dta_pnadc_agg_full, 'Data/dta_pnadc_agg_full.dta')

# Controls and cohort 3
dta_pnadc_agg_cohort3 = dta_pnadc_agg_full %>% 
  filter(cohort %in% c(3, 999))

write_dta(dta_pnadc_agg_cohort3, 'Data/dta_pnadc_agg_cohort3.dta')

# Controls and cohort 7
dta_pnadc_agg_cohort7 = dta_pnadc_agg_full %>% 
  filter(cohort %in% c(7, 999))

write_dta(dta_pnadc_agg_cohort7, 'Data/dta_pnadc_agg_cohort7.dta')

#------------------------------------------------------------------------------#
# 2. BJS estimation with survey weights
#------------------------------------------------------------------------------#

# This step is done in the following do file: 
#   3.bjs_estimation_weighted.do

