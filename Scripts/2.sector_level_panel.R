#------------------------------------------------------------------------------#
# 
# SECTOR-LEVEL AGGREGATED PANEL
# 
#------------------------------------------------------------------------------#
# pnadc_panel = readRDS('Data/pnadc_panel.rds')
#------------------------------------------------------------------------------#
# 1. Aggregating to the sector-quarter level
#------------------------------------------------------------------------------#

pnadc_panel_aggregate = pnadc_panel %>% 
  mutate(formal_w = formal*v1028,
         informal_w = informal*v1028,
         selfemp_w = selfemp*v1028,
         employee_w = employee*v1028) %>% 
  group_by(ano, trimestre, t, v4013) %>% 
  summarise(n = n(),
            formal = sum(formal, na.rm = T) / sum(employee, na.rm = T),
            informal = sum(informal, na.rm = T) / sum(employee, na.rm = T),
            selfemp = mean(selfemp, na.rm = T),
            n_weighted = sum(v1028, na.rm = T),
            formal_w = sum(formal_w, na.rm = T) / sum(employee_w, na.rm = T),
            informal_w = sum(informal_w, na.rm = T) / sum(employee_w, na.rm = T),
            selfemp_w = sum(selfemp_w, na.rm = T) / sum(v1028, na.rm = T),
            .groups = "drop")

#------------------------------------------------------------------------------#
# 2. Assigning cohort according to sector treatment timing
#------------------------------------------------------------------------------#
# Some sectors were treated on September 2012 and some on July 2013.

cohort_sep2012 <- c(
  "55000", 
  "50000", 
  "51000", 
  "49040"
)
cohort_jul2013 <- c(
  "41000", 
  "42000", 
  "43000", 
  # "43999", # not included here bc it is not in the PNADc dataset 
  "49030"
)

pnadc_panel_aggregate = pnadc_panel_aggregate %>% 
  mutate(cohort = case_when(v4013 %in% cohort_sep2012 ~ 3,
                            v4013 %in% cohort_jul2013 ~ 7,
                            TRUE ~ NA))

saveRDS(pnadc_panel_aggregate, 'Data/pnadc_panel_aggregate.rds')
