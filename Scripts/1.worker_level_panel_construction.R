#------------------------------------------------------------------------------#
# 
# WORKER-LEVEL PANEL CONSTRUCTION
# 
#------------------------------------------------------------------------------#
# 1. Importing the data and constructing the base panel
#------------------------------------------------------------------------------#

# Key variables
# 1. Labour market core outcome variables
#   VD4001: condition in the labour force (1=in, 2=out)
#   VD4002: condition of employment (1=employed, 2=unemployed)
#   VD4009: type of employment ***** INFORMALITY FROM HERE
# 2. Treatment assignment
#   V4013:  CNAE (ISIC)
# 3. Survey design
#   V1028:  survey weight
#   UPA
#   V1023
#   posest
# 4. Individual identifiers
#   V1008:  household number
#   V1014:  panel group
#   V2003:  person number within the household
# 5. Individual controls
#   V2007:  sex
#   V2009:  age
#   VD3004: maximum level of education
#   VD3005: years of education
# 6. Geography
#   UF:     state

pnadc_list <- list()

for (yr in 2012:2014) {
  for (qt in 1:4) {
    data = get_pnadc(
      year = yr, quarter = qt,
      vars = c("VD4001", "VD4002", "VD4009",
               "V4013",
               "V1028", "UPA", "V1023", "posest",
               "V1008", "V1014", "V2003",
               "V2007", "V2009", "VD3004", "VD3005",
               "UF", "Ano", "Trimestre"),
      labels = TRUE, design = TRUE, reload = FALSE,
      savedir = "Data/pnadc_data/"
    )
    
    data_df = data$variables %>% 
      as_tibble() %>% 
      select(Ano, Trimestre, UF,
             VD4001, VD4002, VD4009,
             V4013,
             V2007, V2009, VD3004, VD3005,
             V1028, UPA, V1023, posest,
             V1008, V1014, V2003) %>% 
      filter(!is.na(VD4001) & !is.na(VD4009))
    
    rm(data)
    gc()
    
    pnadc_list[[paste(yr, qt, sep = '_')]] = data_df
    cat('Done:', yr, 'Q', qt, '\n')
  }
}
pnadc = bind_rows(pnadc_list) %>% 
  mutate(Ano = as.numeric(Ano),
         Trimestre = as.numeric(Trimestre),
         t = (Ano - 2012)*4 + Trimestre, .after = Trimestre) %>% # period variable
  rename_all(tolower)

rm(pnadc_list)

# Doing some checks on the structure of the data
table(pnadc$ano, pnadc_panel$trimestre) # obs per period
length(unique(pnadc$v4013)) # number of CNAE sector
table(pnadc$t, pnadc$vd4009, useNA = 'always') # type of employment per period

#------------------------------------------------------------------------------#
# 2. Filtering for private sector only and creating type of employment dummies
#------------------------------------------------------------------------------#

pnadc_private = pnadc %>% 
  select(-c(vd4001, vd4002)) %>% 
  filter(vd4009 %in% c(
    "Empregado no setor privado com carteira de trabalho assinada",
    "Empregado no setor privado sem carteira de trabalho assinada",
    # "Conta-própria"
  )) %>% 
  mutate(formal = 1*(vd4009 == "Empregado no setor privado com carteira de trabalho assinada"),
         informal = 1*(vd4009 == "Empregado no setor privado sem carteira de trabalho assinada"),
         selfemp = 1*(vd4009 == "Conta-própria"),
         employee = 1*(formal==1 | informal==1),
         .after = vd4009)

p_private = nrow(pnadc_private) / nrow(pnadc)
p_private # Private sector workers account for 72% of all PNADc observations

# The formal, informal and employee variables will be used to get the informality
# rates for each sector when we aggregate the data later.

#------------------------------------------------------------------------------#
# 3. Dropping always treated sectors
#------------------------------------------------------------------------------#
# As the first treatment period was December 2011 and the PNADc data started being
# collected on 2012, we don't have data on pre-treatment observations for the sectors
# treated in this first wave.
# Therefore, we have to drop the sectors treated in the first wave: 62000 and 82002

cohort_dec2011 <- c("62000", "82002")

pnadc_panel = pnadc_private %>% 
  filter(!v4013 %in% cohort_dec2011)

p_dec2011 = 1 - (nrow(pnadc_panel) / nrow(pnadc_private))
p_dec2011 # Cohort Dec 2011 workers account for less than 1% of all private sector workers

#------------------------------------------------------------------------------#
# 4. Assigning cohort of treatment (sector X period)
#------------------------------------------------------------------------------#

cohort_sep2012 <- c("55000", "50000", "51000", "49040")
cohort_jul2013 <- c("41000", "42000", "43000", "43999", "49030")

pnadc_panel = pnadc_panel %>% 
  mutate(cohort = case_when(v4013 %in% cohort_sep2012 ~ 3,
                            v4013 %in% cohort_jul2013 ~ 7,
                            TRUE ~ NA))

#------------------------------------------------------------------------------#
# 5. Saving the worker-level panel
#------------------------------------------------------------------------------#

saveRDS(pnadc_panel, 'Data/pnadc_panel.rds')
