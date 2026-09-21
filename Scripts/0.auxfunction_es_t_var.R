#------------------------------------------------------------------------------#
# 
# FUNCTION - CREATES THE PERIOD VARIABLE FOR THE STATA BJS ESTIMATION RESULTS
# 
#------------------------------------------------------------------------------#

es_t_var = function(df, label = NULL) {
  suppressWarnings(suppressMessages({
    n_pre = df %>% 
      filter(startsWith(var, "pre")) %>% 
      nrow()
    
    result = df %>%
      filter(!(var == paste0("pre", n_pre))) %>%
      mutate(
        term = case_when(
          startsWith(var, "tau") ~ as.numeric(str_remove(var, "tau")),
          startsWith(var, "pre") ~ -(n_pre + 1 - as.numeric(str_remove(var, "pre")))
        )
      )
    
    if (!is.null(label)) {
      result = result %>% mutate(series = label)
    }
    
    result
  }))
}
