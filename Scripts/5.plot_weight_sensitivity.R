#------------------------------------------------------------------------------#
# 
# PLOT
# COMBINED EVENT STUDY - OVERALL BJS WEIGHTED vs OVERALL BJS UNWEIGHTED
# 
#------------------------------------------------------------------------------#
# Estimators used:
#   BJS  - Borusyak, Jaravel, and Spiess (2021)
#------------------------------------------------------------------------------#
# 1. Importing the results
#------------------------------------------------------------------------------#

bjs_overall_es_w = read_dta('Results/bjs_overall_es_w.dta')
bjs_overall_es_unw = readRDS('Results/bjs_overall_es_unw.RDS')

#------------------------------------------------------------------------------#
# 2. Creating the period variable and adding labels
#------------------------------------------------------------------------------#

# Importing the aux function
source('Scripts/0.auxfunction_es_t_var.R')

# Combining the event studies dataframes
bjs_w_unw_combined_df = bind_rows(
  es_t_var(bjs_overall_es_w, label = "Weighted"),
  bjs_overall_es_unw %>% mutate(series = "Unweighted")
) %>% 
  mutate(series = factor(series, levels = c("Weighted", "Unweighted"))) %>% 
  arrange(series, term)

bjs_w_unw_combined_df

#------------------------------------------------------------------------------#
# 3. Plotting
#------------------------------------------------------------------------------#

bjs_w_unw_combined_plot = bjs_w_unw_combined_df %>%
  ggplot(aes(x = term, y = coef, color = series, shape = series)) +
  theme_classic() +
  geom_hline(yintercept = 0, color = "#808080", linetype = "solid", linewidth = 0.7) +
  geom_vline(xintercept = -0.5, color = "#808080", linetype = "dashed", linewidth = 0.7) +
  geom_point(size = 2.5, position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper),
                width = 0.3, linewidth = 0.7, position = position_dodge(width = 0.5)) +
  scale_x_continuous(breaks = -6:9,
                     expand = expansion(mult = c(0.01, 0.01))) +
  geom_point(aes(x = -1, y = 0), color = '#c10534', size = 2.5, inherit.aes = FALSE) +
  # geom_errorbar(aes(x = -1, ymin = 0, ymax = 0),
  #               width = 0.3, linewidth = 0.7, color = '#c10534',
  #               inherit.aes = FALSE) +
  scale_color_manual(values = c("Weighted" = "#1a476f",
                                "Unweighted" = "#e37e00")) +
  scale_shape_manual(values = c("Weighted" = 16,
                                "Unweighted" = 17)) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.title = element_blank(),
        legend.position = 'bottom',
        legend.text = element_text(size = 14),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 12),
        panel.grid.major.y = element_line(colour = alpha("grey50", 0.25))) +
  labs(x = "Period relative to treatment", y = "Estimate")

# Saving
ggsave(
  plot = bjs_w_unw_combined_plot,
  filename = 'Graphs/combined_bjs_w_unw.png',
  width = 900, height = 375, units = 'px', dpi = 108
)
