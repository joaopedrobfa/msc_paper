#------------------------------------------------------------------------------#
# 
# PLOT
# OVERALL EVENT STUDY
# 
#------------------------------------------------------------------------------#
# Estimators used:
#   BJS  - Borusyak, Jaravel, and Spiess (2021)
#------------------------------------------------------------------------------#
# 1. Importing the results
#------------------------------------------------------------------------------#

bjs_overall_es_w = read_dta('Results/bjs_overall_es_w.dta')

#------------------------------------------------------------------------------#
# 2. Period variable function
#------------------------------------------------------------------------------#

# Importing the aux function
source('Scripts/0.auxfunction_es_t_var.R')

#------------------------------------------------------------------------------#
# 3. Plotting
#------------------------------------------------------------------------------#

bjs_es_main_plot = es_t_var(bjs_overall_es_w) %>%
  ggplot(aes(x = term, y = coef)) +
  theme_classic() +
  geom_hline(yintercept = 0, color = "#808080", linetype = "solid", linewidth = 0.7) +
  geom_vline(xintercept = -0.5, color = "#808080", linetype = "dashed", linewidth = 0.7) +
  geom_point(size = 2.5, position = position_dodge(width = 0.6),
             color = "#1a476f", shape = 16) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper),
                width = 0.3, linewidth = 0.7, position = position_dodge(width = 0.6),
                color = "#1a476f") +
  scale_x_continuous(breaks = -6:9,
                     expand = expansion(mult = c(0.01, 0.01))) +
  geom_point(aes(x = -1, y = 0), color = '#c10534', size = 2.5, inherit.aes = FALSE) +
  # geom_errorbar(aes(x = -1, ymin = 0, ymax = 0),
  #               width = 0.3, linewidth = 0.7, color = '#c10534',
  #               inherit.aes = FALSE) +
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
  plot = bjs_es_main_plot,
  filename = 'Graphs/main_es.png',
  width = 900, height = 375, units = 'px', dpi = 108
)


