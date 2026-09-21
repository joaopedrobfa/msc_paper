#------------------------------------------------------------------------------#
# 
# PLOT
# ROBUSTNESS ACROSS DIFFERENT ESTIMATORS
# 
#------------------------------------------------------------------------------#
# Estimators used:
#   BJS  - Borusyak, Jaravel, and Spiess (2021)
#   CS   - Callaway and Sant'Anna (2021)
#   dCdH - de Chaisemartin and D'Haultfoeuille (2024)
#   TWFE - Two-way fixed effects
#------------------------------------------------------------------------------#
# 1. Importing the results
#------------------------------------------------------------------------------#

bjs_overall_es_w = read_dta('Results/bjs_overall_es_w.dta')
cs_overall_es = readRDS('Results/cs_overall_es.RDS')
dcdh_overall_es = readRDS('Results/dcdh_overall_es.RDS')
twfe_overall_es = readRDS('Results/twfe_overall_es.RDS')

#------------------------------------------------------------------------------#
# 2. Adding labels
#------------------------------------------------------------------------------#

rob_est_plot_df = bind_rows(
  es_t_var(bjs_overall_es_w, label = "Borusyak et al.") %>% select(-N),
  cs_overall_es %>% mutate(series = "Callaway-Sant'Anna"),
  dcdh_overall_es %>% mutate(series = "de Chaisemartin-D'Haultfoeuille"),
  twfe_overall_es %>% mutate(series = "TWFE")
)

#------------------------------------------------------------------------------#
# 3. Plotting
#------------------------------------------------------------------------------#

rob_est_plot = rob_est_plot_df %>%
  mutate(series = factor(series, 
                         levels = c("Borusyak et al.",
                                    "Callaway-Sant'Anna",
                                    "de Chaisemartin-D'Haultfoeuille",
                                    "TWFE"))) %>% 
  ggplot(aes(x = term, y = coef, color = series, shape = series)) +
  theme_classic() +
  geom_hline(yintercept = 0, color = "#808080", linetype = "solid", linewidth = 0.7) +
  geom_vline(xintercept = -0.5, color = "#808080", linetype = "dashed", linewidth = 0.7) +
  geom_point(size = 2.5, position = position_dodge(width = 0.7), stroke = 1.25) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper),
                width = 0.6, linewidth = 0.7, position = position_dodge(width = 0.7)) +
  scale_x_continuous(breaks = -6:9,
                     expand = expansion(mult = c(0.01, 0.01))) +
  geom_point(aes(x = -1, y = 0), color = "black", size = 3, 
             stroke = 0, shape = 19, inherit.aes = FALSE) +
  # geom_errorbar(aes(x = -1, ymin = 0, ymax = 0),
  #               width = 0.3, linewidth = 0.7, color = "grey10",
  #               inherit.aes = FALSE) +
  scale_color_manual(values = c(
    "Borusyak et al." = "#c10534",
    "Callaway-Sant'Anna" = "#55752f",
    "de Chaisemartin-D'Haultfoeuille" = "#1a476f",
    "TWFE" = "#e37e00"
  )) +
  scale_shape_manual(values = c(
    "Borusyak et al." = 19,
    "Callaway-Sant'Anna" = 2,
    "de Chaisemartin-D'Haultfoeuille" = 5,
    "TWFE" = 0
  )) +
  theme(panel.grid.minor = element_blank(),
        # panel.grid.major.x = element_blank(),
        legend.title = element_blank(),
        legend.position = 'bottom',
        legend.text = element_text(size = 13),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 12),
        panel.grid.major.y = element_line(colour = alpha("grey50", 0.25))) +
  labs(x = "Period relative to treatment", y = "Estimate")

# Saving
ggsave(
  plot = rob_est_plot,
  filename = 'Graphs/estimator_robustness.png',
  width = 900, height = 375, units = 'px', dpi = 108
)

