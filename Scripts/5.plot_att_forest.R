#------------------------------------------------------------------------------#
# 
# PLOT
# FOREST PLOT - OVERALL ATT WITH DIFFERENT ESTIMATORS
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

bjs_overall_att_w = read_dta('Results/bjs_overall_att_w.dta')
bjs_overall_att_unw = readRDS('Results/bjs_overall_att_unw.RDS')
cs_overall_att = readRDS('Results/cs_overall_att.RDS')
dcdh_overall_att = readRDS('Results/dcdh_overall_att.RDS')
twfe_overall_att = readRDS('Results/twfe_overall_att.RDS')

#------------------------------------------------------------------------------#
# 2. Adding labels
#------------------------------------------------------------------------------#

att_forest_df = bind_rows(
  bjs_overall_att_w %>% mutate(label = "Borusyak et al.\n(weighted)"),
  bjs_overall_att_unw %>% 
    rename(coef = estimate, ci_lower = conf.low, ci_upper = conf.high) %>% 
    mutate(label = "Borusyak et al.\n(unweighted)"),
  cs_overall_att %>% mutate(label = "Callaway\nSant'Anna"),
  dcdh_overall_att %>% mutate(label = "de Chaisemartin\nD'Haultfoeuille"),
  twfe_overall_att %>% mutate(label = "TWFE")
) %>% 
  select(coef, ci_lower, ci_upper, label)

#------------------------------------------------------------------------------#
# 3. Plotting
#------------------------------------------------------------------------------#

att_forest_plot = att_forest_df %>% 
  mutate(label = factor(label, levels = c(
    "Borusyak et al.\n(weighted)",
    "Borusyak et al.\n(unweighted)",
    "Callaway\nSant'Anna",
    "de Chaisemartin\nD'Haultfoeuille",
    "TWFE"
  ))) %>% 
  ggplot(aes(y = coef, x = label, color = label, shape = label)) +
  theme_light() +
  geom_hline(yintercept = 0, color = "#808080", linetype = "dashed", linewidth = 0.7) +
  geom_point(size = 3.5, stroke = 1.5) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper),
                width = 0.15, linewidth = 1) +
  scale_color_manual(values = c(
    "Borusyak et al.\n(weighted)" = "#c10534",
    "Borusyak et al.\n(unweighted)" = "#800080",
    "Callaway\nSant'Anna" = "#55752f",
    "de Chaisemartin\nD'Haultfoeuille" = "#1a476f",
    "TWFE" = "#e37e00"
  )) +
  scale_shape_manual(values = c(
    "Borusyak et al.\n(weighted)" = 21,
    "Borusyak et al.\n(unweighted)" = 4,
    "Callaway\nSant'Anna" = 24,
    "de Chaisemartin\nD'Haultfoeuille" = 23,
    "TWFE" = 22
  )) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.position = 'none',
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 12, colour = 'black'),
        panel.grid.major.y = element_line(colour = alpha("grey50", 0.25))) +
  labs(y = "Estimate", x = NULL)

# Saving
ggsave(
  plot = att_forest_plot,
  filename = 'Graphs/att_forest.png',
  width = 900, height = 375, units = 'px', dpi = 108
)

