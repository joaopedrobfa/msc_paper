#------------------------------------------------------------------------------#
# 
# PLOT - EXTRAS:
#   1. Staggered rollout - treated and control workers per period
#   2. 
# 
#------------------------------------------------------------------------------#
# 1. Staggered rollout - treated and control workers per period
#------------------------------------------------------------------------------#

staggered_rollout_df = pnadc_panel_aggregate %>% 
  select(t, cohort, n, n_weighted) %>% 
  mutate(treated = 1*(t >= cohort & !is.na(cohort)),
         control = abs(treated-1),
         n_treated = n*treated,
         n_control = n*control,
         n_w_treated = n_weighted*treated,
         n_w_control = n_weighted*control) %>% 
  group_by(t) %>% 
  summarise(n_treated = sum(n_treated),
            n_control = sum(n_control),
            n_w_treated = sum(n_w_treated),
            n_w_control = sum(n_w_control)) %>% 
  pivot_longer(cols = c(n_treated, n_control, n_w_treated, n_w_control),
               names_to = "type",
               values_to = "n") %>% 
  filter(type %in% c("n_w_treated", "n_w_control")) %>% 
  mutate(type = case_when(
    type == "n_w_treated" ~ "Treated",
    type == "n_w_control" ~ "Control"
  ))

staggered_rollout_plot = staggered_rollout_df %>%
  ggplot() +
  theme_classic() + 
  geom_area(aes(x = t, y = n, fill = type)) +
  scale_x_continuous(breaks = 1:12,
                     labels = c("Q1\n2012", "Q2", "Q3", "Q4",
                                "Q1\n2013", "Q2", "Q3", "Q4",
                                "Q1\n2014", "Q2", "Q3", "Q4"),
                     expand = expansion(mult = c(0.01, 0.01))) +
  scale_fill_manual(values = c("Control" = "grey50",
                               "Treated" = "navy")) +
  scale_y_continuous(breaks = c(0, 2e+07, 4e+07, 6e+07), 
                     labels = c("0", "50000", "100000", "150000"),
                     expand = expansion(mult = c(0.02, 0.02))) +
  theme(panel.grid = element_blank(),
        legend.title = element_blank(),
        legend.position = "bottom") + 
  labs(x = NULL, y = "Workers")


# Saving
ggsave(
  plot = staggered_rollout_plot,
  filename = "Graphs/staggered_rollout.png",
  width = 480, height = 400, units = "px", dpi = 108
)

#------------------------------------------------------------------------------#
# 2. Descriptive statistics table
#------------------------------------------------------------------------------#

# pnadc_panel = readRDS('Data/pnadc_panel.rds')

summary_stats_1 = pnadc_panel %>% 
  mutate(cohort = case_when(
    cohort == 3 ~ "Cohort 3",
    cohort == 7 ~ "Cohort 7",
    is.na(cohort) ~ "Never Treated"
  ), informal_w = informal*v1028) %>% 
  group_by(cohort) %>% 
  summarise(n_sectors = n_distinct(v4013),
            n_sectors_t = n_distinct(v4013, t),
            mean_inf_rate = sum(informal_w) / sum(v1028),
            n_individual_obs = n())

summary_stats_2 = dta_pnadc_agg_full %>% 
  mutate(cohort = case_when(
    cohort == 3 ~ "Cohort 3",
    cohort == 7 ~ "Cohort 7",
    cohort == 999 ~ "Never Treated"
  )) %>% 
  group_by(cohort) %>% 
  summarise(mean_n_weighted = mean(n_weighted))

summary_stats_3 = pnadc_panel %>%
  mutate(cohort = case_when(
    cohort == 3 ~ "Cohort 3",
    cohort == 7 ~ "Cohort 7",
    is.na(cohort) ~ "Never Treated"
  ), informal_w = informal*v1028) %>%
  filter(
      (cohort == "Cohort 3" & t < 3) |
      (cohort == "Cohort 7" & t < 7) |
      (cohort == "Never Treated")
  ) %>%
  group_by(cohort) %>%
  summarise(
    mean_pre_treat_inf_rate = sum(informal_w) / sum(v1028)
  )


summary_stats_1 %>% 
  left_join(summary_stats_2, by = "cohort") %>% 
  left_join(summary_stats_3, by = "cohort") %>% 
  mutate(cohort = factor(cohort, levels = c("Never Treated", "Cohort 3", "Cohort 7"))) %>%
  select(cohort, n_sectors, n_sectors_t, mean_inf_rate, 
         mean_pre_treat_inf_rate, mean_n_weighted, n_individual_obs) %>%
  pivot_longer(-cohort, names_to = "Variable", values_to = "value") %>%
  pivot_wider(names_from = cohort, values_from = value) %>%
  mutate(Variable = case_when(
    Variable == "n_sectors" ~ "Number of sectors",
    Variable == "n_sectors_t" ~ "Sector-period observations",
    Variable == "mean_inf_rate" ~ "Mean informality rate",
    Variable == "mean_pre_treat_inf_rate" ~ "Pre-treatment informality rate",
    Variable == "mean_n_weighted" ~ "Mean population weight",
    Variable == "n_individual_obs" ~ "Number of workers"
  )) %>% 
  relocate(`Never Treated`, .after = Variable) %>%
  kable(format = "latex", booktabs = TRUE,
        caption = "Descriptive Statistics by Treatment Group",
        digits = 3,
        label = "desc_stats",
        col.names = c("", "Never Treated", "Cohort 3", "Cohort 7")) %>%
  kable_styling(latex_options = "hold_position") %>%
  footnote(general = "Mean informality rate is the population-weighted share of private sector employees without a formal labour contract. Pre-treatment informality uses only periods before treatment onset for each cohort. Mean population weight is the average sum of individual PNADc sampling weights per sector-period cell.",
           threeparttable = TRUE)

#------------------------------------------------------------------------------#
# 3. ATT table
#------------------------------------------------------------------------------#

# GENERATE CODE !!!!!

#------------------------------------------------------------------------------#
# 4. Event study table
#------------------------------------------------------------------------------#

# Overall
df_overall_es = bind_rows(
  es_t_var(bjs_overall_es_w, label = "BJS") %>% arrange(term),
  bjs_overall_es_unw %>% mutate(series = "BJS (unw.)"),
  cs_overall_es %>% mutate(series = "CS"),
  dcdh_overall_es %>% mutate(series = "dCdH"),
  twfe_overall_es %>% mutate(series = "TWFE")
)

latex_overall_es = df_overall_es %>% 
  mutate(stars = case_when(pval <= 0.01 ~ "^{***}",
                           pval <= 0.05 ~ "^{**}",
                           pval <= 0.10 ~ "^{*}",
                           TRUE ~ ""),
         coef_fmt = paste0("$", formatC(coef, digits = 3, format = "f"), stars, "$"),
         se_fmt = paste0("$(", formatC(stderr, digits = 3, format = "f"), ")$")) %>% 
  select(series, term, coef_fmt, se_fmt) %>% 
  pivot_longer(cols = c(coef_fmt, se_fmt), names_to = "stat", values_to = "value") %>% 
  pivot_wider(names_from = series, values_from = value) %>% 
  arrange(term, stat) %>% 
  mutate(term = ifelse(stat == "coef_fmt", as.character(term), ""),
         term = case_when(
           as.numeric(term) == 0 ~ "\\phantom{+}$0$ (treatment)",
           as.numeric(term) == 1 ~ paste0("$+", term, "$ quarter"),
           as.numeric(term) > 1 ~ paste0("$+", term, "$ quarters"),
           as.numeric(term) < 0 ~ paste0("$", term, "$ quarters")
         ),
         across(everything(), ~replace_na(.x, ""))) %>% 
  select(-stat)

latex_overall_es %>% 
  kbl(
    format = "latex",
    booktabs = T,
    escape = F,
    col.names = c("Time to treatment", "BJS", "BJS (unw.)", "CS", "dCdH", "TWFE"),
    align = c("l", "c", "c", "c", "c", "c"),
    caption = "Event study estimates -- overall sample",
    label = "overall_es_all"
  ) %>% 
  kable_styling(latex_options = c("hold_position"))

# Cohort 3
df_cohort3_es = bind_rows(
  es_t_var(bjs_cohort3_es_w, label = "BJS") %>% arrange(term),
  bjs_cohort3_es_unw %>% mutate(series = "BJS (unw.)"),
  cs_cohort3_es %>% mutate(series = "CS"),
  dcdh_cohort3_es %>% mutate(series = "dCdH"),
  twfe_cohort3_es %>% mutate(series = "TWFE")
)

latex_cohort3_es = df_cohort3_es %>% 
  mutate(stars = case_when(pval <= 0.01 ~ "^{***}",
                           pval <= 0.05 ~ "^{**}",
                           pval <= 0.10 ~ "^{*}",
                           TRUE ~ ""),
         coef_fmt = paste0("$", formatC(coef, digits = 3, format = "f"), stars, "$"),
         se_fmt = paste0("$(", formatC(stderr, digits = 3, format = "f"), ")$")) %>% 
  select(series, term, coef_fmt, se_fmt) %>% 
  pivot_longer(cols = c(coef_fmt, se_fmt), names_to = "stat", values_to = "value") %>% 
  pivot_wider(names_from = series, values_from = value) %>% 
  arrange(term, stat) %>% 
  mutate(term = ifelse(stat == "coef_fmt", as.character(term), ""),
         term = case_when(
           as.numeric(term) == 0 ~ "\\phantom{+}$0$ (treatment)",
           as.numeric(term) == 1 ~ paste0("$+", term, "$ quarter"),
           as.numeric(term) > 1 ~ paste0("$+", term, "$ quarters"),
           as.numeric(term) < 0 ~ paste0("$", term, "$ quarters")
         ),
         across(everything(), ~replace_na(.x, ""))) %>% 
  select(-stat)

latex_cohort3_es %>% 
  kbl(
    format = "latex",
    booktabs = T,
    escape = F,
    col.names = c("Time to treatment", "BJS", "BJS (unw.)", "CS", "dCdH", "TWFE"),
    align = c("l", "c", "c", "c", "c", "c"),
    caption = "Event study estimates -- cohort 3",
    label = "overall_es_coh3"
  ) %>% 
  kable_styling(latex_options = c("hold_position"))

# Cohort 7
df_cohort7_es = bind_rows(
  es_t_var(bjs_cohort7_es_w, label = "BJS") %>% arrange(term),
  bjs_cohort7_es_unw %>% mutate(series = "BJS (unw.)"),
  cs_cohort7_es %>% mutate(series = "CS"),
  dcdh_cohort7_es %>% mutate(series = "dCdH"),
  twfe_cohort7_es %>% mutate(series = "TWFE")
)

latex_cohort7_es = df_cohort7_es %>% 
  mutate(stars = case_when(pval <= 0.01 ~ "^{***}",
                           pval <= 0.05 ~ "^{**}",
                           pval <= 0.10 ~ "^{*}",
                           TRUE ~ ""),
         coef_fmt = paste0("$", formatC(coef, digits = 3, format = "f"), stars, "$"),
         se_fmt = paste0("$(", formatC(stderr, digits = 3, format = "f"), ")$")) %>% 
  select(series, term, coef_fmt, se_fmt) %>% 
  pivot_longer(cols = c(coef_fmt, se_fmt), names_to = "stat", values_to = "value") %>% 
  pivot_wider(names_from = series, values_from = value) %>% 
  arrange(term, stat) %>% 
  mutate(term = ifelse(stat == "coef_fmt", as.character(term), ""),
         term = case_when(
           as.numeric(term) == 0 ~ "\\phantom{+}$0$ (treatment)",
           as.numeric(term) == 1 ~ paste0("$+", term, "$ quarter"),
           as.numeric(term) > 1 ~ paste0("$+", term, "$ quarters"),
           as.numeric(term) < 0 ~ paste0("$", term, "$ quarters")
         ),
         across(everything(), ~replace_na(.x, ""))) %>% 
  select(-stat)

latex_cohort7_es %>% 
  kbl(
    format = "latex",
    booktabs = T,
    escape = F,
    col.names = c("Time to treatment", "BJS", "BJS (unw.)", "CS", "dCdH", "TWFE"),
    align = c("l", "c", "c", "c", "c", "c"),
    caption = "Event study estimates -- cohort 7",
    label = "overall_es_coh7"
  ) %>% 
  kable_styling(latex_options = c("hold_position"))

#------------------------------------------------------------------------------#
# 5. Cohort-specific combined ES plot -- CALENDAR TIME
#------------------------------------------------------------------------------#

calendar_es = bind_rows(
  es_t_var(bjs_cohort3_es_w, label = "Cohort 3") %>% mutate(term = term + 3),
  es_t_var(bjs_cohort7_es_w, label = "Cohort 7") %>% mutate(term = term + 7),
  tibble(coef = 0, term = 2, series = 'Cohort 3'),
  tibble(coef = 0, term = 6, series = 'Cohort 7')
) %>% 
  mutate(series = factor(series, levels = c("Cohort 3", "Cohort 7"))) %>% 
  arrange(series, term)

calendar_es_plot = calendar_es %>%
  ggplot(aes(x = term, y = coef, color = series, shape = series)) +
  theme_classic() +
  geom_hline(yintercept = 0, color = "#808080", linetype = "solid", linewidth = 0.7) +
  geom_vline(xintercept = 2.5, color = "#55752f", linetype = "dashed", linewidth = 0.7) +
  geom_vline(xintercept = 6.5, color = "#e37e00", linetype = "dashed", linewidth = 0.7) +
  geom_point(size = 2.5, position = position_dodge(width = 0.6)) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper),
                width = 0.3, linewidth = 0.7, position = position_dodge(width = 0.6)) +
  scale_x_continuous(breaks = 1:12,
                     expand = expansion(mult = c(0.01, 0.01)),
                     labels = c("Q1\n2012", "Q2", "Q3", "Q4",
                                "Q1\n2013", "Q2", "Q3", "Q4",
                                "Q1\n2014", "Q2", "Q3", "Q4")) +
  # scale_y_continuous(limits = c(-0.06, 0.06)) +
  scale_color_manual(values = c(
    "Cohort 3" = "#55752f",
    "Cohort 7" = "#e37e00")) +
  scale_shape_manual(values = c(
    "Cohort 3" = 17,
    "Cohort 7" = 15)) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.title = element_blank(),
        legend.position = 'bottom',
        legend.text = element_text(size = 14),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 12),
        panel.grid.major.y = element_line(colour = alpha("grey50", 0.25))) +
  labs(x = "Period", y = "Estimate") -> teste_plot

# Saving
ggsave(
  plot = calendar_es_plot,
  filename = 'Graphs/calendar_es.png',
  width = 900, height = 375, units = 'px', dpi = 108
)


