source('setup_all.R')
source('setup_census.R')


# MENTAL HEALTH BAR CHART: BLACK/WHITE =====

# Calculate weighted Cohen's d values for Black and White Americans separately
# for depression and anxiety. Cohen's d here measures the effect size of
# Floyd's death by comparing the mean symptom severity prior to his death against 
# the mean symptom severity following his death. These values are combined into
# the p_vals_pulse variable. See `setup_census.R` for weighted Cohen's d calculation.
p_vals_pulse = tibble()
p_vals_pulse = bind_rows(p_vals_pulse,
                         pulse %>%
                           filter(is_black == 1) %>%
                           mutate(gad2_phq2_sum = gad2_sum + phq2_sum,
                                  g = 'Black') %>%
                           select(g, gad2_sum, phq2_sum, floyd_weekOrNot, PWEIGHT) %>%
                           gather('measure', 'value', gad2_sum, phq2_sum) %>%
                           group_by(g, measure) %>%
                           summarize(weighted_cohens_d(value, floyd_weekOrNot, PWEIGHT)) %>%
                           ungroup())
p_vals_pulse = bind_rows(p_vals_pulse,
                         pulse %>%
                           filter(is_white == 1 & is_hispanic == 0) %>%
                           mutate(gad2_phq2_sum = gad2_sum + phq2_sum,
                                  g = 'White') %>%
                           select(g, gad2_sum, phq2_sum, floyd_weekOrNot, PWEIGHT) %>%
                           gather('measure', 'value', gad2_sum, phq2_sum) %>%
                           group_by(g, measure) %>%
                           summarize(weighted_cohens_d(value, floyd_weekOrNot, PWEIGHT)) %>%
                           ungroup())

# Clean up the category names & calculate the variance of the Cohen's d.
p_vals_pulse = p_vals_pulse %>%
  mutate(g = factor(g, levels = c('White', 'Black')),
         measure = str_to_upper(str_remove(measure, '_sum')),
         v = (1 / n1) + (1 / n2) + (d^2 / (2 * (n1 + n2))))

# Calculate p-values by using the variance to calculate a z-score.
# These values are used to determine the significance levels in the next block.
p_vals_pulse_p = p_vals_pulse %>%
  select(g, measure, d, v) %>%
  group_by(measure) %>%
  summarize(z = diff(d) / sqrt(sum(v))) %>%
  ungroup() %>%
  mutate(p = round(2 * pnorm(-abs(z)), digits = 3))
p_vals_pulse_p

# Plot the mental health Cohen's d values along with significance levels.
p_vals_pulse %>%
  inner_join(p_vals_pulse_p) %>%
  mutate(measure = if_else(measure == 'GAD2', 'Anxiety', 'Depression')) %>%
  ggplot(aes(g, d, fill = measure)) +
  geom_bar(stat = 'identity', position = 'dodge',
           color = 'black', width = 0.7) +
  geom_text(aes(y = d + 0.007), label = '***',
            position = position_dodge(width = 0.7),
            size = 10, vjust = .7) +
  geom_signif(y_position = c(.120, .075),
              xmin = c(1.125, 0.855), xmax = c(2.125, 1.855),
              annotation = c('', ''),
              textsize = 8) +
  geom_text(x = (2.125 - 1.125) / 2 + 1.125,
            y = .115 + 0.013,
            label = '***', size = signif_size, vjust = .7) +
  geom_text(x = (1.855 - .855) / 2 + .855,
            y = .075 + 0.009,
            label = '*', size = signif_size, vjust = .7) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, .15),
                     breaks = seq(0, 1, by = 0.025)) +
  scale_fill_manual(values = c('#333333', 'white')) +
  theme_Publication() +
  theme(panel.grid.major = element_blank(),
        axis.line.y = element_line(size = 1.3),
        legend.position = c(.9, .2),
        legend.title = element_blank(),
        legend.background = element_rect(fill = 'transparent'),
        aspect.ratio = 1/3) +
  coord_flip() +
  labs(x = '', y = paste0('Effect size (Cohen\'s d)'),
       fill = '') +
  guides(fill = guide_legend(reverse = TRUE)) +
  ggsave(paste0('mental_health_significance.png'),
         width=12, height=4, units='in', dpi=300)