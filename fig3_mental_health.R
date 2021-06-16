source('setup_all.R')
source('setup_census.R')


# MENTAL HEALTH BAR CHART: MINNESOTA/OTHER STATES =====

# Calculate weighted Cohen's d values for MN and all other states separately
# for depression and anxiety. Cohen's d here measures the effect size of
# Floyd's death by comparing the mean symptom severity prior to his death against 
# the mean symptom severity following his death. These values are combined into
# the p_vals_pulse_mn variable. See `setup_census.R` for weighted Cohen's d calculation.
p_vals_pulse_mn = tibble()
p_vals_pulse_mn = bind_rows(p_vals_pulse_mn,
                         pulse %>%
                           filter(state == 'Minnesota') %>%
                           mutate(gad2_phq2_sum = gad2_sum + phq2_sum,
                                  g = 'Minnesota') %>%
                           select(g, gad2_sum, phq2_sum, floyd_weekOrNot, PWEIGHT) %>%
                           gather('measure', 'value', gad2_sum, phq2_sum) %>%
                           group_by(g, measure) %>%
                           summarize(weighted_cohens_d(value, floyd_weekOrNot, PWEIGHT)) %>%
                           ungroup())
p_vals_pulse_mn = bind_rows(p_vals_pulse_mn,
                         pulse %>%
                           filter(state != 'Minnesota') %>%
                           mutate(gad2_phq2_sum = gad2_sum + phq2_sum,
                                  g = 'USA') %>%
                           select(g, gad2_sum, phq2_sum, floyd_weekOrNot, PWEIGHT) %>%
                           gather('measure', 'value', gad2_sum, phq2_sum) %>%
                           group_by(g, measure) %>%
                           summarize(weighted_cohens_d(value, floyd_weekOrNot, PWEIGHT)) %>%
                           ungroup())

# Calculate the variance of the Cohen's d, then calculate p-values by using the variance to calculate a z-score.
# These values are used to determine the significance levels in the next block.
p_vals_pulse_mn_p = p_vals_pulse_mn %>%
  mutate(v = (1/n1) + (1/n2) + (d^2/(2*(n1+n2)))) %>%
  select(g, measure, d, v) %>%
  group_by(measure) %>%
  summarize(z = diff(d) / sqrt(sum(v))) %>%
  ungroup() %>%
  mutate(p = round(2*pnorm(-abs(z)), digits = 3))
p_vals_pulse_mn_p

# Run a few weighted t-tests using the weights package. Tests compare pre- and post-Floyd symptom severity.

# Minnesota PHQ2
b1 = pulse %>%
  filter(state == 'Minnesota') %>%
  mutate(gad2_phq2_sum = gad2_sum + phq2_sum) %>%
  select(phq2_sum, PWEIGHT, floyd_weekOrNot) %>%
  filter(floyd_weekOrNot == 0)
b2 = pulse %>%
  filter(state == 'Minnesota') %>%
  mutate(gad2_phq2_sum = gad2_sum + phq2_sum) %>%
  select(phq2_sum, PWEIGHT, floyd_weekOrNot) %>%
  filter(floyd_weekOrNot == 1)
ttest_mn_phq = weights::wtd.t.test(b1$phq2_sum, b2$phq2_sum, b1$PWEIGHT, b2$PWEIGHT)
ttest_mn_phq

# Minnesota GAD
b1 = pulse %>%
  filter(state == 'Minnesota') %>%
  mutate(gad2_phq2_sum = gad2_sum + phq2_sum) %>%
  select(gad2_sum, PWEIGHT, floyd_weekOrNot) %>%
  filter(floyd_weekOrNot == 0)
b2 = pulse %>%
  filter(state == 'Minnesota') %>%
  mutate(gad2_phq2_sum = gad2_sum + phq2_sum) %>%
  select(gad2_sum, PWEIGHT, floyd_weekOrNot) %>%
  filter(floyd_weekOrNot == 1)
ttest_mn_gad = weights::wtd.t.test(b1$gad2_sum, b2$gad2_sum, b1$PWEIGHT, b2$PWEIGHT)
ttest_mn_gad

# Other states PHQ2
b1 = pulse %>%
  filter(state != 'Minnesota') %>%
  mutate(gad2_phq2_sum = gad2_sum + phq2_sum) %>%
  select(phq2_sum, PWEIGHT, floyd_weekOrNot) %>%
  filter(floyd_weekOrNot == 0)
b2 = pulse %>%
  filter(state != 'Minnesota') %>%
  mutate(gad2_phq2_sum = gad2_sum + phq2_sum) %>%
  select(phq2_sum, PWEIGHT, floyd_weekOrNot) %>%
  filter(floyd_weekOrNot == 1)
ttest_otherStates_phq = weights::wtd.t.test(b1$phq2_sum, b2$phq2_sum, b1$PWEIGHT, b2$PWEIGHT)
ttest_otherStates_phq

# Other states GAD
b1 = pulse %>%
  filter(state != 'Minnesota') %>%
  mutate(gad2_phq2_sum = gad2_sum + phq2_sum) %>%
  select(gad2_sum, PWEIGHT, floyd_weekOrNot) %>%
  filter(floyd_weekOrNot == 0)
b2 = pulse %>%
  filter(state != 'Minnesota') %>%
  mutate(gad2_phq2_sum = gad2_sum + phq2_sum) %>%
  select(gad2_sum, PWEIGHT, floyd_weekOrNot) %>%
  filter(floyd_weekOrNot == 1)
ttest_otherStates_gad = weights::wtd.t.test(b1$gad2_sum, b2$gad2_sum, b1$PWEIGHT, b2$PWEIGHT)
ttest_otherStates_gad


# Plotting -----

# Plot the mental health Cohen's d values along with significance levels, which are derived from
# variables above.
p_vals_pulse_mn %>%
  inner_join(p_vals_pulse_mn_p) %>%
  mutate(g = if_else(g == 'USA', 'Other states', g),
         g = factor(g, levels = c('Other states', 'Minnesota')),
         p = if_else(g == 'Other states', '***', '**'),
         measure = if_else(measure == 'gad2_sum', 'Anxiety', 'Depression')) %>%
  ggplot(aes(g, d, fill = measure)) +
  geom_bar(stat = 'identity', color = 'black', position = 'dodge', width = 0.7) +
  geom_text(aes(y = d + 0.005, label = p), position = position_dodge(width = 0.7),
            size = 10, vjust = 0.7,) +
  geom_signif(y_position = c(.105),
              xmin = c(0.8), xmax = c(1.8),
              annotation = c(''),
              textsize = 8) +
  geom_text(x = (1.8 - .8) / 2 + .8,
            y = .105 + 0.005,
            label = '*', size = signif_size, vjust = .7) +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0, 0.13)) +
  scale_fill_manual(values = c('#333333', 'white')) +
  theme_Publication() +
  theme(panel.grid.major = element_blank(),
        axis.line.y = element_line(size = 1.3),
        legend.position = c(.9, .9),
        legend.title = element_blank(),
        legend.background = element_rect(fill = 'transparent'),
        aspect.ratio = 1/3) +
  labs(x = '', y = paste0('Effect size (Cohen\'s d)'),
       fill = '') +
  coord_flip() +
  guides(fill = guide_legend(reverse = TRUE)) +
  ggsave(paste0('mental_health_significance_mn.png'), width=12, height=4, units='in', dpi=300)