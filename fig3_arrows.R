source('setup_all.R')
source('setup_gallup.R')
source('setup_significance_testing.R')


# ARROW PLOTS =====

# Arrow plots represent the change in emotion levels from before Floyd's death (the bottom of the arrow)
# to after (the top of the arrow). Each demographic category is represented by a different arrow.


# Setup -----

# Limit to Floyd week or the preceding 4 weeks
indiv = indiv %>%
  filter(floyd_weekOrNot == 1 | (floyd_weekOrNot == 0 & 
                                   ((endDate_week_delta < 0 & endDate_week_delta > -5)))) %>%
  select(-endDate_week_delta)

# Convert county level FIPS to state. This is fairly slow
anger_sad_state = indiv %>%
  select(-EMPLOYEE_KEY_VALUE, -endDate_asDate) %>%
  mutate(FIPS = if_else(FIPS == '46113', '46102', FIPS)) %>%  # this FIPS was changed: https://github.com/pdil/usmap/issues/6)
  filter(!is.na(FIPS), FIPS != '') %>%
  rowwise() %>%
  mutate(state = fips_info(FIPS)$abbr) %>%
  ungroup()


# Calculate proportions and standard errors -----

# The tricky thing about what follows is that we actually want to depict non-exclusive categories,
# that is, Minnesota will be compared against the rest of the US, but Minneapolis will be compared
# against only the rest of Minnesota -- so Minnesota is not equivalent in the two comparisons.

# Calculate proportion and standard error of proportion for Minnesota and all other states
# for anger and sadness.
sad_mn = anger_sad_state %>%
  mutate(is_MN = if_else(state == 'MN', 'MN', 'Other')) %>%
  select(floyd_weekOrNot, WEC_sadF, WEE_angerF, is_MN) %>%
  mutate(descriptor = 'is_MN', value = is_MN) %>%
  group_by(descriptor, value, floyd_weekOrNot) %>%
  summarize_at(vars(WEE_angerF, WEC_sadF), 
               .funs = list(~mean(., na.rm = T), 
                            se = ~sqrt(mean(., na.rm = T) * (1 - mean(., na.rm = T)) / n()))) %>%
  gather('stat', 'val', 
         WEE_angerF_mean, WEE_angerF_se, 
         WEC_sadF_mean, WEC_sadF_se) %>% 
  spread(floyd_weekOrNot, val) %>%
  rename(floyd = `1`, not_floyd = `0`) %>%
  filter(str_detect(stat, 'sadF'),
       !(value %in% c('asian', 'other'))) %>%
  drop_na() %>%
  gather('floyd', 'val', floyd, not_floyd) %>%
  spread(stat, val) %>%
  group_by(descriptor, value) %>%
  rename(not_floyd_mean = WEC_sadF_mean, not_floyd_se = WEC_sadF_se) %>%
  mutate(floyd_mean = lag(not_floyd_mean), floyd_se = lag(not_floyd_se)) %>%
  ungroup() %>%
  filter(floyd == 'not_floyd') %>%
  mutate(value = factor(value, levels = c('MN', 'Other')),
         descriptor = case_when(
           descriptor == 'is_MN' ~ 'Minnesota'
         ),
         descriptor = factor(descriptor, levels = c('Minnesota')))
angry_mn = anger_sad_state %>%
  mutate(is_MN = if_else(state == 'MN', 'MN', 'Other')) %>%
  select(floyd_weekOrNot, WEC_sadF, WEE_angerF, is_MN) %>%
  mutate(descriptor = 'is_MN', value = is_MN) %>%
  group_by(descriptor, value, floyd_weekOrNot) %>%
  summarize_at(vars(WEE_angerF, WEC_sadF), 
               .funs = list(~mean(., na.rm = T), 
                            ~sd(., na.rm = T),
                            se = ~sqrt(mean(., na.rm = T) * (1 - mean(., na.rm = T)) / n()))) %>%
  gather('stat', 'val', 
         WEE_angerF_mean, WEE_angerF_sd, WEE_angerF_se, 
         WEC_sadF_mean, WEC_sadF_sd, WEC_sadF_se) %>% 
  spread(floyd_weekOrNot, val) %>%
  rename(floyd = `1`, not_floyd = `0`) %>%
  filter(str_detect(stat, 'angerF'),
         descriptor %in% c('is_MN'),
         !(value %in% c('asian', 'other'))) %>%
  drop_na() %>%
  gather('floyd', 'val', floyd, not_floyd) %>%
  spread(stat, val) %>%
  group_by(descriptor, value) %>%
  rename(not_floyd_mean = WEE_angerF_mean, not_floyd_se = WEE_angerF_se) %>%
  mutate(floyd_mean = lag(not_floyd_mean), floyd_se = lag(not_floyd_se)) %>%
  ungroup() %>%
  filter(floyd == 'not_floyd') %>%
  mutate(value = factor(value, levels = c('MN', 'Other')),
         descriptor = case_when(
           descriptor == 'is_MN' ~ 'Minnesota'
         ),
         descriptor = factor(descriptor, levels = c('Minnesota')))
angry_sad_mn = angry_mn %>%
  mutate(descriptor = 'Anger') %>%
  bind_rows(sad_mn) %>%
  mutate(descriptor = if_else(descriptor == 'Anger', descriptor, 'Sadness'))

# Do the same thing, but for Minneapolis (which is compared against the rest of Minnesota).
# Note that although we end up calculating the proportion and standard error for other states, this won't be used.
sad_mn_city = anger_sad_state %>%
  mutate(is_MN = if_else(state == 'MN', 'MN', 'Other'),
         is_Minneapolis = if_else(FIPS == '27053', 'Minneapolis', if_else(state == 'MN', 'Not Minneapolis', 'Other state'))) %>%
  select(floyd_weekOrNot, WEC_sadF, WEE_angerF, is_MN, is_Minneapolis) %>%
  mutate(descriptor = 'is_Minneapolis', value = is_Minneapolis) %>%
  group_by(descriptor, value, floyd_weekOrNot) %>%
  summarize_at(vars(WEE_angerF, WEC_sadF), 
               .funs = list(~mean(., na.rm = T), 
                            ~sd(., na.rm = T),
                            se = ~sqrt(mean(., na.rm = T) * (1 - mean(., na.rm = T)) / n()))) %>%
  gather('stat', 'val', 
         WEE_angerF_mean, WEE_angerF_sd, WEE_angerF_se,
         WEC_sadF_mean, WEC_sadF_sd, WEC_sadF_se) %>% 
  spread(floyd_weekOrNot, val) %>%
  rename(floyd = `1`, not_floyd = `0`) %>%
  filter(str_detect(stat, 'sadF'),
         descriptor %in% c('is_Minneapolis'),
         !(value %in% c('asian', 'other'))) %>%
  drop_na() %>%
  gather('floyd', 'val', floyd, not_floyd) %>%
  spread(stat, val) %>%
  group_by(descriptor, value) %>%
  rename(not_floyd_mean = WEC_sadF_mean, not_floyd_se = WEC_sadF_se) %>%
  mutate(floyd_mean = lag(not_floyd_mean), floyd_se = lag(not_floyd_se)) %>%
  ungroup() %>%
  filter(floyd == 'not_floyd') %>%
  mutate(value = factor(value, levels = c('Minneapolis', 'Not Minneapolis', 'Other state')),
         descriptor = case_when(
           descriptor == 'is_Minneapolis' ~ 'Minneapolis'
         ),
         descriptor = factor(descriptor, levels = c('Minneapolis', 'Not Minneapolis', 'Other state')))
angry_mn_city = anger_sad_state %>%
  mutate(is_MN = if_else(state == 'MN', 'MN', 'Other'),
         is_Minneapolis = if_else(FIPS == '27053', 'Minneapolis', if_else(state == 'MN', 'Not Minneapolis', 'Other state'))) %>%
  select(floyd_weekOrNot, WEC_sadF, WEE_angerF, is_MN, is_Minneapolis) %>%
  mutate(descriptor = 'is_Minneapolis', value = is_Minneapolis) %>%
  group_by(descriptor, value, floyd_weekOrNot) %>%
  summarize_at(vars(WEE_angerF, WEC_sadF), 
               .funs = list(~mean(., na.rm = T), 
                            ~sd(., na.rm = T),
                            se = ~sqrt(mean(., na.rm = T) * (1 - mean(., na.rm = T)) / n()))) %>%
  gather('stat', 'val', 
         WEE_angerF_mean, WEE_angerF_sd, WEE_angerF_se,
         WEC_sadF_mean, WEC_sadF_sd, WEC_sadF_se) %>% 
  spread(floyd_weekOrNot, val) %>%
  rename(floyd = `1`, not_floyd = `0`) %>%
  filter(str_detect(stat, 'angerF'),
         descriptor %in% c('is_Minneapolis'),
         !(value %in% c('asian', 'other'))) %>%
  drop_na() %>%
  gather('floyd', 'val', floyd, not_floyd) %>%
  spread(stat, val) %>%
  group_by(descriptor, value) %>%
  rename(not_floyd_mean = WEE_angerF_mean, not_floyd_se = WEE_angerF_se) %>%
  mutate(floyd_mean = lag(not_floyd_mean), floyd_se = lag(not_floyd_se)) %>%
  ungroup() %>%
  filter(floyd == 'not_floyd') %>%
  mutate(value = factor(value, levels = c('Minneapolis', 'Not Minneapolis', 'Other state')),
         descriptor = case_when(
           descriptor == 'is_Minneapolis' ~ 'Minneapolis'
         ),
         descriptor = factor(descriptor, levels = c('Minneapolis', 'Not Minneapolis', 'Other state')))
angry_sad_mn_city = angry_mn_city %>%
  mutate(descriptor = 'Anger') %>%
  bind_rows(sad_mn_city) %>%
  mutate(descriptor = if_else(descriptor == 'Anger', descriptor, 'Sadness'))


# Significance testing -----

# Run nonparametric significance test to compare arrow sizes (changes in proportion) between
# pairs of mutually exclusive demographic categories. This will take a while!

# Descriptor contains the overarching category containing mutually exclusive subcategories,
# in this case only the "is_MN" descriptor exists with values "Minnesota" and "Other states".
indiv_long_mnVsUs = indiv_state %>%
  mutate(is_MN = if_else(state == 'MN', 'Minnesota', 'Other states')) %>%
  select(-endDate_week_delta, -FIPS, -DEMO_RACE_NAME, -state) %>%
  gather('descriptor', 'value', -WEE_angerF, -WEC_sadF, -floyd_weekOrNot)
descs_mnVsUs = indiv_long_mnVsUs %>%
  select(descriptor) %>%
  distinct() %>%
  drop_na() %>%
  .$descriptor

# Same as above, but here descriptor will be "is_Minneapolis" with values
# "Minneapolis", "Not Minneapolis" (i.e., the rest of MN), and "Other state".
indiv_long_mn = indiv_state %>%
  mutate(is_Minneapolis = if_else(FIPS == '27053', 'Minneapolis', 
                                  if_else(state == 'MN', 'Not Minneapolis', 'Other state'))) %>%
  select(-endDate_week_delta, -FIPS, -DEMO_RACE_NAME, -state) %>%
  gather('descriptor', 'value', -WEE_angerF, -WEC_sadF, -floyd_weekOrNot)
descs_mn = indiv_long_mn %>%
  select(descriptor) %>%
  distinct() %>%
  drop_na() %>%
  .$descriptor

# Now run tests
set.seed(1)
p_vals_mn = tibble()
for (emo in c('WEE_angerF', 'WEC_sadF')) {
  for (d in descs_mn) {
    cats = indiv_long_mn %>%
      filter(descriptor == {{ d }}) %>%
      select(value) %>%
      distinct() %>%
      drop_na() %>%
      .$value
    for (c1i in seq(length(cats))) {
      for (c2i in seq(length(cats))) {
        if (c1i >= c2i) {
          next
        }
        
        c1 = cats[c1i]
        c2 = cats[c2i]
        if (c1 == c2) {
          next
        }
        print(paste0(emo, ': ', c1, ', ', c2))
        
        # First get the observed difference in change of proportion between the two categories
        test_stat = get_diff(indiv_long_mn, c1, c2, emo)

        # Get differences for a bunch of random resamples
        diffs = replicate(n_reps, get_bootstrap_diff(indiv_long_mn, c1, c2, emo))

        # The p-value is the percentage of resamples with a difference at least as large as that observed
        p_val = length(diffs[abs(diffs) > abs(test_stat)]) / n_reps
        
        p_vals_mn = p_vals_mn %>%
          bind_rows(list(e = {{ emo }}, v1 = {{ c1 }}, v2 = {{ c2 }}, p = p_val))
      }
    }
  }
}

set.seed(1)
p_vals_mnVsUS = tibble()
for (emo in c('WEE_angerF', 'WEC_sadF')) {
  for (d in descs_mnVsUs) {
    cats = indiv_long_mnVsUs %>%
      filter(descriptor == {{ d }}) %>%
      select(value) %>%
      distinct() %>%
      drop_na() %>%
      .$value
    for (c1i in seq(length(cats))) {
      for (c2i in seq(length(cats))) {
        if (c1i >= c2i) {
          next
        }
        
        c1 = cats[c1i]
        c2 = cats[c2i]
        if (c1 == c2) {
          next
        }
        print(paste0(emo, ': ', c1, ', ', c2))
        
        # First get the observed difference in change of proportion between the two categories
        test_stat = get_diff(indiv_long_mnVsUs, c1, c2, emo)

        # Get differences for a bunch of random resamples
        diffs = replicate(n_reps, get_bootstrap_diff(indiv_long_mnVsUs, c1, c2, emo))

        # The p-value is the percentage of resamples with a difference at least as large as that observed
        p_val = length(diffs[abs(diffs) > abs(test_stat)]) / n_reps
        
        p_vals_mnVsUS = p_vals_mnVsUS %>%
          bind_rows(list(e = {{ emo }}, v1 = {{ c1 }}, v2 = {{ c2 }}, p = p_val))
      }
    }
  }
}


# Arrow plots -----

# First, drop "Not Minneapolis" (i.e., MN outside Minneapolis) because we don't actually need that.
# Then, concatenate with the MN vs. rest of US, dropping "Other" (because non-MN states are already covered by \code{angry_sad_mn_city}).
# The following code does the following for both anger and sadness:
# * Filtering to variables of interest
# * Reshaping data
# * Cleaning variable names for presentation in figure
# * Creating figure
# Significance levels are derived from the \code{p_vals_mn} and \code{p_vals_mnVsUs} variables.
angry_sad_mn_city %>%
  filter(value != 'Not Minneapolis') %>%
  bind_rows(angry_sad_mn %>% filter(value != 'Other')) %>%
  mutate(value = if_else(descriptor == 'Anger', paste0(value, ', Anger'), paste0(value, ', Sadness')),
         value = factor(value, 
                        levels = c('Other state, Anger', 'MN, Anger', 'Minneapolis, Anger',
                                   'Other state, Sadness', 'MN, Sadness', 'Minneapolis, Sadness'))) %>%
  ggplot(aes(value, fill = descriptor)) +
  geom_tile(aes(y = floyd_mean, height = floyd_se * 2, width = .25),
            linetype = 'blank',
            position = position_nudge(),
            alpha = 0.45) +
  geom_tile(aes(y = not_floyd_mean, height = floyd_se * 2, width = .25),
            linetype = 'blank',
            position = position_nudge(),
            alpha = 0.45) +
  geom_segment(aes(xend = value, y = not_floyd_mean, yend = floyd_mean), 
               arrow = arrow(length = unit(0.4, "cm"), type = "closed")) +
  geom_signif(aes(y = floyd_mean),
              y_position = c(.75, .53, .55, .75),
              comparisons = list(
                c('Minneapolis, Anger', 'Other state, Anger'),
                c('MN, Anger', 'Other state, Anger'),
                c('MN, Sadness', 'Other state, Sadness'),
                c('Minneapolis, Sadness', 'Other state, Sadness')
              ), 
              annotation = c('*', '**', '*', '†'),
              textsize = signif_size) +
  theme_Publication() +
  theme(axis.text.x = element_text(angle = 35, hjust = 1),
        panel.grid.major.x = element_blank(),
        legend.position = c(10, 10)) +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1L),
                     breaks = seq(0, 1, by = .1),
                     limits = c(0, 1)) +
  scale_x_discrete(labels = c('Other states', 'Minnesota', 'Minneapolis',
                              'Other states', 'Minnesota', 'Minneapolis')) +
  scale_color_manual(values = c(orange, blue)) +
  scale_fill_manual(values = c(orange, blue)) +
  labs(x = '',
       y = 'Percent angry or sad',
       color = '', fill = '') +
  ggsave('minnesota_arrows.png', width=12, height=8, units='in', dpi=300)
angry_sad_mn_city %>%
  filter(value != 'Not Minneapolis') %>%
  bind_rows(angry_sad_mn %>% filter(value != 'Other')) %>%
  mutate(value = if_else(descriptor == 'Anger', paste0(value, ', Anger'), paste0(value, ', Sadness')),
         value = factor(value, 
                        levels = c('Other state, Anger', 'MN, Anger', 'Minneapolis, Anger',
                                   'Other state, Sadness', 'MN, Sadness', 'Minneapolis, Sadness'))) %>%
  ggplot(aes(value, fill = descriptor)) +
  geom_tile(aes(y = floyd_mean, height = floyd_se * 2, width = .25),
            linetype = 'blank',
            position = position_nudge(),
            alpha = 0.45) +
  geom_tile(aes(y = not_floyd_mean, height = floyd_se * 2, width = .25),
            linetype = 'blank',
            position = position_nudge(),
            alpha = 0.45) +
  geom_segment(aes(xend = value, y = not_floyd_mean, yend = floyd_mean), 
               arrow = arrow(length = unit(0.4, "cm"), type = "closed")) +
  geom_signif(aes(y = floyd_mean),
              y_position = c(.75, .53, .55, .75),
              comparisons = list(
                c('Minneapolis, Anger', 'Other state, Anger'),
                c('MN, Anger', 'Other state, Anger'),
                c('MN, Sadness', 'Other state, Sadness'),
                c('Minneapolis, Sadness', 'Other state, Sadness')
              ), 
              annotation = c('0.013', '0.005', '0.013', '0.097'),
              textsize = signif_size) +
  theme_Publication() +
  theme(axis.text.x = element_text(angle = 35, hjust = 1),
        panel.grid.major.x = element_blank(),
        legend.position = c(10, 10)) +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1L),
                     breaks = seq(0, 1, by = .1),
                     limits = c(0, 1)) +
  scale_x_discrete(labels = c('Other states', 'Minnesota', 'Minneapolis',
                              'Other states', 'Minnesota', 'Minneapolis')) +
  scale_color_manual(values = c(orange, blue)) +
  scale_fill_manual(values = c(orange, blue)) +
  labs(x = '',
       y = 'Percent angry or sad',
       color = '', fill = '') +
  ggsave('minnesota_arrows_with_pvals.png', width=12, height=8, units='in', dpi=300)