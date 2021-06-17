# Perform setup
source('setup_all.R')
source('setup_gallup.R')
source('setup_census.R')
source('setup_significance_testing.R')


# DAILY TIMELINES =====

# Calculates daily proportion and standard error. These estimates are unweighted since much of the data
# cannot be weighted at the day level.

# Anger -----

indiv %>%
  mutate(endDate_asDate = as.Date(endDate_asDate)) %>% 
  group_by(endDate_asDate) %>%
  summarize(across(.cols = c('WEE_angerF', 'WEC_sadF'),
                   .fns = list(p = ~mean(., na.rm = T),
                               se = ~sqrt(mean(., na.rm = T) * (1 - mean(., na.rm = T)) / n())))) %>%
  ggplot(aes(endDate_asDate, WEE_angerF_p)) +
  geom_point() +
  geom_smooth(span = .1, se = F, color = orange) +
  geom_errorbar(aes(ymin = WEE_angerF_p - WEE_angerF_se, ymax = WEE_angerF_p + WEE_angerF_se)) +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1L),
                     breaks = seq(-1, 1, by = .05)) +
  theme_Publication() +
  theme(legend.position = 'none') +
  labs(x = 'Day', y = 'Percent angry') +
  ggsave('anger_daily_timeseries.png', width=11, height=8, units='in', dpi=300)

# Sadness -----

indiv %>%
  mutate(endDate_asDate = as.Date(endDate_asDate)) %>% 
  group_by(endDate_asDate) %>%
  summarize(across(.cols = c('WEE_angerF', 'WEC_sadF'),
                   .fns = list(p = ~mean(., na.rm = T),
                               se = ~sqrt(mean(., na.rm = T) * (1 - mean(., na.rm = T)) / n())))) %>%
  ggplot(aes(endDate_asDate, WEC_sadF_p)) +
  geom_point() +
  geom_smooth(span = .1, se = F, color = blue) +
  geom_errorbar(aes(ymin = WEC_sadF_p - WEC_sadF_se, ymax = WEC_sadF_p + WEC_sadF_se)) +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1L),
                     breaks = seq(-1, 1, by = .05)) +
  theme_Publication() +
  theme(legend.position = 'none') +
  labs(x = 'Day', y = 'Percent sad') +
  ggsave('sad_daily_timeseries.png', width=11, height=8, units='in', dpi=300)


# TIMELINES BY CATEGORY =====


# Calculate weekly proportions and standard errors for each category. These are not weighted
# since Gallup weights do not apply to subgroups.
anger_sad_summary = indiv %>%
  mutate(DEMO_AGE_BINS2 = if_else(DEMO_AGE_BINS2 == '45+', '46+', DEMO_AGE_BINS2)) %>%
  select(-EMPLOYEE_KEY_VALUE, -endDate_week_delta) %>%
  mutate(wk = floor_date(as.Date(endDate_asDate), 'week', week_start = 1)) %>%
  gather('descriptor', 'value', -WEE_angerF, -WEC_sadF, -wk) %>%
  group_by(descriptor, value, wk) %>%
  summarize_at(vars(WEE_angerF, WEC_sadF), 
               .funs = list(~mean(., na.rm = T), 
                            se = ~sqrt(mean(., na.rm = T) * (1 - mean(., na.rm = T)) / n())))

# Create timelines for each overarching demographic category. Excludes some races that have little data and
# add visual confusion to the plots.

# Anger
for (desc in c('DEMO_GENDER_NAME', 'DEMO_INCOME_2020_name', 'party_name', 'DEMO_AGE_BINS2', 'DEMO_RACE_NAME', 'DEMO_EDUCATION_2017')) {
  anger_sad_summary %>%
    filter(descriptor == {{ desc }},
           !is.na(value),
           !(value %in% c('asian', 'other', 'hispanic'))) %>%
    mutate(value = str_to_title(value)) %>%
    ggplot(aes(wk, WEE_angerF_mean, color = value, shape = value, fill = value)) +
    geom_point(size = 3) +
    geom_smooth(span = .1, se = F) +
    geom_rect(aes(ymin = WEE_angerF_mean - WEE_angerF_se, ymax = WEE_angerF_mean + WEE_angerF_se,
                  xmin = wk - 1, xmax = wk + 1),
              linetype = 'blank',
              position = position_nudge(),
              alpha = 0.25) +
    theme_Publication() +
    scale_y_continuous(labels = scales::label_percent(accuracy = 1L),
                       breaks = seq(0, 1, by = .05)) +
    scale_color_lancet() +
    scale_fill_lancet() +
    scale_linetype_manual(values = c('solid', 'longdash', 'dotdash')) +
    theme(legend.key.width = unit(1.25, 'cm')) +
    labs(x = 'Week', y = 'Percent angry', color = '', shape = '', fill = '') +
    ggsave(paste0('anger_', desc, '_main.png'), width=12, height=8, units='in', dpi=300)
}

# Sadness
for (desc in c('DEMO_GENDER_NAME', 'DEMO_INCOME_2020_name', 'party_name', 'DEMO_AGE_BINS2', 'DEMO_RACE_NAME', 'DEMO_EDUCATION_2017')) {
  anger_sad_summary %>%
    filter(descriptor == {{ desc }},
           !is.na(value),
           !(value %in% c('asian', 'other', 'hispanic'))) %>%
    mutate(value = str_to_title(value)) %>%
    ggplot(aes(wk, WEC_sadF_mean, color = value, shape = value, fill = value)) +
    geom_point(size = 3) +
    geom_smooth(span = .1, se = F) +
    geom_rect(aes(ymin = WEC_sadF_mean - WEC_sadF_se, ymax = WEC_sadF_mean + WEC_sadF_se,
                  xmin = wk - 1, xmax = wk + 1),
              linetype = 'blank',
              position = position_nudge(),
              alpha = 0.25) +
    theme_Publication() +
    scale_y_continuous(labels = scales::label_percent(accuracy = 1L),
                       breaks = seq(0, 1, by = .05)) +
    scale_color_lancet() +
    scale_fill_lancet() +
    scale_linetype_manual(values = c('solid', 'longdash', 'dotdash')) +
    theme(legend.key.width = unit(1.25, 'cm')) +
    labs(x = 'Week', y = 'Percent sad', color = '', shape = '', fill = '') +
    ggsave(paste0('sad_', desc, '_main.png'), width=12, height=8, units='in', dpi=300)
}

 
# FULL ARROW PLOTS: ANGER/SADNESS =====


# Arrow plots represent the change in emotion levels from before Floyd's death (the bottom of the arrow)
# to after (the top of the arrow). Each demographic category is represented by a different arrow.

# See fig1_arrows.R for the main arrow plot code. This code is largely a repeat of that file, but produces
# arrow plots containing more demographic categories than are shown in the main figure. Also contains p-values
# instead of significance levels.


# Change in proportion -----

# Limit to Floyd week or the preceding 4 weeks
indiv = indiv %>%
  filter(floyd_weekOrNot == 1 | (floyd_weekOrNot == 0 & 
                                   ((endDate_week_delta < 0 & endDate_week_delta > -5)))) %>%
  select(-endDate_week_delta)

# Calculate proportion (mean) and standard error of each emotion (anger and sadness) for each demographic
# category separately. Note that weights are not used, per Gallup's recommendations.
anger_sad_summary = indiv %>%
  select(-EMPLOYEE_KEY_VALUE, -endDate_asDate, -FIPS) %>%
  gather('descriptor', 'value', -WEE_angerF, -WEC_sadF, -floyd_weekOrNot) %>%
  group_by(descriptor, value, floyd_weekOrNot) %>%
  summarize_at(vars(WEE_angerF, WEC_sadF), 
               .funs = list(~mean(., na.rm = T), 
                            se = ~sqrt(mean(., na.rm = T) * (1 - mean(., na.rm = T)) / n())))

# Also calculate proportion and standard error overall (i.e., not by demographic category).
overall_anger_sad_summary = indiv %>%
  group_by(floyd_weekOrNot) %>%
  summarize_at(vars(WEE_angerF, WEC_sadF), 
               .funs = list(~mean(., na.rm = T), 
                            se = ~sqrt(mean(., na.rm = T) * (1 - mean(., na.rm = T)) / n()))) %>%
  mutate(descriptor = 'Overall', value = 'Overall')

anger_sad_summary = anger_sad_summary %>% bind_rows(overall_anger_sad_summary)

# Reshape data and calculate change in emotion from before Floyd's death to after.
anger_sad_summary = anger_sad_summary %>%
  gather('stat', 'val', 
         WEE_angerF_mean, WEE_angerF_se, 
         WEC_sadF_mean, WEC_sadF_se) %>% 
  spread(floyd_weekOrNot, val) %>%
  rename(not_floyd = `0`,
         floyd = `1`) %>%
  mutate(change = floyd - not_floyd)


# Significance testing -----

# Run nonparametric significance test to compare arrow sizes (changes in proportion) between
# pairs of mutually exclusive demographic categories. This will take a while!

# Descriptor contains the overarching category containing mutually exclusive subcategories,
# e.g., race, gender, income level, etc.
indiv_long = indiv %>%
  select(-EMPLOYEE_KEY_VALUE, -endDate_asDate, -WEIGHT, -FIPS) %>%
  gather('descriptor', 'value', -WEE_angerF, -WEC_sadF, -floyd_weekOrNot)
descs = indiv_long %>%
  select(descriptor) %>%
  distinct() %>%
  drop_na() %>%
  .$descriptor

set.seed(1)
p_vals = tibble()
for (emo in c('WEE_angerF', 'WEC_sadF')) {
  for (d in descs) {
    cats = indiv_long %>%
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
        test_stat = get_diff(indiv_long, c1, c2, emo)

        # Get differences for a bunch of random resamples
        diffs = replicate(n_reps, get_bootstrap_diff(indiv_long, c1, c2, emo))

        # The p-value is the percentage of resamples with a difference at least as large as that observed
        p_val = length(diffs[abs(diffs) >= abs(test_stat)]) / n_reps
        
        p_vals = p_vals %>%
          bind_rows(list(e = {{ emo }}, v1 = {{ c1 }}, v2 = {{ c2 }}, p = p_val))
      }
    }
  }
}

# Clean up various text
p_vals = p_vals %>%
  mutate(across(c(v1, v2),
                ~if_else(. == '$119,999+', '$120,000+', 
                        if_else(. == '$60,000 to $119,999', '$60,000-$119,999', .)))) %>%
  mutate(across(c(v1, v2),
                str_to_title)) %>%
  mutate(across(c(v1, v2),
                ~factor(., levels = c('Overall', 
                                     'Asian', 'Black', 'Hispanic', 'White', 'Other',
                                     'Democrat', 'Republican',
                                     '18-30', '31-45', '45+',
                                     'Female', 'Male',
                                     'Less Than College', 'College', 'More Than College',
                                     '<$59,999', '$60,000-$119,999', '$120,000+'))))


# Plot the figures -----

# Create arrow plots for change in anger and sadness. Contains several steps:
# * Filtering to variables of interest
# * Reshaping data
# * Cleaning variable names for presentation in figure
# * Creating figure
# Significance levels are derived from the \code{p_vals} variable.
anger_sad_summary %>%
  select(-change) %>%
  filter(str_detect(stat, 'angerF')) %>%
  drop_na() %>%
  gather('floyd', 'val', floyd, not_floyd) %>%
  spread(stat, val) %>%
  group_by(descriptor, value) %>%
  rename(not_floyd_mean = WEE_angerF_mean, not_floyd_se = WEE_angerF_se) %>%
  mutate(floyd_mean = lag(not_floyd_mean), floyd_se = lag(not_floyd_se)) %>%
  ungroup() %>%
  filter(floyd == 'not_floyd') %>% 
  mutate(value = if_else(value == '$119,999+', '$120,000+', 
                         if_else(value == '$60,000 to $119,999', '$60,000-$119,999', 
                                 if_else(value == '<$59,999', '$0-$59,999', value))),
         value = reorder(str_to_title(value), descriptor),
         value = factor(value, levels = c('Overall', 
                                          'Asian', 'Black', 'Hispanic', 'White', 'Other',
                                          'Democrat', 'Republican',
                                          '18-30', '31-45', '46+',
                                          'Female', 'Male',
                                          '< College', 'College', '> College',
                                          '$0-$59,999', '$60,000-$119,999', '$120,000+')),
         descriptor = case_when(
           descriptor == 'Overall' ~ 'Overall',
           descriptor == 'DEMO_AGE_BINS2' ~ 'Age',
           descriptor == 'DEMO_EDUCATION_2017' ~ 'Education',
           descriptor == 'DEMO_GENDER_NAME' ~ 'Gender',
           descriptor == 'DEMO_INCOME_2020_name' ~ 'Income',
           descriptor == 'DEMO_RACE_NAME' ~ 'Race',
           descriptor == 'party_name' ~ 'Political party'
         ),
         descriptor = factor(descriptor, levels = c('Overall', 'Race', 'Political party', 'Age',
                                                    'Gender', 'Education', 'Income'))) %>%
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
              y_position = c(.56, .6, .52, .43, .46, .53),
              comparisons = list(
                c('White', 'Black'),
                c('White', 'Asian'),
                c('Hispanic', 'Black'),
                c('Democrat', 'Republican'),
                c('46+', '31-45'),
                c('46+', '18-30')
              ), 
              annotation = c('<0.001', '0.014', '0.098', '0.044', '0.012', '0.006'),
              textsize = 8) +
  theme_Publication() +
  theme(axis.text.x = element_text(angle = 35, hjust = 1),
        panel.grid.major.x = element_blank(),
        legend.position = c(10, 10)) +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1L),
                     breaks = seq(0, 1, by = .1),
                     limits = c(NA, .65)) +
  scale_color_manual(values = c('#ba0c00', my_palette[2:length(my_palette)])) +
  scale_fill_manual(values = c('#ba0c00', my_palette[2:length(my_palette)])) +
  labs(x = '',
       y = 'Percent angry',
       color = '', fill = '') +
  ggsave('anger_arrows_full.png', width=12, height=8, units='in', dpi=300)

anger_sad_summary %>%
  select(-change) %>%
  filter(str_detect(stat, 'sadF')) %>%
  drop_na() %>%
  gather('floyd', 'val', floyd, not_floyd) %>%
  spread(stat, val) %>%
  group_by(descriptor, value) %>%
  rename(not_floyd_mean = WEC_sadF_mean, not_floyd_se = WEC_sadF_se) %>%
  mutate(floyd_mean = lag(not_floyd_mean), floyd_se = lag(not_floyd_se)) %>%
  ungroup() %>%
  filter(floyd == 'not_floyd') %>%
  mutate(value = if_else(value == '$119,999+', '$120,000+', 
                         if_else(value == '$60,000 to $119,999', '$60,000-$119,999', 
                                 if_else(value == '<$59,999', '$0-$59,999', value))),
         value = reorder(str_to_title(value), descriptor),
         value = factor(value, levels = c('Overall', 
                                          'Asian', 'Black', 'Hispanic', 'White', 'Other',
                                          'Democrat', 'Republican',
                                          '18-30', '31-45', '46+',
                                          'Female', 'Male',
                                          '< College', 'College', '> College',
                                          '$0-$59,999', '$60,000-$119,999', '$120,000+')),
         descriptor = case_when(
           descriptor == 'Overall' ~ 'Overall',
           descriptor == 'DEMO_AGE_BINS2' ~ 'Age',
           descriptor == 'DEMO_EDUCATION_2017' ~ 'Education',
           descriptor == 'DEMO_GENDER_NAME' ~ 'Gender',
           descriptor == 'DEMO_INCOME_2020_name' ~ 'Income',
           descriptor == 'DEMO_RACE_NAME' ~ 'Race',
           descriptor == 'party_name' ~ 'Political party'
         ),
         descriptor = factor(descriptor, levels = c('Overall', 'Race', 'Political party', 'Age',
                                                    'Gender', 'Education', 'Income'))) %>%
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
              y_position = c(.55, .61, .505, .42, .41),
              comparisons = list(
                c('White', 'Black'),
                c('White', 'Asian'),
                c('White', 'Hispanic'),
                c('46+', '31-45'),
                c('> College', 'College')
              ), 
              annotation = c('0.011', '0.021', '0.077', '0.082', '0.076'),
              textsize = 8) +
  theme_Publication() +
  theme(axis.text.x = element_text(angle = 35, hjust = 1),
        panel.grid.major.x = element_blank(),
        legend.position = c(10, 10)) +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1L),
                     breaks = seq(0, 1, by = .1),
                     limits = c(NA, .65)) +
  scale_color_manual(values = c('#00468b', my_palette[2:length(my_palette)])) +
  scale_fill_manual(values = c('#00468b', my_palette[2:length(my_palette)])) +
  labs(x = '',
       y = 'Percent sad',
       color = '', fill = '') +
  ggsave('sad_arrows_full.png', width=12, height=8, units='in', dpi=300)


# FULL ARROW PLOTS: GEOGRAPHY =====


# See fig3_arrows.R for the main arrow plot code. This code is largely a repeat of that file, but produces
# arrow plots containing p-values instead of significance levels.

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
indiv_long_mnVsUs = anger_sad_state %>%
  mutate(is_MN = if_else(state == 'MN', 'Minnesota', 'Other states')) %>%
  select(-FIPS, -state, -WEIGHT) %>%
  gather('descriptor', 'value', -WEE_angerF, -WEC_sadF, -floyd_weekOrNot)
descs_mnVsUs = indiv_long_mnVsUs %>%
  select(descriptor) %>%
  distinct() %>%
  drop_na() %>%
  .$descriptor

# Same as above, but here descriptor will be "is_Minneapolis" with values
# "Minneapolis", "Not Minneapolis" (i.e., the rest of MN), and "Other state".
indiv_long_mn = anger_sad_state %>%
  mutate(is_Minneapolis = if_else(FIPS == '27053', 'Minneapolis', 
                                  if_else(state == 'MN', 'Not Minneapolis', 'Other state'))) %>%
  select(-FIPS, -state, -WEIGHT) %>%
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


# Plot the mental health Cohen's d values along with p-values.
p_vals_pulse_mn %>%
  mutate(g = if_else(g == 'USA', 'Other states', g),
         g = factor(g, levels = c('Other states', 'Minnesota')),
         p = c(ttest_mn_gad$coefficients['p.value'],
               ttest_mn_phq$coefficients['p.value'],
               ttest_otherStates_gad$coefficients['p.value'],
               ttest_otherStates_phq$coefficients['p.value']),
         p = round(p, 3),
         p = ifelse(p < 0.001, '<0.001', p),
         measure = if_else(measure == 'gad2_sum', 'Anxiety', 'Depression')) %>%
  ggplot(aes(g, d, fill = measure)) +
  geom_bar(stat = 'identity', color = 'black', position = 'dodge', width = 0.7) +
  geom_text(aes(y = d + 0.002, label = p), position = position_dodge(width = 0.7),
            size = 10, vjust = 0.7, hjust = 0) +
  geom_signif(y_position = c(.1135),
              xmin = c(0.8), xmax = c(1.8),
              annotation = c(''),
              textsize = signif_size) +
  geom_text(x = (1.8 - .8) / 2 + .8,
            y = .1135 + 0.002,
            label = '0.011', size = signif_size, vjust = .7, hjust = 0) +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0, 0.135)) +
  scale_fill_manual(values = c('#333333', 'white')) +
  theme_Publication() +
  theme(panel.grid.major = element_blank(),
        axis.line.y = element_line(size = 1.3),
        legend.position = c(.9, .95),
        aspect.ratio = 1/3) +
  labs(x = '', y = paste0('Effect size (Cohen\'s d)'),
       fill = '') +
  coord_flip() +
  guides(fill = guide_legend(reverse = TRUE)) +
  ggsave(paste0('mental_health_significance_mn_with_ps.png'), width=12, height=4, units='in', dpi=300)


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

# Run a few weighted t-tests using the weights package. Tests compare pre- and post-Floyd symptom severity.

# Black Americans PHQ2
b1 = pulse %>%
  filter(is_black == 1) %>%
  mutate(gad2_phq2_sum = gad2_sum + phq2_sum) %>%
  select(phq2_sum, PWEIGHT, floyd_weekOrNot) %>%
  filter(floyd_weekOrNot == 0)
b2 = pulse %>%
  filter(is_black == 1) %>%
  mutate(gad2_phq2_sum = gad2_sum + phq2_sum) %>%
  select(phq2_sum, PWEIGHT, floyd_weekOrNot) %>%
  filter(floyd_weekOrNot == 1)
ttest_black_phq = weights::wtd.t.test(b1$phq2_sum, b2$phq2_sum, b1$PWEIGHT, b2$PWEIGHT)
ttest_black_phq

# Black Americans GAD
b1 = pulse %>%
  filter(is_black == 1) %>%
  mutate(gad2_phq2_sum = gad2_sum + phq2_sum) %>%
  select(gad2_sum, PWEIGHT, floyd_weekOrNot) %>%
  filter(floyd_weekOrNot == 0)
b2 = pulse %>%
  filter(is_black == 1) %>%
  mutate(gad2_phq2_sum = gad2_sum + phq2_sum) %>%
  select(gad2_sum, PWEIGHT, floyd_weekOrNot) %>%
  filter(floyd_weekOrNot == 1)
ttest_black_gad = weights::wtd.t.test(b1$gad2_sum, b2$gad2_sum, b1$PWEIGHT, b2$PWEIGHT)
ttest_black_gad

# White Americans PHQ2
b1 = pulse %>%
  filter(is_white == 1 & is_hispanic == 0) %>%
  mutate(gad2_phq2_sum = gad2_sum + phq2_sum) %>%
  select(phq2_sum, PWEIGHT, floyd_weekOrNot) %>%
  filter(floyd_weekOrNot == 0)
b2 = pulse %>%
  filter(is_white == 1 & is_hispanic == 0) %>%
  mutate(gad2_phq2_sum = gad2_sum + phq2_sum) %>%
  select(phq2_sum, PWEIGHT, floyd_weekOrNot) %>%
  filter(floyd_weekOrNot == 1)
ttest_white_phq = weights::wtd.t.test(b1$phq2_sum, b2$phq2_sum, b1$PWEIGHT, b2$PWEIGHT)
ttest_white_phq

# White Americans GAD
b1 = pulse %>%
  filter(is_white == 1 & is_hispanic == 0) %>%
  mutate(gad2_phq2_sum = gad2_sum + phq2_sum) %>%
  select(gad2_sum, PWEIGHT, floyd_weekOrNot) %>%
  filter(floyd_weekOrNot == 0)
b2 = pulse %>%
  filter(is_white == 1 & is_hispanic == 0) %>%
  mutate(gad2_phq2_sum = gad2_sum + phq2_sum) %>%
  select(gad2_sum, PWEIGHT, floyd_weekOrNot) %>%
  filter(floyd_weekOrNot == 1)
ttest_white_gad = weights::wtd.t.test(b1$gad2_sum, b2$gad2_sum, b1$PWEIGHT, b2$PWEIGHT)
ttest_white_gad


# Plot the mental health Cohen's d values along with p-values.
p_vals_pulse %>%
  inner_join(p_vals_pulse_p) %>%
  mutate(measure = if_else(measure == 'GAD2', 'Anxiety', 'Depression'),
         p = c(ttest_black_gad$coefficients['p.value'],
               ttest_black_phq$coefficients['p.value'],
               ttest_white_gad$coefficients['p.value'],
               ttest_white_phq$coefficients['p.value']),
         p = round(p, 3),
         p = ifelse(p < 0.001, '<0.001', p)) %>%
  ggplot(aes(g, d, fill = measure)) +
  geom_bar(stat = 'identity', position = 'dodge',
           color = 'black', width = 0.7) +
  #geom_errorbar(aes(ymin = d - se, ymax = d + se), width = 0.25, color = 'blue') +
  geom_text(aes(y = d + 0.002, label = p), position = position_dodge(width = 0.7),
            size = signif_size, vjust = .7, hjust = 0) +
  geom_signif(y_position = c(.145, .1),
              xmin = c(1.125, 0.855), xmax = c(2.125, 1.855),
              annotation = c('', ''),
              textsize = signif_size) +
  geom_text(x = (2.125 - 1.125) / 2 + 1.125,
            y = .145 + 0.005,
            label = '<0.001', size = signif_size, vjust = .7, hjust = 0) +
  geom_text(x = (1.855 - .855) / 2 + .855,
            y = .1 + 0.005,
            label = '0.019', size = signif_size, vjust = .7, hjust = 0) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, .19),
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
  ggsave(paste0('mental_health_significance_with_ps.png'), width=12, height=4, units='in', dpi=300)


# DESCRIPTIVE STATISTICS: GALLUP DEMOGRAPHICS =====

# Calculate various descriptive statistics for each demographic group, which are used for Table S1.

# Reset \code{indiv} variable to include all data included in our most inclusive Gallup plot (the timeline).
indiv = individual_sadness %>%
  inner_join(individual_anger %>% select(endDate_asDate, floyd_weekOrNot) %>% distinct()) %>%
  select(EMPLOYEE_KEY_VALUE, DEMO_RACE_NAME, party_name, 
         DEMO_AGE_BINS2, DEMO_EDUCATION_2017, DEMO_INCOME_2020_name, 
         DEMO_GENDER_NAME, endDate_asDate, WEC_sadF, endDate_week_delta, FIPS,
         floyd_weekOrNot, WEIGHT) %>%
  mutate(DEMO_EDUCATION_2017 = ifelse(DEMO_EDUCATION_2017 > 6, NA, 
                                      ifelse(DEMO_EDUCATION_2017 < 4, '< College', 
                                             ifelse(DEMO_EDUCATION_2017 == 4 | DEMO_EDUCATION_2017 == 5, 'College', 
                                                    ifelse(DEMO_EDUCATION_2017 == 6, '> College', DEMO_EDUCATION_2017)))))
indiv = indiv %>% full_join(individual_anger %>%
                              select(EMPLOYEE_KEY_VALUE, DEMO_RACE_NAME, party_name, 
                                     DEMO_AGE_BINS2, DEMO_EDUCATION_2017, DEMO_INCOME_2020_name, 
                                     DEMO_GENDER_NAME, endDate_asDate, WEE_angerF, endDate_week_delta, FIPS,
                                     floyd_weekOrNot, WEIGHT) %>%
                              mutate(DEMO_EDUCATION_2017 = ifelse(DEMO_EDUCATION_2017 > 6, NA, 
                                                                  ifelse(DEMO_EDUCATION_2017 < 4, '< College', 
                                                                         ifelse(DEMO_EDUCATION_2017 == 4 | DEMO_EDUCATION_2017 == 5, 'College', 
                                                                                ifelse(DEMO_EDUCATION_2017 == 6, '> College', DEMO_EDUCATION_2017))))))

# N of each demographic category.
descriptor_n = indiv %>%
  select(-FIPS, -endDate_asDate) %>%
  gather('descriptor', 'value', -EMPLOYEE_KEY_VALUE) %>%
  group_by(descriptor, value) %>%
  summarize(n = n_distinct(EMPLOYEE_KEY_VALUE))

# Proportion and standard error of each demographic category. Note these are unweighted since Gallup weights
# do not apply to subgroups.
anger_sad_summary = indiv %>%
  select(-EMPLOYEE_KEY_VALUE, -FIPS, -endDate_asDate) %>%
  gather('descriptor', 'value', -WEE_angerF, -WEC_sadF) %>%
  group_by(descriptor, value) %>%
  summarize_at(vars(WEE_angerF, WEC_sadF), .funs = list(~mean(., na.rm = T), 
                                                        se = ~sqrt(mean(., na.rm = T) * (1 - mean(., na.rm = T)) / n())))


# DESCRIPTIVE STATISTICS: GALLUP DEMOGRAPHICS BEFORE AND AFTER FLOYD'S MURDER =====

# Calculate various descriptive statistics for each demographic group pre- and post-Floyd killing, which are used for Table S2.

# N, proportion, and standard error of each demographic category before and after Floyd's murder.
# Note these are unweighted since Gallup weights do not apply to subgroups.
desc_stats = indiv %>%
  select(DEMO_AGE_BINS2, DEMO_GENDER_NAME, DEMO_EDUCATION_2017,
         DEMO_INCOME_2020_name, DEMO_RACE_NAME, party_name,
         WEE_angerF, WEC_sadF, floyd_weekOrNot) %>%
  filter(!is.na(floyd_weekOrNot)) %>%
  gather('descriptor', 'value', -WEE_angerF, -WEC_sadF, -floyd_weekOrNot) %>%
  group_by(floyd_weekOrNot, value) %>%
  summarize(across(c(WEE_angerF, WEC_sadF), .fns = c(perc = ~mean(., na.rm = T),
                                                     se = ~sqrt(mean(., na.rm = T) * (1 - mean(., na.rm = T)) / n()), 
                                                     n = ~n()))) %>%
  ungroup() %>%
  gather('d', 'v', -value, -floyd_weekOrNot) %>%
  spread(floyd_weekOrNot, v) %>%
  mutate(change = `1` - `0`)

# N, proportion, and standard error across all demographic categories before and after Floyd's murder.
desc_stats_overall = indiv %>%
  select(DEMO_AGE_BINS2, DEMO_GENDER_NAME, DEMO_EDUCATION_2017,
         DEMO_INCOME_2020_name, DEMO_RACE_NAME, party_name,
         WEE_angerF, WEC_sadF, floyd_weekOrNot) %>%
  filter(!is.na(floyd_weekOrNot)) %>%
  group_by(floyd_weekOrNot) %>%
  summarize(across(c(WEE_angerF, WEC_sadF), .fns = c(perc = ~mean(., na.rm = T),
                                                     se = ~sqrt(mean(., na.rm = T) * (1 - mean(., na.rm = T)) / n()), 
                                                     n = ~n()))) %>%
  ungroup() %>%
  gather('d', 'v', -floyd_weekOrNot) %>%
  spread(floyd_weekOrNot, v) %>%
  mutate(change = `1` - `0`)
 
# z-statistics and p-values for pre- and post-Floyd proportion comparisons for each demographic category.
zs = indiv %>% 
  select(DEMO_AGE_BINS2, DEMO_GENDER_NAME, DEMO_EDUCATION_2017,
         DEMO_INCOME_2020_name, DEMO_RACE_NAME, party_name,
         WEE_angerF, WEC_sadF, floyd_weekOrNot, endDate_asDate) %>%
  filter(!is.na(floyd_weekOrNot)) %>%
  mutate(wk = floor_date(as.Date(endDate_asDate), 'week', week_start = 1)) %>% 
  select(-endDate_asDate) %>%
  gather('descriptor', 'value', -WEE_angerF, -WEC_sadF, -floyd_weekOrNot, -wk) %>%
  group_by(value) %>% 
  summarize(WEE_angerF_z = sqrt(prop.test(table(WEE_angerF, floyd_weekOrNot))$statistic),
            WEE_angerF_p = prop.test(table(WEE_angerF, floyd_weekOrNot))$p.value,
            WEC_sadF_z = sqrt(prop.test(table(WEC_sadF, floyd_weekOrNot))$statistic),
            WEC_sadF_p = prop.test(table(WEC_sadF, floyd_weekOrNot))$p.value) %>% 
  ungroup()

# z-statistics and p-values for pre- and post-Floyd comparisons across all demographic categories.
overall_z = indiv %>% 
  select(DEMO_AGE_BINS2, DEMO_GENDER_NAME, DEMO_EDUCATION_2017,
         DEMO_INCOME_2020_name, DEMO_RACE_NAME, party_name,
         WEE_angerF, WEC_sadF, floyd_weekOrNot, endDate_asDate) %>%
  filter(!is.na(floyd_weekOrNot)) %>%
  mutate(wk = floor_date(as.Date(endDate_asDate), 'week', week_start = 1)) %>% 
  select(-endDate_asDate) %>%
  summarize(WEE_angerF_z = sqrt(prop.test(table(WEE_angerF, floyd_weekOrNot))$statistic),
            WEE_angerF_p = prop.test(table(WEE_angerF, floyd_weekOrNot))$p.value,
            WEC_sadF_z = sqrt(prop.test(table(WEC_sadF, floyd_weekOrNot))$statistic),
            WEC_sadF_p = prop.test(table(WEC_sadF, floyd_weekOrNot))$p.value)


# DESCRIPTIVE STATISTICS: GALLUP GEOGRAPHIC BEFORE AND AFTER FLOYD'S MURDER =====

# Calculate various descriptive statistics for each geographic region pre- and 
# post-Floyd killing, which are used for Table S3.

# Some of the required data frames are calculated above (e.g., \code{angry_sad_mn_city}, \code{angry_sad_mn}).
# Others are not.

# Calculate N...
# ...for MN vs. Other states
anger_sad_state_ns = anger_sad_state %>%
  mutate(is_MN = if_else(state == 'MN', 'MN', 'Other'),
         is_Minneapolis = if_else(FIPS == '27053', 'Minneapolis', 
                                  if_else(state == 'MN', 'Not Minneapolis', 'Other state'))) %>%
  select(floyd_weekOrNot, WEC_sadF, WEE_angerF, is_MN, is_Minneapolis) %>%
  group_by(floyd_weekOrNot, is_MN) %>%
  summarize(WEE_angerF_n = sum(!is.na(WEE_angerF)),
            WEC_sadF_n = sum(!is.na(WEC_sadF))) %>%
  ungroup()
# ...for Minneapolis vs. rest of MN vs. Other states
anger_sad_state_city_ns = anger_sad_state %>%
  mutate(is_MN = if_else(FIPS == '27053', 'Minneapolis', 
                                  if_else(state == 'MN', 'Not Minneapolis', 'Other state'))) %>%
  select(floyd_weekOrNot, WEC_sadF, WEE_angerF, is_MN) %>%
  group_by(floyd_weekOrNot, is_MN) %>%
  summarize(WEE_angerF_n = sum(!is.na(WEE_angerF)),
            WEC_sadF_n = sum(!is.na(WEC_sadF))) %>%
  ungroup()
# ...overall
anger_sad_state_overall_ns = indiv %>%
  select(floyd_weekOrNot, WEC_sadF, WEE_angerF) %>%
  mutate(is_MN = 'Overall') %>%
  group_by(floyd_weekOrNot, is_MN) %>%
  summarize(WEE_angerF_n = sum(!is.na(WEE_angerF)),
            WEC_sadF_n = sum(!is.na(WEC_sadF))) %>%
  ungroup()
# ...and combine
anger_sad_geo_ns = bind_rows(anger_sad_state_ns, anger_sad_state_city_ns, anger_sad_state_overall_ns) %>%
  gather('descriptor', 'v', WEE_angerF_n, WEC_sadF_n) %>%
  mutate(descriptor = if_else(str_detect(descriptor, 'anger'), 'Anger', 'Sadness')) %>%
  spread(floyd_weekOrNot, v) %>%
  rename(`not_floyd_n` = `0`, `floyd_n` = `1`, `value` = is_MN)

# Calculate z-statistics and p-values...
# ...for Minneapolis and other states
anger_sad_state_city_zs = anger_sad_state %>%
  mutate(is_MN = if_else(state == 'MN', 'MN', 'Other'),
         is_Minneapolis = if_else(FIPS == '27053', 'Minneapolis', if_else(state == 'MN', 'Not Minneapolis', 'Other state'))) %>%
  select(floyd_weekOrNot, WEC_sadF, WEE_angerF, is_MN, is_Minneapolis) %>%
  mutate(descriptor = 'is_Minneapolis', value = is_Minneapolis) %>% 
  filter(is_Minneapolis != 'Not Minneapolis') %>%
  group_by(floyd_weekOrNot, descriptor, value) %>%
  mutate(WEE_angerF_n = sum(!is.na(WEE_angerF)),
         WEC_sadF_n = sum(!is.na(WEC_sadF))) %>%
  group_by(descriptor, value) %>%
  summarize(WEE_angerF_z = sqrt(prop.test(table(WEE_angerF, floyd_weekOrNot))$statistic),
            WEE_angerF_p = prop.test(table(WEE_angerF, floyd_weekOrNot))$p.value,
            WEC_sadF_z = sqrt(prop.test(table(WEC_sadF, floyd_weekOrNot))$statistic),
            WEC_sadF_p = prop.test(table(WEC_sadF, floyd_weekOrNot))$p.value) %>%
  ungroup() %>%
  select(-descriptor) %>%
  gather('k', 'v', -value) %>%
  mutate(descriptor = case_when(
    k == 'WEE_angerF_z' ~ 'Anger',
    k == 'WEE_angerF_p' ~ 'Anger',
    k == 'WEE_angerF_n' ~ 'Anger',
    k == 'WEC_sadF_z' ~ 'Sadness',
    k == 'WEC_sadF_p' ~ 'Sadness',
    k == 'WEC_sadF_n' ~ 'Sadness'
  ), n = case_when(
    k == 'WEE_angerF_z' ~ 'z',
    k == 'WEE_angerF_p' ~ 'p',
    k == 'WEE_angerF_n' ~ 'n',
    k == 'WEC_sadF_z' ~ 'z',
    k == 'WEC_sadF_p' ~ 'p',
    k == 'WEC_sadF_n' ~ 'n'
  )) %>%
  select(-k) %>%
  spread(n, v)
# ...for Minnesota
anger_sad_state_city_zs = anger_sad_state %>%
  mutate(is_MN = if_else(state == 'MN', 'MN', 'Other'),
         is_Minneapolis = if_else(FIPS == '27053', 'Minneapolis', if_else(state == 'MN', 'Not Minneapolis', 'Other state'))) %>%
  select(floyd_weekOrNot, WEC_sadF, WEE_angerF, is_MN, is_Minneapolis) %>%
  mutate(descriptor = 'is_Minneapolis', value = is_MN) %>% 
  filter(is_MN == 'MN') %>%
  group_by(floyd_weekOrNot, descriptor, value) %>%
  mutate(WEE_angerF_n = sum(!is.na(WEE_angerF)),
         WEC_sadF_n = sum(!is.na(WEC_sadF))) %>%
  group_by(descriptor, value) %>%
  summarize(WEE_angerF_z = sqrt(prop.test(table(WEE_angerF, floyd_weekOrNot))$statistic),
            WEE_angerF_p = prop.test(table(WEE_angerF, floyd_weekOrNot))$p.value,
            WEC_sadF_z = sqrt(prop.test(table(WEC_sadF, floyd_weekOrNot))$statistic),
            WEC_sadF_p = prop.test(table(WEC_sadF, floyd_weekOrNot))$p.value) %>%
  ungroup() %>%
  select(-descriptor) %>%
  gather('k', 'v', -value) %>%
  mutate(descriptor = case_when(
    k == 'WEE_angerF_z' ~ 'Anger',
    k == 'WEE_angerF_p' ~ 'Anger',
    k == 'WEE_angerF_n' ~ 'Anger',
    k == 'WEC_sadF_z' ~ 'Sadness',
    k == 'WEC_sadF_p' ~ 'Sadness',
    k == 'WEC_sadF_n' ~ 'Sadness'
  ), n = case_when(
    k == 'WEE_angerF_z' ~ 'z',
    k == 'WEE_angerF_p' ~ 'p',
    k == 'WEE_angerF_n' ~ 'n',
    k == 'WEC_sadF_z' ~ 'z',
    k == 'WEC_sadF_p' ~ 'p',
    k == 'WEC_sadF_n' ~ 'n'
  )) %>%
  select(-k) %>%
  spread(n, v) %>%
  bind_rows(anger_sad_state_city_zs)
# ...overall
anger_sad_state_city_zs = anger_sad_state %>%
  select(floyd_weekOrNot, WEC_sadF, WEE_angerF) %>%
  mutate(descriptor = 'is_Minneapolis', value = 'Overall') %>% 
  group_by(floyd_weekOrNot, descriptor, value) %>%
  mutate(WEE_angerF_n = sum(!is.na(WEE_angerF)),
         WEC_sadF_n = sum(!is.na(WEC_sadF))) %>%
  group_by(descriptor, value) %>%
  summarize(WEE_angerF_z = sqrt(prop.test(table(WEE_angerF, floyd_weekOrNot))$statistic),
            WEE_angerF_p = prop.test(table(WEE_angerF, floyd_weekOrNot))$p.value,
            WEC_sadF_z = sqrt(prop.test(table(WEC_sadF, floyd_weekOrNot))$statistic),
            WEC_sadF_p = prop.test(table(WEC_sadF, floyd_weekOrNot))$p.value) %>%
  ungroup() %>%
  select(-descriptor) %>%
  gather('k', 'v', -value) %>%
  mutate(descriptor = case_when(
    k == 'WEE_angerF_z' ~ 'Anger',
    k == 'WEE_angerF_p' ~ 'Anger',
    k == 'WEE_angerF_n' ~ 'Anger',
    k == 'WEC_sadF_z' ~ 'Sadness',
    k == 'WEC_sadF_p' ~ 'Sadness',
    k == 'WEC_sadF_n' ~ 'Sadness'
  ), n = case_when(
    k == 'WEE_angerF_z' ~ 'z',
    k == 'WEE_angerF_p' ~ 'p',
    k == 'WEE_angerF_n' ~ 'n',
    k == 'WEC_sadF_z' ~ 'z',
    k == 'WEC_sadF_p' ~ 'p',
    k == 'WEC_sadF_n' ~ 'n'
  )) %>%
  select(-k) %>%
  spread(n, v) %>%
  bind_rows(anger_sad_state_city_zs)

# Calculate overall proportion and standard error before and after Floyd's murder...
# ...for sadness
sad_mn_city_overall = anger_sad_state %>%
  select(floyd_weekOrNot, WEC_sadF, WEE_angerF) %>%
  mutate(descriptor = 'is_Minneapolis', value = 'Overall') %>%
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
  filter(floyd == 'not_floyd')
# ...for anger
angry_mn_city_overall = anger_sad_state %>%
  select(floyd_weekOrNot, WEC_sadF, WEE_angerF) %>%
  mutate(descriptor = 'is_Minneapolis', value = 'Overall') %>%
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
  filter(floyd == 'not_floyd')
# ...and combine
angry_sad_mn_city_overall = angry_mn_city_overall %>%
  mutate(descriptor = 'Anger') %>%
  bind_rows(sad_mn_city_overall) %>%
  mutate(descriptor = if_else(descriptor == 'Anger', descriptor, 'Sadness'))

# Combines several distinct data frames to produce a single coherent descriptive stats table.
angry_sad_mn_city %>%
  filter(value != 'Not Minneapolis') %>%
  bind_rows(angry_sad_mn %>% filter(value != 'Other')) %>%
  bind_rows(angry_sad_mn_city_overall) %>%
  inner_join(anger_sad_geo_ns) %>%
  select(-floyd, -WEE_angerF_sd, -WEC_sadF_sd) %>%
  mutate(change = floyd_mean - not_floyd_mean) %>% 
  inner_join(anger_sad_state_city_zs) %>%
  mutate(across(c(floyd_mean, not_floyd_mean, floyd_se, not_floyd_se, change, z, p),
         ~round(., digits = 3))) %>%
  mutate(not_floyd = paste0(not_floyd_mean, ' (', not_floyd_se, ')'),
         floyd = paste0(floyd_mean, ' (', floyd_se, ')')) %>%
  select(descriptor, value, not_floyd, floyd, change, z, p, not_floyd_n, floyd_n) %>%
  rename(`% Pre-Floyd (SE)` = not_floyd, `% Floyd (SE)` = floyd, `Change` = change,
         `N Pre-Floyd` = not_floyd_n, `N Floyd` = floyd_n) %>%
  arrange(descriptor, value)


# DESCRIPTIVE STATISTICS: CENSUS DEMOGRAPHICS =====

# Calculate various descriptive statistics for census demographics, which are used for Table S4.

# Recode income, age, and education
pulse = pulse %>%
  mutate(edu = if_else(EEDUC < 4, '< College',
                       if_else(EEDUC > 6, '> College', 'College')),
         age = if_else(age_in_years < 31, '19-30',
                       if_else(age_in_years > 45, '46+', '31-45')),
         income = if_else(INCOME < 4, '$0-$74,999',
                          if_else(INCOME > 6, '$150,000+', '$75,000-$149,999')))

# Make a single race identifier variable
pulse = pulse %>%
  mutate(race = case_when(
    is_black == 1 & is_hispanic == 0 ~ 'Black',
    is_white == 1 & is_hispanic == 0 ~ 'White',
    is_asian == 1 ~ 'Asian',
    is_hispanic == 1 ~ 'Hispanic'
  ))

# Calculate weighted mean, weighted standard error, and N for depression and anxiety
# symptom severity in each demographic category.
s1_severity = pulse %>%
  select(SCRAM, gad2_sum, phq2_sum, race, is_female, age, edu, income, floyd_weekOrNot, PWEIGHT) %>%
  gather('descriptor', 'value', race, is_female, age, edu, income) %>%
  group_by(descriptor, value) %>%
  summarize(across(.cols = c(gad2_sum, phq2_sum),
                   .fns = list(mean = ~weighted.mean(., PWEIGHT, na.rm = T),
                               se = ~diagis::weighted_se(., PWEIGHT, na.rm = T),
                               n = ~n_distinct(SCRAM[!is.na(.)])
                               )
                   )
            ) %>%
  ungroup()

# Same as above, but using thresholds to categorize participants as depressed or not and anxious or not
# before calculating weighted proportion, weighted standard error, and N.
s1_props = pulse %>%
  select(SCRAM, gad2_sum, phq2_sum, race, is_female, age, edu, income, floyd_weekOrNot, PWEIGHT) %>%
  gather('descriptor', 'value', race, is_female, age, edu, income) %>%
  mutate(is_dep = phq2_sum > 2, is_anx = gad2_sum > 2) %>%
  group_by(descriptor, value) %>%
  summarize(across(.cols = c(is_dep, is_anx),
                   .fns = list(mean = ~weighted.mean(., PWEIGHT, na.rm = T),
                               se = ~weighted_se(., PWEIGHT, na.rm = T),
                               n = ~n_distinct(SCRAM[!is.na(.)])
                               )
                   )
        ) %>%
  ungroup() %>%
  mutate(across(c(is_dep_mean, is_dep_se, is_anx_mean, is_anx_se), ~(. * 100)))

census_s1 = s1_severity %>%
  inner_join(s1_props)

# Clean up variable names
census_s1 = census_s1 %>%
  mutate(descriptor = case_when(
    descriptor == 'age' ~ 'Age',
    descriptor == 'edu' ~ 'Education',
    descriptor == 'income' ~ 'Income',
    descriptor == 'is_female' ~ 'Gender',
    descriptor == 'race' ~ 'Race'
  ),
  descriptor = factor(descriptor, levels = c('Race', 'Gender', 'Age', 'Education', 'Income'))) %>%
  arrange(descriptor) %>%
  drop_na() %>%
  mutate(value = if_else(value == 0, 'Male', 
                         if_else(value == 1, 'Female', value))) %>%
  mutate(across(c(-descriptor, -value), .fns = ~round(., 3)))
census_s1

# Repeat all of the above, but calculating overall statistics (across all demographic categories).
s1_severity = pulse %>%
  select(SCRAM, gad2_sum, phq2_sum, race, is_female, age, edu, income, floyd_weekOrNot, PWEIGHT) %>%
  mutate(descriptor = 'Overall', value = 'Overall',
         is_dep = phq2_sum > 2, is_anx = gad2_sum > 2) %>%
  group_by(descriptor, value) %>%
  summarize(across(.cols = c(gad2_sum, phq2_sum),
                   .fns = list(mean = ~weighted.mean(., PWEIGHT, na.rm = T),
                               se = ~diagis::weighted_se(., PWEIGHT, na.rm = T),
                               n = ~n_distinct(SCRAM[!is.na(.)])
                   )
  )
  ) %>%
  ungroup()
s1_props = pulse %>%
  select(SCRAM, gad2_sum, phq2_sum, race, is_female, age, edu, income, floyd_weekOrNot, PWEIGHT) %>%
  mutate(descriptor = 'Overall', value = 'Overall',
         is_dep = phq2_sum > 2, is_anx = gad2_sum > 2) %>%
  group_by(descriptor, value) %>%
  summarize(across(.cols = c(is_dep, is_anx),
                   .fns = list(mean = ~weighted.mean(., PWEIGHT, na.rm = T),
                               se = ~weighted_se(., PWEIGHT, na.rm = T),
                               n = ~n_distinct(SCRAM[!is.na(.)])
                   )
  )
  ) %>%
  ungroup() %>%
  mutate(across(c(is_dep_mean, is_dep_se, is_anx_mean, is_anx_se), ~(. * 100)))
census_s1_overall = s1_severity %>%
  inner_join(s1_props)
census_s1_overall = census_s1_overall %>%
  arrange(descriptor) %>%
  drop_na() %>%
  mutate(value = if_else(value == 0, 'Male', 
                         if_else(value == 1, 'Female', value))) %>%
  mutate(across(c(-descriptor, -value), .fns = ~round(., 3)))
census_s1_overall


# DESCRIPTIVE STATISTICS: CENSUS DEMOGRAPHICS SYMPTOM SEVERITY BEFORE AND AFTER FLOYD'S MURDER =====

# Function to get value from weighted t-test
get_from_ttest = function(ttest, val) {
  val = if_else(val == 't', 't.value',
                if_else(val == 'p', 'p.value', val))
  ttest$coefficients[val]
}

# The following several blocks of code calculate the weighted mean, weighted standard error, and N
# for each of PHQ2 and GAD for each demographic category before and after Floyd's murder. These 
# will ultimately be combined at the end.
s3_severity_gad_means = pulse %>%
  select(gad2_sum, phq2_sum, race, is_female, age, edu, income, floyd_weekOrNot, PWEIGHT) %>%
  gather('descriptor', 'value', race, is_female, age, edu, income) %>%
  mutate(is_dep = phq2_sum > 2, is_anx = gad2_sum > 2) %>%
  group_by(descriptor, value, floyd_weekOrNot) %>%
  summarize(across(.cols = c(gad2_sum, phq2_sum),
                   .fns = list(mean = ~weighted.mean(., PWEIGHT, na.rm = T),
                               se = ~diagis::weighted_se(., PWEIGHT, na.rm = T),
                               n = ~sum(!is.na(.))
                   )
  )
  ) %>%
  ungroup() %>%
  select(descriptor, value, floyd_weekOrNot, gad2_sum_mean) %>%
  spread(floyd_weekOrNot, gad2_sum_mean) %>%
  rename(pre_floyd_gad = `0`, floyd_gad = `1`) %>%
  mutate(change_gad = floyd_gad - pre_floyd_gad) %>%
  drop_na()
s3_severity_gad_ses = pulse %>%
  select(gad2_sum, phq2_sum, race, is_female, age, edu, income, floyd_weekOrNot, PWEIGHT) %>%
  gather('descriptor', 'value', race, is_female, age, edu, income) %>%
  mutate(is_dep = phq2_sum > 2, is_anx = gad2_sum > 2) %>%
  group_by(descriptor, value, floyd_weekOrNot) %>%
  summarize(across(.cols = c(gad2_sum, phq2_sum),
                   .fns = list(mean = ~weighted.mean(., PWEIGHT, na.rm = T),
                               se = ~diagis::weighted_se(., PWEIGHT, na.rm = T),
                               n = ~sum(!is.na(.))
                   )
  )
  ) %>%
  ungroup() %>%
  select(descriptor, value, floyd_weekOrNot, gad2_sum_se) %>%
  spread(floyd_weekOrNot, gad2_sum_se) %>%
  rename(pre_floyd_gad_se = `0`, floyd_gad_se = `1`) %>%
  drop_na()
s3_severity_phq_means = pulse %>%
  select(gad2_sum, phq2_sum, race, is_female, age, edu, income, floyd_weekOrNot, PWEIGHT) %>%
  gather('descriptor', 'value', race, is_female, age, edu, income) %>%
  mutate(is_dep = phq2_sum > 2, is_anx = gad2_sum > 2) %>%
  group_by(descriptor, value, floyd_weekOrNot) %>%
  summarize(across(.cols = c(gad2_sum, phq2_sum),
                   .fns = list(mean = ~weighted.mean(., PWEIGHT, na.rm = T),
                               se = ~diagis::weighted_se(., PWEIGHT, na.rm = T),
                               n = ~sum(!is.na(.))
                   )
  )
  ) %>%
  ungroup() %>%
  select(descriptor, value, floyd_weekOrNot, phq2_sum_mean) %>%
  spread(floyd_weekOrNot, phq2_sum_mean) %>%
  rename(pre_floyd_phq = `0`, floyd_phq = `1`) %>%
  mutate(change_phq = floyd_phq - pre_floyd_phq) %>%
  drop_na()
s3_severity_phq_ses = pulse %>%
  select(gad2_sum, phq2_sum, race, is_female, age, edu, income, floyd_weekOrNot, PWEIGHT) %>%
  gather('descriptor', 'value', race, is_female, age, edu, income) %>%
  mutate(is_dep = phq2_sum > 2, is_anx = gad2_sum > 2) %>%
  group_by(descriptor, value, floyd_weekOrNot) %>%
  summarize(across(.cols = c(gad2_sum, phq2_sum),
                   .fns = list(mean = ~weighted.mean(., PWEIGHT, na.rm = T),
                               se = ~diagis::weighted_se(., PWEIGHT, na.rm = T),
                               n = ~sum(!is.na(.))
                   )
  )
  ) %>%
  ungroup() %>%
  select(descriptor, value, floyd_weekOrNot, phq2_sum_se) %>%
  spread(floyd_weekOrNot, phq2_sum_se) %>%
  rename(pre_floyd_phq_se = `0`, floyd_phq_se = `1`) %>%
  drop_na()

# Like the above, but calculating pre- and post-Floyd weighted Cohen's d values.
s3_severity_gad_d_n = pulse %>%
  select(gad2_sum, phq2_sum, race, is_female, age, edu, income, floyd_weekOrNot, PWEIGHT) %>%
  gather('descriptor', 'value', race, is_female, age, edu, income) %>%
  filter(!is.na(value)) %>%
  group_by(descriptor, value) %>%
  summarize(weighted_cohens_d(gad2_sum, floyd_weekOrNot, PWEIGHT)) %>%
  ungroup() %>%
  rename(floyd_n_gad = n1, pre_floyd_n_gad = n2, d_gad = d) %>%
  select(-se)
s3_severity_phq_d_n = pulse %>%
  select(gad2_sum, phq2_sum, race, is_female, age, edu, income, floyd_weekOrNot, PWEIGHT) %>%
  gather('descriptor', 'value', race, is_female, age, edu, income) %>%
  filter(!is.na(value)) %>%
  group_by(descriptor, value) %>%
  summarize(weighted_cohens_d(phq2_sum, floyd_weekOrNot, PWEIGHT)) %>%
  ungroup() %>%
  rename(floyd_n_phq = n1, pre_floyd_n_phq = n2, d_phq = d) %>%
  select(-se)

# Like the above, but calculating weighted t-tests comparing pre- and post-Floyd symptom severity
# and collecting the t statistic, degrees of freedom, and p-value.
s3_severity_gad_p = pulse %>%
  select(gad2_sum, phq2_sum, race, is_female, age, edu, income, floyd_weekOrNot, PWEIGHT) %>%
  gather('descriptor', 'value', race, is_female, age, edu, income) %>%
  filter(!is.na(value)) %>%
  group_by(descriptor, value) %>%
  summarize(ttest = weights::wtd.t.test(gad2_sum[floyd_weekOrNot == 0],
                                        gad2_sum[floyd_weekOrNot == 1],
                                        PWEIGHT[floyd_weekOrNot == 0],
                                        PWEIGHT[floyd_weekOrNot == 1]),
            t_gad = get_from_ttest(ttest, 't'),
            df_gad = get_from_ttest(ttest, 'df'),
            p_gad = get_from_ttest(ttest, 'p')) %>%
  ungroup() %>%
  select(-ttest) %>%
  distinct()
s3_severity_phq_p = pulse %>%
  select(gad2_sum, phq2_sum, race, is_female, age, edu, income, floyd_weekOrNot, PWEIGHT) %>%
  gather('descriptor', 'value', race, is_female, age, edu, income) %>%
  filter(!is.na(value)) %>%
  group_by(descriptor, value) %>%
  summarize(ttest = weights::wtd.t.test(phq2_sum[floyd_weekOrNot == 0],
                                        phq2_sum[floyd_weekOrNot == 1],
                                        PWEIGHT[floyd_weekOrNot == 0],
                                        PWEIGHT[floyd_weekOrNot == 1]),
            t_phq = get_from_ttest(ttest, 't'),
            df_phq = get_from_ttest(ttest, 'df'),
            p_phq = get_from_ttest(ttest, 'p')) %>%
  ungroup() %>%
  select(-ttest) %>%
  distinct()

# Combining all of the above into a single table, and cleaning up variable names.
s3_severity_table = s3_severity_gad_means %>%
  inner_join(s3_severity_gad_ses) %>%
  inner_join(s3_severity_gad_d_n) %>%
  inner_join(s3_severity_gad_p) %>%
  inner_join(s3_severity_phq_means) %>%
  inner_join(s3_severity_phq_ses) %>%
  inner_join(s3_severity_phq_d_n) %>%
  inner_join(s3_severity_phq_p) %>%
  select(descriptor, value, 
         pre_floyd_gad, pre_floyd_gad_se, floyd_gad, floyd_gad_se, change_gad, d_gad, t_gad, df_gad, p_gad, pre_floyd_n_gad, floyd_n_gad,
         pre_floyd_phq, pre_floyd_phq_se, floyd_phq, floyd_phq_se, change_phq, d_phq, t_phq, df_phq, p_phq, pre_floyd_n_phq, floyd_n_phq) %>%
  mutate(descriptor = case_when(
    descriptor == 'age' ~ 'Age',
    descriptor == 'edu' ~ 'Education',
    descriptor == 'income' ~ 'Income',
    descriptor == 'is_female' ~ 'Gender',
    descriptor == 'race' ~ 'Race'
  ),
  descriptor = factor(descriptor, levels = c('Race', 'Gender', 'Age', 'Education', 'Income'))) %>%
  arrange(descriptor) %>%
  drop_na() %>%
  mutate(value = if_else(value == 0, 'Male', 
                         if_else(value == 1, 'Female', value))) %>%
  mutate(across(c(-descriptor, -value), .fns = ~round(., 3)))
s3_severity_table
  
# Now do all of the above all over again, but this time calculating overall values (across all demographic categories).
s3_severity_gad_means = pulse %>%
  select(gad2_sum, phq2_sum, floyd_weekOrNot, PWEIGHT) %>%
  mutate(descriptor = 'Overall', value = 'Overall',
         is_dep = phq2_sum > 2, is_anx = gad2_sum > 2) %>%
  group_by(descriptor, value, floyd_weekOrNot) %>%
  summarize(across(.cols = c(gad2_sum, phq2_sum),
                   .fns = list(mean = ~weighted.mean(., PWEIGHT, na.rm = T),
                               se = ~diagis::weighted_se(., PWEIGHT, na.rm = T),
                               n = ~sum(!is.na(.))
                   )
  )
  ) %>%
  ungroup() %>%
  select(descriptor, value, floyd_weekOrNot, gad2_sum_mean) %>%
  spread(floyd_weekOrNot, gad2_sum_mean) %>%
  rename(pre_floyd_gad = `0`, floyd_gad = `1`) %>%
  mutate(change_gad = floyd_gad - pre_floyd_gad) %>%
  drop_na()
s3_severity_gad_ses = pulse %>%
  select(gad2_sum, phq2_sum, race, is_female, age, edu, income, floyd_weekOrNot, PWEIGHT) %>%
  mutate(descriptor = 'Overall', value = 'Overall',
         is_dep = phq2_sum > 2, is_anx = gad2_sum > 2) %>%
  group_by(descriptor, value, floyd_weekOrNot) %>%
  summarize(across(.cols = c(gad2_sum, phq2_sum),
                   .fns = list(mean = ~weighted.mean(., PWEIGHT, na.rm = T),
                               se = ~diagis::weighted_se(., PWEIGHT, na.rm = T),
                               n = ~sum(!is.na(.))
                   )
  )
  ) %>%
  ungroup() %>%
  select(descriptor, value, floyd_weekOrNot, gad2_sum_se) %>%
  spread(floyd_weekOrNot, gad2_sum_se) %>%
  rename(pre_floyd_gad_se = `0`, floyd_gad_se = `1`) %>%
  drop_na()
s3_severity_phq_means = pulse %>%
  select(gad2_sum, phq2_sum, race, is_female, age, edu, income, floyd_weekOrNot, PWEIGHT) %>%
  mutate(descriptor = 'Overall', value = 'Overall',
         is_dep = phq2_sum > 2, is_anx = gad2_sum > 2) %>%
  group_by(descriptor, value, floyd_weekOrNot) %>%
  summarize(across(.cols = c(gad2_sum, phq2_sum),
                   .fns = list(mean = ~weighted.mean(., PWEIGHT, na.rm = T),
                               se = ~diagis::weighted_se(., PWEIGHT, na.rm = T),
                               n = ~sum(!is.na(.))
                   )
  )
  ) %>%
  ungroup() %>%
  select(descriptor, value, floyd_weekOrNot, phq2_sum_mean) %>%
  spread(floyd_weekOrNot, phq2_sum_mean) %>%
  rename(pre_floyd_phq = `0`, floyd_phq = `1`) %>%
  mutate(change_phq = floyd_phq - pre_floyd_phq) %>%
  drop_na()
s3_severity_phq_ses = pulse %>%
  select(gad2_sum, phq2_sum, race, is_female, age, edu, income, floyd_weekOrNot, PWEIGHT) %>%
  mutate(descriptor = 'Overall', value = 'Overall',
         is_dep = phq2_sum > 2, is_anx = gad2_sum > 2) %>%
  group_by(descriptor, value, floyd_weekOrNot) %>%
  summarize(across(.cols = c(gad2_sum, phq2_sum),
                   .fns = list(mean = ~weighted.mean(., PWEIGHT, na.rm = T),
                               se = ~diagis::weighted_se(., PWEIGHT, na.rm = T),
                               n = ~sum(!is.na(.))
                   )
  )
  ) %>%
  ungroup() %>%
  select(descriptor, value, floyd_weekOrNot, phq2_sum_se) %>%
  spread(floyd_weekOrNot, phq2_sum_se) %>%
  rename(pre_floyd_phq_se = `0`, floyd_phq_se = `1`) %>%
  drop_na()
s3_severity_gad_d_n = pulse %>%
  select(gad2_sum, phq2_sum, race, is_female, age, edu, income, floyd_weekOrNot, PWEIGHT) %>%
  mutate(descriptor = 'Overall', value = 'Overall') %>%
  filter(!is.na(value)) %>%
  group_by(descriptor, value) %>%
  summarize(weighted_cohens_d(gad2_sum, floyd_weekOrNot, PWEIGHT)) %>%
  ungroup() %>%
  rename(floyd_n_gad = n1, pre_floyd_n_gad = n2, d_gad = d) %>%
  select(-se)
s3_severity_phq_d_n = pulse %>%
  select(gad2_sum, phq2_sum, race, is_female, age, edu, income, floyd_weekOrNot, PWEIGHT) %>%
  mutate(descriptor = 'Overall', value = 'Overall') %>%
  group_by(descriptor, value) %>%
  summarize(weighted_cohens_d(phq2_sum, floyd_weekOrNot, PWEIGHT)) %>%
  ungroup() %>%
  rename(floyd_n_phq = n1, pre_floyd_n_phq = n2, d_phq = d) %>%
  select(-se)
s3_severity_gad_p = pulse %>%
  select(gad2_sum, phq2_sum, race, is_female, age, edu, income, floyd_weekOrNot, PWEIGHT) %>%
  mutate(descriptor = 'Overall', value = 'Overall') %>%
  filter(!is.na(value)) %>%
  group_by(descriptor, value) %>%
  summarize(ttest = weights::wtd.t.test(gad2_sum[floyd_weekOrNot == 0],
                                        gad2_sum[floyd_weekOrNot == 1],
                                        PWEIGHT[floyd_weekOrNot == 0],
                                        PWEIGHT[floyd_weekOrNot == 1]),
            t_gad = get_from_ttest(ttest, 't'),
            df_gad = get_from_ttest(ttest, 'df'),
            p_gad = get_from_ttest(ttest, 'p')) %>%
  ungroup() %>%
  select(-ttest) %>%
  distinct()
s3_severity_phq_p = pulse %>%
  select(gad2_sum, phq2_sum, race, is_female, age, edu, income, floyd_weekOrNot, PWEIGHT) %>%
  mutate(descriptor = 'Overall', value = 'Overall') %>%
  filter(!is.na(value)) %>%
  group_by(descriptor, value) %>%
  summarize(ttest = weights::wtd.t.test(phq2_sum[floyd_weekOrNot == 0],
                                        phq2_sum[floyd_weekOrNot == 1],
                                        PWEIGHT[floyd_weekOrNot == 0],
                                        PWEIGHT[floyd_weekOrNot == 1]),
            t_phq = get_from_ttest(ttest, 't'),
            df_phq = get_from_ttest(ttest, 'df'),
            p_phq = get_from_ttest(ttest, 'p')) %>%
  ungroup() %>%
  select(-ttest) %>%
  distinct()
s3_severity_table_overall = s3_severity_gad_means %>%
  inner_join(s3_severity_gad_ses) %>%
  inner_join(s3_severity_gad_d_n) %>%
  inner_join(s3_severity_gad_p) %>%
  inner_join(s3_severity_phq_means) %>%
  inner_join(s3_severity_phq_ses) %>%
  inner_join(s3_severity_phq_d_n) %>%
  inner_join(s3_severity_phq_p) %>%
  select(descriptor, value, 
         pre_floyd_gad, pre_floyd_gad_se, floyd_gad, floyd_gad_se, change_gad, d_gad, t_gad, df_gad, p_gad, pre_floyd_n_gad, floyd_n_gad,
         pre_floyd_phq, pre_floyd_phq_se, floyd_phq, floyd_phq_se, change_phq, d_phq, t_phq, df_phq, p_phq, pre_floyd_n_phq, floyd_n_phq) %>%
  arrange(descriptor) %>%
  drop_na() %>%
  mutate(value = if_else(value == 0, 'Male', 
                         if_else(value == 1, 'Female', value))) %>%
  mutate(across(c(-descriptor, -value), .fns = ~round(., 3)))
s3_severity_table_overall


# DESCRIPTIVE STATISTICS: CENSUS DEMOGRAPHICS SYMPTOM SEVERITY BEFORE AND AFTER FLOYD'S MURDER =====

# This section is almost identical to the previous section, except instead of symptom severity, we use thresholds
# to classify participants into binary depressed or not and anxious or not categories. There will be no comments
# for the following code; see the previous section for explanations. This data is used for Table S6.

s3_percent_gad_means = pulse %>%
  select(gad2_sum, phq2_sum, race, is_female, age, edu, income, floyd_weekOrNot, PWEIGHT) %>%
  gather('descriptor', 'value', race, is_female, age, edu, income) %>%
  mutate(is_dep = phq2_sum > 2, is_anx = gad2_sum > 2) %>%
  group_by(descriptor, value, floyd_weekOrNot) %>%
  summarize(across(.cols = c(is_dep, is_anx),
                   .fns = list(mean = ~weighted.mean(., PWEIGHT, na.rm = T),
                               n = ~sum(!is.na(.))
                   )
  )
  ) %>%
  ungroup() %>%
  select(descriptor, value, floyd_weekOrNot, is_anx_mean) %>%
  spread(floyd_weekOrNot, is_anx_mean) %>%
  rename(pre_floyd_gad = `0`, floyd_gad = `1`) %>%
  mutate(change_gad = floyd_gad - pre_floyd_gad) %>%
  drop_na()
s3_percent_gad_n = pulse %>%
  select(gad2_sum, phq2_sum, race, is_female, age, edu, income, floyd_weekOrNot, PWEIGHT) %>%
  gather('descriptor', 'value', race, is_female, age, edu, income) %>%
  mutate(is_dep = phq2_sum > 2, is_anx = gad2_sum > 2) %>%
  group_by(descriptor, value, floyd_weekOrNot) %>%
  summarize(across(.cols = c(is_dep, is_anx),
                   .fns = list(mean = ~weighted.mean(., PWEIGHT, na.rm = T),
                               n = ~sum(!is.na(.))
                   )
  )
  ) %>%
  ungroup() %>%
  select(descriptor, value, floyd_weekOrNot, is_anx_n) %>%
  spread(floyd_weekOrNot, is_anx_n) %>%
  rename(pre_floyd_gad_n = `0`, floyd_gad_n = `1`) %>%
  drop_na()
s3_percent_phq_means = pulse %>%
  select(gad2_sum, phq2_sum, race, is_female, age, edu, income, floyd_weekOrNot, PWEIGHT) %>%
  gather('descriptor', 'value', race, is_female, age, edu, income) %>%
  mutate(is_dep = phq2_sum > 2, is_anx = gad2_sum > 2) %>%
  group_by(descriptor, value, floyd_weekOrNot) %>%
  summarize(across(.cols = c(is_dep, is_anx),
                   .fns = list(mean = ~weighted.mean(., PWEIGHT, na.rm = T),
                               n = ~sum(!is.na(.))
                   )
  )
  ) %>%
  ungroup() %>%
  select(descriptor, value, floyd_weekOrNot, is_dep_mean) %>%
  spread(floyd_weekOrNot, is_dep_mean) %>%
  rename(pre_floyd_phq = `0`, floyd_phq = `1`) %>%
  mutate(change_phq = floyd_phq - pre_floyd_phq) %>%
  drop_na()
s3_percent_phq_n = pulse %>%
  select(gad2_sum, phq2_sum, race, is_female, age, edu, income, floyd_weekOrNot, PWEIGHT) %>%
  gather('descriptor', 'value', race, is_female, age, edu, income) %>%
  mutate(is_dep = phq2_sum > 2, is_anx = gad2_sum > 2) %>%
  group_by(descriptor, value, floyd_weekOrNot) %>%
  summarize(across(.cols = c(is_dep, is_anx),
                   .fns = list(mean = ~weighted.mean(., PWEIGHT, na.rm = T),
                               n = ~sum(!is.na(.))
                   )
  )
  ) %>%
  ungroup() %>%
  select(descriptor, value, floyd_weekOrNot, is_dep_n) %>%
  spread(floyd_weekOrNot, is_dep_n) %>%
  rename(pre_floyd_phq_n = `0`, floyd_phq_n = `1`) %>%
  drop_na()
s3_percent_table = s3_percent_gad_means %>%
  inner_join(s3_percent_gad_n) %>%
  inner_join(s3_percent_phq_means) %>%
  inner_join(s3_percent_phq_n) %>%
  drop_na() %>%
  mutate(value = if_else(value == 0, 'Male', 
                         if_else(value == 1, 'Female', value))) %>%
  mutate(descriptor = case_when(
    descriptor == 'age' ~ 'Age',
    descriptor == 'edu' ~ 'Education',
    descriptor == 'income' ~ 'Income',
    descriptor == 'is_female' ~ 'Gender',
    descriptor == 'race' ~ 'Race'
    ),
    descriptor = factor(descriptor, levels = c('Race', 'Gender', 'Age', 'Education', 'Income'))) %>%
  mutate(across(c(pre_floyd_gad, floyd_gad, change_gad,
                  pre_floyd_phq, floyd_phq, change_phq), .fns = ~round(. * 100, 3)))
s3_percent_table

s3_percent_gad_means = pulse %>%
  select(gad2_sum, phq2_sum, race, is_female, age, edu, income, floyd_weekOrNot, PWEIGHT) %>%
  mutate(descriptor = 'Overall', value = 'Overall',
         is_dep = phq2_sum > 2, is_anx = gad2_sum > 2) %>%
  group_by(descriptor, value, floyd_weekOrNot) %>%
  summarize(across(.cols = c(is_dep, is_anx),
                   .fns = list(mean = ~weighted.mean(., PWEIGHT, na.rm = T),
                               n = ~sum(!is.na(.))
                   )
  )
  ) %>%
  ungroup() %>%
  select(descriptor, value, floyd_weekOrNot, is_anx_mean) %>%
  spread(floyd_weekOrNot, is_anx_mean) %>%
  rename(pre_floyd_gad = `0`, floyd_gad = `1`) %>%
  mutate(change_gad = floyd_gad - pre_floyd_gad) %>%
  drop_na()
s3_percent_gad_n = pulse %>%
  select(gad2_sum, phq2_sum, race, is_female, age, edu, income, floyd_weekOrNot, PWEIGHT) %>%
  mutate(descriptor = 'Overall', value = 'Overall',
         is_dep = phq2_sum > 2, is_anx = gad2_sum > 2) %>%
  group_by(descriptor, value, floyd_weekOrNot) %>%
  summarize(across(.cols = c(is_dep, is_anx),
                   .fns = list(mean = ~weighted.mean(., PWEIGHT, na.rm = T),
                               n = ~sum(!is.na(.))
                   )
  )
  ) %>%
  ungroup() %>%
  select(descriptor, value, floyd_weekOrNot, is_anx_n) %>%
  spread(floyd_weekOrNot, is_anx_n) %>%
  rename(pre_floyd_gad_n = `0`, floyd_gad_n = `1`) %>%
  drop_na()
s3_percent_phq_means = pulse %>%
  select(gad2_sum, phq2_sum, race, is_female, age, edu, income, floyd_weekOrNot, PWEIGHT) %>%
  mutate(descriptor = 'Overall', value = 'Overall',
         is_dep = phq2_sum > 2, is_anx = gad2_sum > 2) %>%
  group_by(descriptor, value, floyd_weekOrNot) %>%
  summarize(across(.cols = c(is_dep, is_anx),
                   .fns = list(mean = ~weighted.mean(., PWEIGHT, na.rm = T),
                               n = ~sum(!is.na(.))
                   )
  )
  ) %>%
  ungroup() %>%
  select(descriptor, value, floyd_weekOrNot, is_dep_mean) %>%
  spread(floyd_weekOrNot, is_dep_mean) %>%
  rename(pre_floyd_phq = `0`, floyd_phq = `1`) %>%
  mutate(change_phq = floyd_phq - pre_floyd_phq) %>%
  drop_na()
s3_percent_phq_n = pulse %>%
  select(gad2_sum, phq2_sum, race, is_female, age, edu, income, floyd_weekOrNot, PWEIGHT) %>%
  mutate(descriptor = 'Overall', value = 'Overall',
         is_dep = phq2_sum > 2, is_anx = gad2_sum > 2) %>%
  group_by(descriptor, value, floyd_weekOrNot) %>%
  summarize(across(.cols = c(is_dep, is_anx),
                   .fns = list(mean = ~weighted.mean(., PWEIGHT, na.rm = T),
                               n = ~sum(!is.na(.))
                   )
  )
  ) %>%
  ungroup() %>%
  select(descriptor, value, floyd_weekOrNot, is_dep_n) %>%
  spread(floyd_weekOrNot, is_dep_n) %>%
  rename(pre_floyd_phq_n = `0`, floyd_phq_n = `1`) %>%
  drop_na()
s3_percent_table_overall = s3_percent_gad_means %>%
  inner_join(s3_percent_gad_n) %>%
  inner_join(s3_percent_phq_means) %>%
  inner_join(s3_percent_phq_n) %>%
  drop_na() %>%
  mutate(value = if_else(value == 0, 'Male', 
                         if_else(value == 1, 'Female', value))) %>%
  mutate(descriptor = case_when(
    descriptor == 'age' ~ 'Age',
    descriptor == 'edu' ~ 'Education',
    descriptor == 'income' ~ 'Income',
    descriptor == 'is_female' ~ 'Gender',
    descriptor == 'race' ~ 'Race'
  ),
  descriptor = factor(descriptor, levels = c('Race', 'Gender', 'Age', 'Education', 'Income'))) %>%
  mutate(across(c(pre_floyd_gad, floyd_gad, change_gad,
                  pre_floyd_phq, floyd_phq, change_phq), .fns = ~round(. * 100, 3)))
s3_percent_table_overall 


# DESCRIPTIVE STATISTICS: CENSUS GEOGRAPHIC BEFORE AND AFTER FLOYD'S MURDER =====

# Calculates descriptive statistics for symptom severity in Minnesota vs. other states in
# the same manner as prior sections. Comments will be elided. This data is used for Table S7.

s3_severity_gad_means = pulse %>%
  select(gad2_sum, phq2_sum, state, floyd_weekOrNot, PWEIGHT) %>%
  mutate(state = if_else(state == 'Minnesota', state, 'Other states')) %>%
  gather('descriptor', 'value', state) %>%
  mutate(is_dep = phq2_sum > 2, is_anx = gad2_sum > 2) %>%
  group_by(descriptor, value, floyd_weekOrNot) %>%
  summarize(across(.cols = c(gad2_sum, phq2_sum),
                   .fns = list(mean = ~weighted.mean(., PWEIGHT, na.rm = T),
                               se = ~diagis::weighted_se(., PWEIGHT, na.rm = T),
                               n = ~sum(!is.na(.))
                   )
  )
  ) %>%
  ungroup() %>%
  select(descriptor, value, floyd_weekOrNot, gad2_sum_mean) %>%
  spread(floyd_weekOrNot, gad2_sum_mean) %>%
  rename(pre_floyd_gad = `0`, floyd_gad = `1`) %>%
  mutate(change_gad = floyd_gad - pre_floyd_gad) %>%
  drop_na()
s3_severity_gad_ses = pulse %>%
  select(gad2_sum, phq2_sum, state, floyd_weekOrNot, PWEIGHT) %>%
  mutate(state = if_else(state == 'Minnesota', state, 'Other states')) %>%
  gather('descriptor', 'value', state) %>%
  mutate(is_dep = phq2_sum > 2, is_anx = gad2_sum > 2) %>%
  group_by(descriptor, value, floyd_weekOrNot) %>%
  summarize(across(.cols = c(gad2_sum, phq2_sum),
                   .fns = list(mean = ~weighted.mean(., PWEIGHT, na.rm = T),
                               se = ~diagis::weighted_se(., PWEIGHT, na.rm = T),
                               n = ~sum(!is.na(.))
                   )
  )
  ) %>%
  ungroup() %>%
  select(descriptor, value, floyd_weekOrNot, gad2_sum_se) %>%
  spread(floyd_weekOrNot, gad2_sum_se) %>%
  rename(pre_floyd_gad_se = `0`, floyd_gad_se = `1`) %>%
  drop_na()
s3_severity_phq_means = pulse %>%
  select(gad2_sum, phq2_sum, state, floyd_weekOrNot, PWEIGHT) %>%
  mutate(state = if_else(state == 'Minnesota', state, 'Other states')) %>%
  gather('descriptor', 'value', state) %>%
  mutate(is_dep = phq2_sum > 2, is_anx = gad2_sum > 2) %>%
  group_by(descriptor, value, floyd_weekOrNot) %>%
  summarize(across(.cols = c(gad2_sum, phq2_sum),
                   .fns = list(mean = ~weighted.mean(., PWEIGHT, na.rm = T),
                               se = ~diagis::weighted_se(., PWEIGHT, na.rm = T),
                               n = ~sum(!is.na(.))
                   )
  )
  ) %>%
  ungroup() %>%
  select(descriptor, value, floyd_weekOrNot, phq2_sum_mean) %>%
  spread(floyd_weekOrNot, phq2_sum_mean) %>%
  rename(pre_floyd_phq = `0`, floyd_phq = `1`) %>%
  mutate(change_phq = floyd_phq - pre_floyd_phq) %>%
  drop_na()
s3_severity_phq_ses = pulse %>%
  select(gad2_sum, phq2_sum, state, floyd_weekOrNot, PWEIGHT) %>%
  mutate(state = if_else(state == 'Minnesota', state, 'Other states')) %>%
  gather('descriptor', 'value', state) %>%
  mutate(is_dep = phq2_sum > 2, is_anx = gad2_sum > 2) %>%
  group_by(descriptor, value, floyd_weekOrNot) %>%
  summarize(across(.cols = c(gad2_sum, phq2_sum),
                   .fns = list(mean = ~weighted.mean(., PWEIGHT, na.rm = T),
                               se = ~diagis::weighted_se(., PWEIGHT, na.rm = T),
                               n = ~sum(!is.na(.))
                   )
  )
  ) %>%
  ungroup() %>%
  select(descriptor, value, floyd_weekOrNot, phq2_sum_se) %>%
  spread(floyd_weekOrNot, phq2_sum_se) %>%
  rename(pre_floyd_phq_se = `0`, floyd_phq_se = `1`) %>%
  drop_na()
s3_severity_gad_d_n = pulse %>%
  select(gad2_sum, phq2_sum, state, floyd_weekOrNot, PWEIGHT) %>%
  mutate(state = if_else(state == 'Minnesota', state, 'Other states')) %>%
  gather('descriptor', 'value', state) %>%
  filter(!is.na(value)) %>%
  group_by(descriptor, value) %>%
  summarize(weighted_cohens_d(gad2_sum, floyd_weekOrNot, PWEIGHT)) %>%
  ungroup() %>%
  rename(floyd_n_gad = n1, pre_floyd_n_gad = n2, d_gad = d) %>%
  select(-se)
s3_severity_phq_d_n = pulse %>%
  select(gad2_sum, phq2_sum, state, floyd_weekOrNot, PWEIGHT) %>%
  mutate(state = if_else(state == 'Minnesota', state, 'Other states')) %>%
  gather('descriptor', 'value', state) %>%
  filter(!is.na(value)) %>%
  group_by(descriptor, value) %>%
  summarize(weighted_cohens_d(phq2_sum, floyd_weekOrNot, PWEIGHT)) %>%
  ungroup() %>%
  rename(floyd_n_phq = n1, pre_floyd_n_phq = n2, d_phq = d) %>%
  select(-se)
s3_severity_gad_p = pulse %>%
  select(gad2_sum, phq2_sum, state, floyd_weekOrNot, PWEIGHT) %>%
  mutate(state = if_else(state == 'Minnesota', state, 'Other states')) %>%
  gather('descriptor', 'value', state) %>%
  filter(!is.na(value)) %>%
  group_by(descriptor, value) %>%
  summarize(ttest = weights::wtd.t.test(gad2_sum[floyd_weekOrNot == 0],
                                        gad2_sum[floyd_weekOrNot == 1],
                                        PWEIGHT[floyd_weekOrNot == 0],
                                        PWEIGHT[floyd_weekOrNot == 1]),
            t_gad = get_from_ttest(ttest, 't'),
            df_gad = get_from_ttest(ttest, 'df'),
            p_gad = get_from_ttest(ttest, 'p')) %>%
  ungroup() %>%
  select(-ttest) %>%
  distinct()
s3_severity_phq_p = pulse %>%
  select(gad2_sum, phq2_sum, state, floyd_weekOrNot, PWEIGHT) %>%
  mutate(state = if_else(state == 'Minnesota', state, 'Other states')) %>%
  gather('descriptor', 'value', state) %>%
  filter(!is.na(value)) %>%
  group_by(descriptor, value) %>%
  summarize(ttest = weights::wtd.t.test(phq2_sum[floyd_weekOrNot == 0],
                                        phq2_sum[floyd_weekOrNot == 1],
                                        PWEIGHT[floyd_weekOrNot == 0],
                                        PWEIGHT[floyd_weekOrNot == 1]),
            t_phq = get_from_ttest(ttest, 't'),
            df_phq = get_from_ttest(ttest, 'df'),
            p_phq = get_from_ttest(ttest, 'p')) %>%
  ungroup() %>%
  select(-ttest) %>%
  distinct()
s3_severity_table_mn = s3_severity_gad_means %>%
  inner_join(s3_severity_gad_ses) %>%
  inner_join(s3_severity_gad_d_n) %>%
  inner_join(s3_severity_gad_p) %>%
  inner_join(s3_severity_phq_means) %>%
  inner_join(s3_severity_phq_ses) %>%
  inner_join(s3_severity_phq_d_n) %>%
  inner_join(s3_severity_phq_p) %>%
  select(descriptor, value, 
         pre_floyd_gad, pre_floyd_gad_se, floyd_gad, floyd_gad_se, change_gad, d_gad, t_gad, df_gad, p_gad, pre_floyd_n_gad, floyd_n_gad,
         pre_floyd_phq, pre_floyd_phq_se, floyd_phq, floyd_phq_se, change_phq, d_phq, t_phq, df_phq, p_phq, pre_floyd_n_phq, floyd_n_phq) %>%
  drop_na() %>%
  mutate(value = if_else(value == 0, 'Male', 
                         if_else(value == 1, 'Female', value))) %>%
  mutate(across(c(-descriptor, -value), .fns = ~round(., 3)))
s3_severity_table_mn

# Overall
s3_severity_gad_means = pulse %>%
  select(gad2_sum, phq2_sum, state, floyd_weekOrNot, PWEIGHT) %>%
  mutate(descriptor = 'Overall', value = 'Overall',
         state = if_else(state == 'Minnesota', state, 'Other states')) %>%
  mutate(is_dep = phq2_sum > 2, is_anx = gad2_sum > 2) %>%
  group_by(descriptor, value, floyd_weekOrNot) %>%
  summarize(across(.cols = c(gad2_sum, phq2_sum),
                   .fns = list(mean = ~weighted.mean(., PWEIGHT, na.rm = T),
                               se = ~diagis::weighted_se(., PWEIGHT, na.rm = T),
                               n = ~sum(!is.na(.))
                   )
  )
  ) %>%
  ungroup() %>%
  select(descriptor, value, floyd_weekOrNot, gad2_sum_mean) %>%
  spread(floyd_weekOrNot, gad2_sum_mean) %>%
  rename(pre_floyd_gad = `0`, floyd_gad = `1`) %>%
  mutate(change_gad = floyd_gad - pre_floyd_gad) %>%
  drop_na()
s3_severity_gad_ses = pulse %>%
  select(gad2_sum, phq2_sum, state, floyd_weekOrNot, PWEIGHT) %>%
  mutate(descriptor = 'Overall', value = 'Overall',
         state = if_else(state == 'Minnesota', state, 'Other states')) %>%
  mutate(is_dep = phq2_sum > 2, is_anx = gad2_sum > 2) %>%
  group_by(descriptor, value, floyd_weekOrNot) %>%
  summarize(across(.cols = c(gad2_sum, phq2_sum),
                   .fns = list(mean = ~weighted.mean(., PWEIGHT, na.rm = T),
                               se = ~diagis::weighted_se(., PWEIGHT, na.rm = T),
                               n = ~sum(!is.na(.))
                   )
  )
  ) %>%
  ungroup() %>%
  select(descriptor, value, floyd_weekOrNot, gad2_sum_se) %>%
  spread(floyd_weekOrNot, gad2_sum_se) %>%
  rename(pre_floyd_gad_se = `0`, floyd_gad_se = `1`) %>%
  drop_na()
s3_severity_phq_means = pulse %>%
  select(gad2_sum, phq2_sum, state, floyd_weekOrNot, PWEIGHT) %>%
  mutate(descriptor = 'Overall', value = 'Overall',
         state = if_else(state == 'Minnesota', state, 'Other states')) %>%
  mutate(is_dep = phq2_sum > 2, is_anx = gad2_sum > 2) %>%
  group_by(descriptor, value, floyd_weekOrNot) %>%
  summarize(across(.cols = c(gad2_sum, phq2_sum),
                   .fns = list(mean = ~weighted.mean(., PWEIGHT, na.rm = T),
                               se = ~diagis::weighted_se(., PWEIGHT, na.rm = T),
                               n = ~sum(!is.na(.))
                   )
  )
  ) %>%
  ungroup() %>%
  select(descriptor, value, floyd_weekOrNot, phq2_sum_mean) %>%
  spread(floyd_weekOrNot, phq2_sum_mean) %>%
  rename(pre_floyd_phq = `0`, floyd_phq = `1`) %>%
  mutate(change_phq = floyd_phq - pre_floyd_phq) %>%
  drop_na()
s3_severity_phq_ses = pulse %>%
  select(gad2_sum, phq2_sum, state, floyd_weekOrNot, PWEIGHT) %>%
  mutate(descriptor = 'Overall', value = 'Overall',
         state = if_else(state == 'Minnesota', state, 'Other states')) %>%
  mutate(is_dep = phq2_sum > 2, is_anx = gad2_sum > 2) %>%
  group_by(descriptor, value, floyd_weekOrNot) %>%
  summarize(across(.cols = c(gad2_sum, phq2_sum),
                   .fns = list(mean = ~weighted.mean(., PWEIGHT, na.rm = T),
                               se = ~diagis::weighted_se(., PWEIGHT, na.rm = T),
                               n = ~sum(!is.na(.))
                   )
  )
  ) %>%
  ungroup() %>%
  select(descriptor, value, floyd_weekOrNot, phq2_sum_se) %>%
  spread(floyd_weekOrNot, phq2_sum_se) %>%
  rename(pre_floyd_phq_se = `0`, floyd_phq_se = `1`) %>%
  drop_na()
s3_severity_gad_d_n = pulse %>%
  select(gad2_sum, phq2_sum, state, floyd_weekOrNot, PWEIGHT) %>%
  mutate(descriptor = 'Overall', value = 'Overall',
         state = if_else(state == 'Minnesota', state, 'Other states')) %>%
  filter(!is.na(value)) %>%
  group_by(descriptor, value) %>%
  summarize(weighted_cohens_d(gad2_sum, floyd_weekOrNot, PWEIGHT)) %>%
  ungroup() %>%
  rename(floyd_n_gad = n1, pre_floyd_n_gad = n2, d_gad = d) %>%
  select(-se)
s3_severity_phq_d_n = pulse %>%
  select(gad2_sum, phq2_sum, state, floyd_weekOrNot, PWEIGHT) %>%
  mutate(descriptor = 'Overall', value = 'Overall',
         state = if_else(state == 'Minnesota', state, 'Other states')) %>%
  filter(!is.na(value)) %>%
  group_by(descriptor, value) %>%
  summarize(weighted_cohens_d(phq2_sum, floyd_weekOrNot, PWEIGHT)) %>%
  ungroup() %>%
  rename(floyd_n_phq = n1, pre_floyd_n_phq = n2, d_phq = d) %>%
  select(-se)
s3_severity_gad_p = pulse %>%
  select(gad2_sum, phq2_sum, state, floyd_weekOrNot, PWEIGHT) %>%
  mutate(descriptor = 'Overall', value = 'Overall',
         state = if_else(state == 'Minnesota', state, 'Other states')) %>%
  filter(!is.na(value)) %>%
  group_by(descriptor, value) %>%
  summarize(ttest = weights::wtd.t.test(gad2_sum[floyd_weekOrNot == 0],
                                        gad2_sum[floyd_weekOrNot == 1],
                                        PWEIGHT[floyd_weekOrNot == 0],
                                        PWEIGHT[floyd_weekOrNot == 1]),
            t_gad = get_from_ttest(ttest, 't'),
            df_gad = get_from_ttest(ttest, 'df'),
            p_gad = get_from_ttest(ttest, 'p')) %>%
  ungroup() %>%
  select(-ttest) %>%
  distinct()
s3_severity_phq_p = pulse %>%
  select(gad2_sum, phq2_sum, state, floyd_weekOrNot, PWEIGHT) %>%
  mutate(descriptor = 'Overall', value = 'Overall',
         state = if_else(state == 'Minnesota', state, 'Other states')) %>%
  filter(!is.na(value)) %>%
  group_by(descriptor, value) %>%
  summarize(ttest = weights::wtd.t.test(phq2_sum[floyd_weekOrNot == 0],
                                        phq2_sum[floyd_weekOrNot == 1],
                                        PWEIGHT[floyd_weekOrNot == 0],
                                        PWEIGHT[floyd_weekOrNot == 1]),
            t_phq = get_from_ttest(ttest, 't'),
            df_phq = get_from_ttest(ttest, 'df'),
            p_phq = get_from_ttest(ttest, 'p')) %>%
  ungroup() %>%
  select(-ttest) %>%
  distinct()
s3_severity_table_mn_overall = s3_severity_gad_means %>%
  inner_join(s3_severity_gad_ses) %>%
  inner_join(s3_severity_gad_d_n) %>%
  inner_join(s3_severity_gad_p) %>%
  inner_join(s3_severity_phq_means) %>%
  inner_join(s3_severity_phq_ses) %>%
  inner_join(s3_severity_phq_d_n) %>%
  inner_join(s3_severity_phq_p) %>%
  select(descriptor, value, 
         pre_floyd_gad, pre_floyd_gad_se, floyd_gad, floyd_gad_se, change_gad, d_gad, t_gad, df_gad, p_gad, pre_floyd_n_gad, floyd_n_gad,
         pre_floyd_phq, pre_floyd_phq_se, floyd_phq, floyd_phq_se, change_phq, d_phq, t_phq, df_phq, p_phq, pre_floyd_n_phq, floyd_n_phq) %>%
  drop_na() %>%
  mutate(value = if_else(value == 0, 'Male', 
                         if_else(value == 1, 'Female', value))) %>%
  mutate(across(c(-descriptor, -value), .fns = ~round(., 3)))
s3_severity_table_mn_overall 


# DESCRIPTIVE STATISTICS: CENSUS GEOGRAPHIC BEFORE AND AFTER FLOYD'S MURDER =====

# Calculates descriptive statistics for depression and anxiety categories in Minnesota vs. other states in
# the same manner as prior sections. Comments will be elided. This data is used for Table S8.

s3_percent_gad_means = pulse %>%
  select(gad2_sum, phq2_sum, state, floyd_weekOrNot, PWEIGHT) %>%
  mutate(state = if_else(state == 'Minnesota', state, 'Other states')) %>%
  gather('descriptor', 'value', state) %>%
  mutate(is_dep = phq2_sum > 2, is_anx = gad2_sum > 2) %>%
  group_by(descriptor, value, floyd_weekOrNot) %>%
  summarize(across(.cols = c(is_dep, is_anx),
                   .fns = list(mean = ~weighted.mean(., PWEIGHT, na.rm = T),
                               n = ~sum(!is.na(.))
                   )
  )
  ) %>%
  ungroup() %>%
  select(descriptor, value, floyd_weekOrNot, is_anx_mean) %>%
  spread(floyd_weekOrNot, is_anx_mean) %>%
  rename(pre_floyd_gad = `0`, floyd_gad = `1`) %>%
  mutate(change_gad = floyd_gad - pre_floyd_gad) %>%
  drop_na()
s3_percent_gad_n = pulse %>%
  select(gad2_sum, phq2_sum, state, floyd_weekOrNot, PWEIGHT) %>%
  mutate(state = if_else(state == 'Minnesota', state, 'Other states')) %>%
  gather('descriptor', 'value', state) %>%
  mutate(is_dep = phq2_sum > 2, is_anx = gad2_sum > 2) %>%
  group_by(descriptor, value, floyd_weekOrNot) %>%
  summarize(across(.cols = c(is_dep, is_anx),
                   .fns = list(mean = ~weighted.mean(., PWEIGHT, na.rm = T),
                               n = ~sum(!is.na(.))
                   )
  )
  ) %>%
  ungroup() %>%
  select(descriptor, value, floyd_weekOrNot, is_anx_n) %>%
  spread(floyd_weekOrNot, is_anx_n) %>%
  rename(pre_floyd_gad_n = `0`, floyd_gad_n = `1`) %>%
  drop_na()
s3_percent_phq_means = pulse %>%
  select(gad2_sum, phq2_sum, state, floyd_weekOrNot, PWEIGHT) %>%
  mutate(state = if_else(state == 'Minnesota', state, 'Other states')) %>%
  gather('descriptor', 'value', state) %>%
  mutate(is_dep = phq2_sum > 2, is_anx = gad2_sum > 2) %>%
  group_by(descriptor, value, floyd_weekOrNot) %>%
  summarize(across(.cols = c(is_dep, is_anx),
                   .fns = list(mean = ~weighted.mean(., PWEIGHT, na.rm = T),
                               n = ~sum(!is.na(.))
                   )
  )
  ) %>%
  ungroup() %>%
  select(descriptor, value, floyd_weekOrNot, is_dep_mean) %>%
  spread(floyd_weekOrNot, is_dep_mean) %>%
  rename(pre_floyd_phq = `0`, floyd_phq = `1`) %>%
  mutate(change_phq = floyd_phq - pre_floyd_phq) %>%
  drop_na()
s3_percent_phq_n = pulse %>%
  select(gad2_sum, phq2_sum, state, floyd_weekOrNot, PWEIGHT) %>%
  mutate(state = if_else(state == 'Minnesota', state, 'Other states')) %>%
  gather('descriptor', 'value', state) %>%
  mutate(is_dep = phq2_sum > 2, is_anx = gad2_sum > 2) %>%
  group_by(descriptor, value, floyd_weekOrNot) %>%
  summarize(across(.cols = c(is_dep, is_anx),
                   .fns = list(mean = ~weighted.mean(., PWEIGHT, na.rm = T),
                               n = ~sum(!is.na(.))
                   )
  )
  ) %>%
  ungroup() %>%
  select(descriptor, value, floyd_weekOrNot, is_dep_n) %>%
  spread(floyd_weekOrNot, is_dep_n) %>%
  rename(pre_floyd_phq_n = `0`, floyd_phq_n = `1`) %>%
  drop_na()
s3_percent_mn = s3_percent_gad_means %>%
  inner_join(s3_percent_gad_n) %>%
  inner_join(s3_percent_phq_means) %>%
  inner_join(s3_percent_phq_n) %>%
  drop_na() %>%
  mutate(value = if_else(value == 0, 'Male', 
                         if_else(value == 1, 'Female', value))) %>%
  mutate(across(c(pre_floyd_gad, floyd_gad, change_gad,
                  pre_floyd_phq, floyd_phq, change_phq), .fns = ~round(. * 100, 3)))
s3_percent_mn

# Overall
s3_percent_gad_means = pulse %>%
  select(gad2_sum, phq2_sum, state, floyd_weekOrNot, PWEIGHT) %>%
  mutate(descriptor = 'Overall', value = 'Overall',
         state = if_else(state == 'Minnesota', state, 'Other states')) %>%
  mutate(is_dep = phq2_sum > 2, is_anx = gad2_sum > 2) %>%
  group_by(descriptor, value, floyd_weekOrNot) %>%
  summarize(across(.cols = c(is_dep, is_anx),
                   .fns = list(mean = ~weighted.mean(., PWEIGHT, na.rm = T),
                               n = ~sum(!is.na(.))
                   )
  )
  ) %>%
  ungroup() %>%
  select(descriptor, value, floyd_weekOrNot, is_anx_mean) %>%
  spread(floyd_weekOrNot, is_anx_mean) %>%
  rename(pre_floyd_gad = `0`, floyd_gad = `1`) %>%
  mutate(change_gad = floyd_gad - pre_floyd_gad) %>%
  drop_na()
s3_percent_gad_n = pulse %>%
  select(gad2_sum, phq2_sum, state, floyd_weekOrNot, PWEIGHT) %>%
  mutate(descriptor = 'Overall', value = 'Overall',
         state = if_else(state == 'Minnesota', state, 'Other states')) %>%
  mutate(is_dep = phq2_sum > 2, is_anx = gad2_sum > 2) %>%
  group_by(descriptor, value, floyd_weekOrNot) %>%
  summarize(across(.cols = c(is_dep, is_anx),
                   .fns = list(mean = ~weighted.mean(., PWEIGHT, na.rm = T),
                               n = ~sum(!is.na(.))
                   )
  )
  ) %>%
  ungroup() %>%
  select(descriptor, value, floyd_weekOrNot, is_anx_n) %>%
  spread(floyd_weekOrNot, is_anx_n) %>%
  rename(pre_floyd_gad_n = `0`, floyd_gad_n = `1`) %>%
  drop_na()
s3_percent_phq_means = pulse %>%
  select(gad2_sum, phq2_sum, state, floyd_weekOrNot, PWEIGHT) %>%
  mutate(descriptor = 'Overall', value = 'Overall',
         state = if_else(state == 'Minnesota', state, 'Other states')) %>%
  mutate(is_dep = phq2_sum > 2, is_anx = gad2_sum > 2) %>%
  group_by(descriptor, value, floyd_weekOrNot) %>%
  summarize(across(.cols = c(is_dep, is_anx),
                   .fns = list(mean = ~weighted.mean(., PWEIGHT, na.rm = T),
                               n = ~sum(!is.na(.))
                   )
  )
  ) %>%
  ungroup() %>%
  select(descriptor, value, floyd_weekOrNot, is_dep_mean) %>%
  spread(floyd_weekOrNot, is_dep_mean) %>%
  rename(pre_floyd_phq = `0`, floyd_phq = `1`) %>%
  mutate(change_phq = floyd_phq - pre_floyd_phq) %>%
  drop_na()
s3_percent_phq_n = pulse %>%
  select(gad2_sum, phq2_sum, state, floyd_weekOrNot, PWEIGHT) %>%
  mutate(descriptor = 'Overall', value = 'Overall',
         state = if_else(state == 'Minnesota', state, 'Other states')) %>%
  mutate(is_dep = phq2_sum > 2, is_anx = gad2_sum > 2) %>%
  group_by(descriptor, value, floyd_weekOrNot) %>%
  summarize(across(.cols = c(is_dep, is_anx),
                   .fns = list(mean = ~weighted.mean(., PWEIGHT, na.rm = T),
                               n = ~sum(!is.na(.))
                   )
  )
  ) %>%
  ungroup() %>%
  select(descriptor, value, floyd_weekOrNot, is_dep_n) %>%
  spread(floyd_weekOrNot, is_dep_n) %>%
  rename(pre_floyd_phq_n = `0`, floyd_phq_n = `1`) %>%
  drop_na()
s3_percent_mn_overall = s3_percent_gad_means %>%
  inner_join(s3_percent_gad_n) %>%
  inner_join(s3_percent_phq_means) %>%
  inner_join(s3_percent_phq_n) %>%
  drop_na() %>%
  mutate(value = if_else(value == 0, 'Male', 
                         if_else(value == 1, 'Female', value))) %>%
  mutate(across(c(pre_floyd_gad, floyd_gad, change_gad,
                  pre_floyd_phq, floyd_phq, change_phq), .fns = ~round(. * 100, 3)))
s3_percent_mn_overall 