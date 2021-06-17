source('setup_all.R')
source('setup_gallup.R')
source('setup_significance_testing.R')


# ARROW PLOTS =====

# Arrow plots represent the change in emotion levels from before Floyd's death (the bottom of the arrow)
# to after (the top of the arrow). Each demographic category is represented by a different arrow.


# Change in proportion -----

# Limit to Floyd week or the preceding 4 weeks
indiv = indiv %>%
  filter(floyd_weekOrNot == 1 | (floyd_weekOrNot == 0 & 
                                   ((endDate_week_delta < 0 & endDate_week_delta > -5)))) %>%
  select(-endDate_week_delta, -FIPS)

# Calculate proportion (mean) and standard error of each emotion (anger and sadness) for each demographic
# category separately. Note that weights are not used, per Gallup's recommendations.
anger_sad_summary = indiv %>%
  select(-EMPLOYEE_KEY_VALUE, -endDate_asDate) %>%
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
  select(-EMPLOYEE_KEY_VALUE, -endDate_asDate, -WEIGHT) %>%
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
      filter(descriptor == {{d}}) %>%
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

# Create arrow plot for change in anger. Contains several steps:
# * Filtering to variables of interest
# * Reshaping data
# * Cleaning variable names for presentation in figure
# * Creating figure
# Significance levels are derived from the \code{p_vals} variable.
anger_sad_summary %>%
  select(-change) %>%
  filter(str_detect(stat, 'angerF'),
         descriptor %in% c('Overall', 'DEMO_AGE_BINS2', 'DEMO_GENDER_NAME', 'DEMO_RACE_NAME', 'party_name'),
         !(value %in% c('asian', 'other', 'hispanic'))) %>%
  drop_na() %>%
  gather('floyd', 'val', floyd, not_floyd) %>%
  spread(stat, val) %>%
  group_by(descriptor, value) %>%
  rename(not_floyd_mean = WEE_angerF_mean, not_floyd_se = WEE_angerF_se) %>%
  mutate(floyd_mean = lag(not_floyd_mean), floyd_se = lag(not_floyd_se)) %>%
  ungroup() %>%
  filter(floyd == 'not_floyd') %>%
  mutate(value = reorder(str_to_title(value), descriptor),
         value = factor(value, levels = c('Overall',
                                          'Asian', 'Black', 'Hispanic', 'White', 'Other',
                                          'Democrat', 'Republican',
                                          '18-30', '31-45', '46+',
                                          'Female', 'Male')),
         descriptor = case_when(
           descriptor == 'Overall' ~ 'Overall',
           descriptor == 'DEMO_AGE_BINS2' ~ 'Age',
           descriptor == 'DEMO_GENDER_NAME' ~ 'Gender',
           descriptor == 'DEMO_RACE_NAME' ~ 'Race',
           descriptor == 'party_name' ~ 'Political party'
         ),
         descriptor = factor(descriptor, levels = c('Overall', 'Race', 'Political party', 'Age',
                                                    'Gender'))) %>%
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
              y_position = c(.53, .43, .45, .53),
              comparisons = list(
                c('White', 'Black'),
                c('Democrat', 'Republican'),
                c('46+', '31-45'),
                c('46+', '18-30')
              ), 
              annotation = c('***', '*', '*', '**'),
              textsize = signif_size) +
  theme_Publication() +
  theme(axis.text.x = element_text(angle = 35, hjust = 1),
        panel.grid.major.x = element_blank(),
        legend.position = c(10, 10)) +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1L),
                     breaks = seq(0, 1, by = .1),
                     limits = c(NA, .55)) +
  scale_color_manual(values = c('#ba0c00', my_palette[2:length(my_palette)])) +
  scale_fill_manual(values = c('#ba0c00', my_palette[2:length(my_palette)])) +
  labs(x = '',
       y = 'Percent angry',
       color = '', fill = '') +
  ggsave('anger_arrows_main.png', width=12, height=8, units='in', dpi=300)

# Create arrow plot for change in sadness. Follows the same steps as above.
anger_sad_summary %>%
  select(-change) %>%
  filter(str_detect(stat, 'sadF'),
         descriptor %in% c('Overall', 'DEMO_AGE_BINS2', 'DEMO_GENDER_NAME', 'DEMO_RACE_NAME', 'party_name'),
         !(value %in% c('asian', 'other', 'hispanic'))) %>%
  drop_na() %>%
  gather('floyd', 'val', floyd, not_floyd) %>%
  spread(stat, val) %>%
  group_by(descriptor, value) %>%
  rename(not_floyd_mean = WEC_sadF_mean, not_floyd_se = WEC_sadF_se) %>%
  mutate(floyd_mean = lag(not_floyd_mean), floyd_se = lag(not_floyd_se)) %>%
  ungroup() %>%
  filter(floyd == 'not_floyd') %>%
  mutate(value = reorder(str_to_title(value), descriptor),
         value = factor(value, levels = c('Overall',
                                          'Asian', 'Black', 'Hispanic', 'White', 'Other',
                                          'Democrat', 'Republican',
                                          '18-30', '31-45', '46+',
                                          'Female', 'Male')),
         descriptor = case_when(
           descriptor == 'Overall' ~ 'Overall',
           descriptor == 'DEMO_AGE_BINS2' ~ 'Age',
           descriptor == 'DEMO_GENDER_NAME' ~ 'Gender',
           descriptor == 'DEMO_RACE_NAME' ~ 'Race',
           descriptor == 'party_name' ~ 'Political party'
         ),
         descriptor = factor(descriptor, levels = c('Overall', 'Race', 'Political party', 'Age',
                                                    'Gender'))) %>%
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
              y_position = c(.53, .42),
              comparisons = list(
                c('White', 'Black'),
                c('46+', '31-45')
              ), 
              annotation = c('*', '†'),
              textsize = signif_size) +
  theme_Publication() +
  theme(axis.text.x = element_text(angle = 35, hjust = 1),
        panel.grid.major.x = element_blank(),
        legend.position = c(10, 10)) +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1L),
                     breaks = seq(0, 1, by = .1),
                     limits = c(NA, .55)) +
  scale_color_manual(values = c('#00468b', my_palette[2:length(my_palette)])) +
  scale_fill_manual(values = c('#00468b', my_palette[2:length(my_palette)])) +
  labs(x = '',
       y = 'Percent sad',
       color = '', fill = '') +
  ggsave('sad_arrows_main.png', width=12, height=8, units='in', dpi=300)