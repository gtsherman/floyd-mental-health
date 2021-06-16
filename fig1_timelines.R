# Perform setup
source('setup_all.R')
source('setup_gallup.R')


# TIMELINES =====

# Anger -----

# Gallup data through April 26 was weighted at the day level, so we compute daily
# weighted aggregates (proportions and standard errors). Our unit of analysis is
# at the week level, so we then compute unweighted means across days.
daily_weighted = indiv %>%
  mutate(endDate_asDate = as.Date(endDate_asDate)) %>%
  filter(endDate_asDate <= '2020-04-26') %>%  # when data was aggregated daily
  group_by(endDate_asDate) %>%
  summarize(p_angry = weighted.mean(WEE_angerF, WEIGHT, na.rm = T),
            se_angry = weighted_se(WEE_angerF, WEIGHT, na.rm = T) %>%
  ungroup() %>%
  mutate(wk = floor_date(endDate_asDate, 'week', week_start = 1)) %>%
  group_by(wk) %>%
  summarize(se_angry = mean(se_angry),
            p_angry = mean(p_angry)) %>%
  ungroup()

# After April 26, responses are weighted at the week level, so we directly compute
# weighted proportion and standard error for weeks.
weekly_weighted = indiv %>%
  mutate(endDate_asDate = as.Date(endDate_asDate)) %>%
  filter(endDate_asDate > '2020-04-26') %>%
  mutate(wk = floor_date(endDate_asDate, 'week', week_start = 1)) %>%
  group_by(wk) %>%
  summarize(p_angry = weighted.mean(WEE_angerF, WEIGHT, na.rm = T),
            se_angry = weighted_se(WEE_angerF, WEIGHT, na.rm = T)) %>%
  ungroup()

# Concatenate the daily and weekly weighted data and plot as a timeseries.
daily_weighted %>%
  bind_rows(weekly_weighted) %>%
  ggplot(aes(wk, p_angry)) +
  geom_point() +
  geom_smooth(span = .1, se = F, color = orange) +
  geom_errorbar(aes(ymin = p_angry - se_angry, ymax = p_angry + se_angry)) +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1L)) +
  theme_Publication() +
  theme(legend.position = 'none') +
  labs(x = 'Week', y = 'Percent angry') +
  ggsave('anger_timeseries.png', width=12, height=8, units='in', dpi=300)

# Concatenate the daily and weekly weighted data and plot as a histogram.
daily_weighted %>%
  bind_rows(weekly_weighted) %>%
  ggplot(aes(p_angry)) +
  geom_histogram(binwidth = 0.005, fill = orange) +
  geom_vline(aes(xintercept = mean(p_angry)),
             linetype = 'dashed',
             color = '#575757') +
  geom_vline(aes(xintercept = max(p_angry)),
             linetype = 'dashed',
             color = '#575757') +
  geom_segment(data = data.frame(1), y = 3.3, yend = 3.3, x = .252 + .005, xend = .384 - .005, 
               arrow = arrow(length = unit(0.4, "cm"), type = "closed"),
               color = orange, size = 1) +
  coord_flip() +
  theme_Publication() +
  theme(panel.background = element_rect(fill = 'transparent'),
        plot.background = element_rect(fill = 'transparent', color = NA),
        panel.grid.major.x = element_blank(),
        axis.text.y = element_blank()) +
  scale_x_continuous(labels = scales::label_percent(accuracy = 1L),
                     limits = c(.2, .4)) +
  scale_y_continuous(breaks = seq(0, 10, 2)) +
  labs(x = '', y = 'Count') +
  ggsave('anger_histogram.png', width=3, height=8, units='in', dpi=300, bg='transparent')

  
# Sadness -----

# The comments above apply exactly for this code, so will not be repeated.

daily_weighted = indiv %>%
  mutate(endDate_asDate = as.Date(endDate_asDate)) %>%
  filter(endDate_asDate <= '2020-04-26') %>%  # when data was aggregated daily
  group_by(endDate_asDate) %>%
  summarize(p_sad = weighted.mean(WEC_sadF, WEIGHT, na.rm = T),
            se_sad = weighted_se(WEC_sadF, WEIGHT, na.rm = T) %>%
  ungroup() %>%
  mutate(wk = floor_date(endDate_asDate, 'week', week_start = 1)) %>%
  group_by(wk) %>%
  summarize(se_sad = mean(se_sad), 
            p_sad = mean(p_sad)) %>%
  ungroup()
weekly_weighted = indiv %>%
  mutate(endDate_asDate = as.Date(endDate_asDate)) %>%
  filter(endDate_asDate > '2020-04-26') %>%
  mutate(wk = floor_date(endDate_asDate, 'week', week_start = 1)) %>%
  group_by(wk) %>%
  summarize(p_sad = weighted.mean(WEC_sadF, WEIGHT, na.rm = T),
            se_sad = weighted_se(WEC_sadF, WEIGHT, na.rm = T)) %>%
  ungroup()
daily_weighted %>%
  bind_rows(weekly_weighted) %>%
  ggplot(aes(wk, p_sad)) +
  geom_point() +
  geom_smooth(span = .1, se = F, color = blue) +
  geom_errorbar(aes(ymin = p_sad - se_sad, ymax = p_sad + se_sad)) +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1L),
                     limits = c(.2, .4)) +
  theme_Publication() +
  theme(legend.position = 'none') +
  labs(x = 'Week', y = 'Percent sad') +
  ggsave('sad_timeseries.png', width=12, height=8, units='in', dpi=300)
daily_weighted %>%
  bind_rows(weekly_weighted) %>%
  ggplot(aes(p_sad)) +
  geom_histogram(binwidth = 0.005, fill = blue) +
  geom_vline(aes(xintercept = mean(p_sad)),
             linetype = 'dashed',
             color = '#575757') +
  geom_vline(aes(xintercept = max(p_sad)),
             linetype = 'dashed',
             color = '#575757') +
  geom_segment(data = data.frame(1), 
               y = 2.3, yend = 2.3, x = .298 + .005, xend = .381 - .005, 
               arrow = arrow(length = unit(0.4, "cm"), type = "closed"),
               color = blue, size = 1) +
  coord_flip() +
  theme_Publication() +
  theme(panel.background = element_rect(fill = 'transparent'),
        plot.background = element_rect(fill = 'transparent', color = NA),
        panel.grid.major.x = element_blank(),
        axis.text.y = element_blank()) +
  scale_x_continuous(labels = scales::label_percent(accuracy = 1L),
                     limits = c(.2, .4)) +
  scale_y_continuous(breaks = seq(0, 10, 2)) +
  labs(x = '', y = 'Count') +
  ggsave('sad_histogram.png', width=3, height=8, units='in', dpi=300)