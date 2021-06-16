library(tidyverse)
library(ggsci)
library(ggsignif)
library(lubridate)
library(usmap)


n_reps = 10000

#' Change in difference
#' 
#' Calculates the difference of the change in mean between two categories.
#' 
#' @section Difference of change in mean:
#' We want to compare the difference of means between two categories, i.e.,
#' is the change from before Floyd's death to after significantly different
#' for one category compared to another. This function first calculates the
#' change in mean for each category, and then calculates the difference between
#' them.
#' 
#' @param indiv_long A long format tibble containing the values to compare.
#' @param c1,c2 The names of the categories to compare.
#' @param emo Specifies which emotion we want to calculate this difference for.
#' @return The difference between the change of means, numeric
get_diff = function(indiv_long, c1, c2, emo = 'WEE_angerF') {
  indiv_long %>%
    filter(value == {{ c1 }} | value == {{ c2 }}) %>%
    group_by(value, floyd_weekOrNot) %>%
    summarize(across(c(WEC_sadF, WEE_angerF), ~mean(., na.rm = T)),
              .groups = 'drop') %>%
    ungroup() %>%
    select(value, floyd_weekOrNot, WEC_sadF, WEE_angerF) %>%
    gather('emotion', 'emo_val', WEC_sadF, WEE_angerF) %>%
    spread(floyd_weekOrNot, -value) %>%
    mutate(change = `1` - `0`) %>%
    select(emotion, value, change) %>%
    spread(value, change) %>%
    mutate(diff = .[[c1]] - .[[c2]]) %>%
    filter(emotion == {{ emo }}) %>%
    .$diff
}

#' Bootstrap difference
#' 
#' Run a single bootstrap iteration to calculate the difference in change of means.
#' 
#' Uses the built-in \code{sample} method to sample with replacement. These samples
#' are performed independently for each of the two categories.
#' 
#' See \code{\link{get_diff}} for a description of the difference in change of means.
get_bootstrap_diff = function(indiv_long, c1, c2, s, emo = 'WEE_angerF') {
  indiv_long %>%
    filter(value == {{ c1 }} | value == {{ c2 }}) %>%
    group_by(value) %>%
    mutate(WEC_sadF = sample(WEC_sadF, replace = T),
           WEE_angerF = sample(WEE_angerF, replace = T)) %>%
    ungroup() %>%
    get_diff(c1, c2, emo)
}