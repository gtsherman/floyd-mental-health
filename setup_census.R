# Load libraries
library(tidyverse)
library(psych)


# Load Floyd week data
con = DBI::dbConnect(RMariaDB::MariaDB(), dbname = 'household_pulse',
                     host = '127.0.0.1', user = '', password = '')
pulse_floyd = tbl(con, 'pulse2020_puf_05') %>%
    select(SCRAM, gad2_sum, phq2_sum, is_black, is_white,
           is_hispanic, is_asian, WEEK, state, PWEIGHT,
           INCOME, is_female, age_in_years, EEDUC) %>%
    as_tibble()

# Load pre-Floyd data
pulse_base = tibble()
for (i in seq(4)) {
  df = tbl(con, paste0('pulse2020_puf_0', i)) %>%
    select(SCRAM, gad2_sum, phq2_sum, is_black, is_white,
           is_hispanic, is_asian, WEEK, state, PWEIGHT,
           INCOME, is_female, age_in_years, EEDUC) %>%
    as_tibble()
  pulse_base = rbind(pulse_base, df)
}

DBI::dbDisconnect(con)

# Combine Floyd and pre-Floyd data with identifier
pulse = pulse_floyd %>%
  bind_rows(pulse_base) %>%
  mutate(floyd_weekOrNot = if_else(WEEK == 5, 1, 0),
         PWEIGHT = as.numeric(PWEIGHT))


#' Weighted Cohen's d
#' 
#' Computes a Cohen's d effect size using survey weights to calculate the weighted mean and variance.
#' 
#' @param x The data vector, numeric.
#' @param groups A vector containing two groups, characters or factors, of the same size as \code{x}.
#' @param w A weight vector, numeric, of the same size as \code{x}.
#' @return A tibble containing the weighted Cohen's d estimate, the weighted standard error, and the size of each group.
weighted_cohens_d = function(x, groups, w, na.rm = T) {
  df = tibble(x = x, g = groups, w = w)
  d = df %>%
    group_by(g) %>%
    summarize(m = weighted.mean(x, w, na.rm = {{ na.rm }}),
              v = Hmisc::wtd.var(x, w, na.rm = {{ na.rm }}),
              sd = sqrt(v)) %>%
    ungroup() %>%
    summarize(m = diff(m),
              sd = sqrt(sum(sd^2) / length(sd))) %>%
    summarize(d = m / sd) %>%
    .$d
  
  groups = unique(groups)
  g1 = groups[1]
  g2 = groups[2]
  
  n1 = df %>%
    drop_na() %>%
    filter(g == {{ g1 }}) %>%
    summarize(n = n()) %>%
    .$n
  n2 = df %>%
    drop_na() %>%
    filter(g == {{ g2 }}) %>%
    summarize(n = n()) %>%
    .$n
  
  se = cohen.d.ci(d, n1, n2)
  se = (se[2] - se[1]) / 1.96
  
  return(tibble(d = d, se = se, n1 = n1, n2 = n2))
}