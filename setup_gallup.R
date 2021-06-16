# Load required libraries
library(tidyverse)
library(lubridate)
library(ggsci)
library(us_map)


# Load data
con = DBI::dbConnect(RMariaDB::MariaDB(), dbname = 'gallup_covid')
individual_anger = tbl(con, 'old_hasAngBefAug17_recodedEmoRaceGenPartyAgeIncEdu_v4_02_15') %>% as_tibble()
individual_sadness = tbl(con, 'old_hasSadBefAug17_recodedEmoRaceGenPartyAge_v3_02_15') %>% as_tibble()
week = tbl(con, 'old_hasAngBefAug17_recodedEmoRaceGenPartyAge_v3_02_15_byWeek') %>% as_tibble()
DBI::dbDisconnect(con)

# Get the data into shape:
# * Filter (through joins) to responses with both sadness and anger
# * Select the columns of interest
# * Bucket the education values (other values have already been bucketed)
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
