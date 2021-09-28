## Generate random sample 
## Add random seed for replicability

## https://www.db-fiddle.com/f/q3MF5Uanrgme5HFQcP2mK9/0
## https://stackoverflow.com/questions/70878642/cumulative-calculation-in-window-functions/70880004?noredirect=1#comment125305841_70880004

library(tidyverse)
library(lubridate)

set.seed(100)

### Table users
# 1. user_id - VARCHAR(40)
# 2. first_seen - TIMESTAMP 

first_seen <- sample(seq(as.Date('2018/08/20'), 
                         as.Date('2018/10/01'),
                         by="day"), 100, 
                         replace = T)
user_id <- as.character(round(runif(n = 100, min = 1000, max = 2000),0))
users <- data.frame(user_id, first_seen)

# extract week and count
users %>%
  mutate(week = floor_date(first_seen, "weeks", week_start = 1)) %>%
  group_by(week) %>%
  summarise(n = n())

### Table paid_subscription
# 1. user_id - VARCHAR(40)
# 2. event_time - TIMESTAMP 
# 3. subscription_product_id - VARCHAR(40)

# subset of user_id 
users %>%
  sample_frac(0.2) %>%
  mutate(event_time = )




## B.1 - Write a SQL query that aggregates subscribers into weekly cohorts based on first_seen 
# and calculates a cumulative subscription conversion rate at a given set of offsets 
# from first_seen. The offsets are 1, 30, 90 and 360 days and we call the respective 
# cumulative conversion rates D1, D30, D90 and D360. So D30 for a weekly cohort is the fraction of 
# users from that cohort who subscribed in the first 30 days after their first_seen.


## solve it with Sqldf
# https://www.rdocumentation.org/packages/sqldf/versions/0.4-11

