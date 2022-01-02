library(tidyverse)
library(janitor)
library(ggplot2)
library(lubridate)

# read file
df <- read_delim("data/RKI_COVID19_Simplified_2020-07-29.csv", delim = ",") %>%
  janitor::clean_names() 

df %>%
  mutate(delay = reporting_date - symptom_start_date) -> df

#######
## 1 ##
#######

# The number of daily cases by Symtom_Start_Date for June and July 2020

## symptom_start_date plot
df %>%
  filter(symptom_start_date >= '2020-06-01') %>%
  group_by(symptom_start_date) %>%
  summarise(cases = sum(cases)) %>%
  ggplot(aes(symptom_start_date, cases)) + geom_line()

df %>%
  filter(symptom_start_date >= '2020-06-01') %>%
  drop_na(symptom_start_date) %>%
  group_by(symptom_start_date) %>%
  summarise(under_reported_cases = sum(cases)) -> df_under

## Problem: Imputatation of NA 
## Check the delay distribution

df %>%
  select(delay) %>%
  filter(!is.na(delay))  -> a

median_onset = median(a$delay)

df %>%
  filter(reporting_date >= '2020-07-01') %>%
  select(reporting_date, symptom_start_date, cases) %>%
  mutate(symptom_start_date_imp = if_else(is.na(symptom_start_date)==T,
                                          reporting_date - 3,
                                          symptom_start_date)) -> a
  







df %>%
  mutate(symptom_start_date_imp = if_else(is.na(symptom_start_date)==T,
                                          reporting_date - 3,
                                          symptom_start_date)) %>%
  group_by(symptom_start_date_imp) %>%
  summarise(imp_cases = sum(cases)) %>%
  filter(symptom_start_date_imp >= '2020-06-01') %>%
  rename(symptom_start_date = symptom_start_date_imp) -> df_imp

# join 

df_imp %>%
  left_join(df_under) -> a


%>%
  ggplot() + geom_line(aes(x=symptom_start_date,y=imp_cases),color='red') + 
  geom_line(aes(x=symptom_start_date,y=under_reported_cases),color='blue') + 
  ylab('Values')+xlab('date')








%>%
  filter(symptom_start_date_imp >= '2020-06-01') %>% 
  group_by(symptom_start_date_imp) %>%
  summarise(cases = sum(cases)) %>%
  ggplot(aes(symptom_start_date_imp, cases)) + geom_line()

  








