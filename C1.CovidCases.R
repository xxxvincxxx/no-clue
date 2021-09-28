library(tidyverse)
library(janitor)
library(ggplot2)
library(lubridate)
library(surveillance)

df <- read_delim("data/RKI_COVID19_Simplified_2020-07-29.csv", delim = ",")

df %>%
  janitor::clean_names() %>%
  ## I create the delay variable:
  mutate(delay = reporting_date - symptom_start_date) %>%
  mutate(delay = as.numeric(delay)) %>%
  # filter positive cases, some cases are negative; 
  # I also remove delays that are extremely large
  filter(cases > 0) %>%
  filter(delay < 14) -> df


df %>%
  group_by(date = reporting_date) %>%
  summarise(reported_cases = sum(cases)) -> df_rep

df %>%
  group_by(date = symptom_start_date) %>%
  summarise(symptom_onset = sum(cases)) -> df_sym

df_sym %>%
  left_join(df_rep) %>%
  replace(is.na(.), 0) -> a

## ggplottin'...

a %>% 
  filter(date >= "2020-06-01") %>%
  pivot_longer(
    -date
  ) %>%
  mutate(date = as.Date(date)) %>% 
  ggplot(aes(x=date, y=value, fill=name))+
  geom_col()+
  scale_fill_manual(values = c("#e4424e", "#18A6C5"))+
  scale_x_date(date_labels = "%b %d %Y") +
  theme(axis.text.x = element_text(angle = 25, vjust = 0.5, hjust=1)) +
  theme(legend.position="bottom") +
  labs(x ="", y = "Number of reported cases")+ 
  labs(caption="Onset of symptoms, alternatively report date") + 
  theme(plot.caption = element_text(hjust=0.5, size=rel(1.2)))
