library(tidyverse)
library(janitor)
library(ggplot2)
library(lubridate)
library(surveillance)

df <- read_delim("data/RKI_COVID19_Simplified_2020-07-29.csv", delim = ",") %>%
  janitor::clean_names() %>%
  mutate(delay = reporting_date - symptom_start_date)

#######
## 1 ##
#######

# The number of daily cases by Symtom_Start_Date for June and July 2020

## symptom_start_date plot
df %>%
  filter(symptom_start_date >= '2020-06-01') %>%
  group_by(symptom_start_date) %>%
  summarise(cases = sum(cases)) %>%
  ggplot(aes(symptom_start_date, cases)) + geom_line() + stat_smooth(method = loess)

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

a %>%
  ggplot() + geom_line(aes(x=symptom_start_date,y=imp_cases),color='red') + 
  geom_line(aes(x=symptom_start_date,y=under_reported_cases),color='blue') + 
  ylab('Values')+xlab('date')








%>%
  filter(symptom_start_date_imp >= '2020-06-01') %>% 
  group_by(symptom_start_date_imp) %>%
  summarise(cases = sum(cases)) %>%
  ggplot(aes(symptom_start_date_imp, cases)) + geom_line()

  


library(tidyverse)
library(janitor)
library(lubridate)
library(surveillance)

## Import DF 
df <- read_delim("data/RKI_COVID19_Simplified_2020-07-29.csv", delim = ",") %>%
  janitor::clean_names() %>%
  mutate(delay = reporting_date - symptom_start_date) %>%
  # filter positive cases, some cases are negative; it might mean there are positive cases that were
  # not covid at the end?
  filter(cases > 0) %>%
  # filter for >= 2020-06-01 to the end 
  filter(symptom_start_date >= "2020-06-01") %>%
  select(symptom_start_date, reporting_date, cases) %>%
  uncount(cases)

df <- as.data.frame(df)

# Control variables of the nowcast 
# only do nowcasts for the last max_delay days!

now <- max(df$reporting_date) 
max_delay <- 14
safePredictLag <- 0

so_range <- c(min(df$symptom_start_date, na.rm = TRUE), now)

#Fix nowcast time points so they don't depend on the imputed data.
nowcastDates <- seq(from = now - safePredictLag - max_delay,
                    to = now - safePredictLag, by = 1)

# create a "survelliance time series" object, quite self-explanatory

sts <- linelist2sts(
  as.data.frame(df),
  dateCol = "symptom_start_date",
  aggregate.by = "1 day", 
  dRange = so_range)


nc.control <- list(
  N.tInf.max = 4e3,
  #Perform Bayesian nowcasting assuming the delay distribution
  # is stable over time via a Poisson / Gamma
  N.tInf.prior = structure("poisgamma",
                           mean.lambda = mean(observed(sts)),
                           var.lambda = 5 * var(observed(sts))
  ),
  predPMF = TRUE,
  dRange = so_range)

nc <- nowcast(now = now, when = nowcastDates, data = as.data.frame(df),
              dEventCol = "symptom_start_date",
              dReportCol = "reporting_date",
              aggregate.by = "1 day",
              D = 30, 
              ## Assume constant delay distribution, but only within the last m=14 days
              method = "bayes.trunc",
              ## only use last 14 days for the delay estimation
              m = 30,
              control = nc.control
)


##Convert to tibble (in wide format)
nc_tidy <- nc %>% 
  as_tibble() %>% 
  # Add prediction interval
  mutate(pi_lower = nc@pi[,,1],  pi_upper=  nc@pi[,,2]) %>% 
  # Return only time points which were nowcasted.
  filter(epoch %in% nowcastDates) %>% 
  # Restrict to relevant columns
  select(date = epoch,
         observed,
         predicted = upperbound,
         predicted_lower= pi_lower,
         predicted_upper = pi_upper ) %>% 
  # Reduce nowcast objects to only showing results during last 2 weeks
  # A consequence of using D=14 is that older delays than 14 days are not 
  # adjusted at all.
  filter(date > (max(date) - weeks(6))) %>% 
  mutate(obnyr = predicted - observed)

nc_df <- nc_tidy

nc_tidy <- nc_tidy %>% 
  select(date, observed, obnyr) %>% 
  gather(key, value, - date) %>% 
  ungroup()


# last case of linelist to filter nowcast data
last_linelist_case <- df %>% 
  summarise(n = n(),
            last_case = max(reporting_date)) %>% 
  pull(last_case)



# Plot nowcasts with corresponding prediction intervals
nc_tidy %>% 
  mutate(key = case_when(key == "obnyr" ~ "nowcast",
                         TRUE ~ key)) %>%
  filter(date <= ymd(last_linelist_case)) %>%
  ggplot(aes(date, value)) +
  geom_col(aes(fill = key), alpha = 0.9)  +
  geom_errorbar(
    data = (nc_df %>%
              mutate(value = 1, key = NA) %>%
              filter(date > (max(date) - weeks(10)))), 
    aes(ymin = predicted_lower, ymax = predicted_upper),
    width = 0.2, size = 1) +
  scale_fill_manual(values = c("#ff7f00", "#377eb8")) +
  scale_y_continuous(labels = scales::number_format(accuracy = 1)) +
  scale_x_date(date_breaks = "1 days") +
  labs(x = "",
       y = "Cases") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))





