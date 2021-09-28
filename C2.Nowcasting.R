library(tidyverse)
library(janitor)
library(lubridate)
library(surveillance)

## Import DF 
df <- read_delim("data/RKI_COVID19_Simplified_2020-07-29.csv", delim = ",") %>%
  janitor::clean_names() %>%
  mutate(delay = reporting_date - symptom_start_date) %>%
  # filter positive cases, some cases are negative
  filter(cases > 0) %>%
  # filter for >= 2020-06-01 to the end 
  filter(symptom_start_date >= "2020-06-01") %>%
  select(symptom_start_date, reporting_date, cases) %>%
  # the data is presented as "count", I use "uncount"
  # in order to transform this time series to the individual report level
  uncount(cases)

df <- as.data.frame(df)

# Only do nowcasts for the last max_delay days!

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
              ## Assume constant delay distribution, but only within the last m=30 days
              method = "bayes.trunc",
              ## only use last 30 days for the delay estimation
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
  # Reduce nowcast objects to only showing results during last 30
  # A consequence of using D=30 is that older delays than 30 days are not 
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
