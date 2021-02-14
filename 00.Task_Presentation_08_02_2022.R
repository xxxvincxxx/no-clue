##############
### MSG CVR ##
##############
library(tidyverse)
library(janitor)
library(ggplot2)
library(lubridate)
library(surveillance)

# Define the structure of the matrix
# y as a dummy 1,0 
y <- rep(0:1, c(4, 4))

# Define the "treatment" AKA the message received by
# the user (A,B) and their "platform" (ios, Android)

trt <- rep(c("A", "B"), 4)
platform <- rep(c("IOS", "IOS", "Android", "Android"), 2)

# Values
all_cnt <- c(174000, 540000, 526000, 160000)
# "Views"
subscriptions <- c(16200, 46800, 38400, 11000)

non_subscriptions <- all_cnt - subscriptions
weight <- c(non_subscriptions, subscriptions)

df <- tibble(
  y,
  trt = factor(trt, levels = c("B", "A")),
  platform,
  weight
)

df

## Basically make this INDIVIDUAL LEVEL!!!
df_long <- df %>% 
  # operate row-wise
  rowwise() %>% 
  # add unique ids
  mutate(ids = list(seq(1, weight))) %>% 
  # use unnest() to make this ids visible in the mutate
  unnest(ids)

## Run the regression on the "long" df 
summary(lm(y ~ trt + platform, df_long)) 

## Analyze:
# T_A
# IOS
## Significant !!!
## for Both: trtA = 0.54%, Message A 
## seems to be better on iOS and on Android, and on Both

###########################
### SQL Window Functions ##
###########################








#################
### Nowcasting ##
#################

library(tidyverse)
library(janitor)
library(ggplot2)
library(lubridate)
library(surveillance)

## https://github.com/hoehleatsu/STA427_SM4IDE/blob/main/Lectures/Lecture04/lecture04-handout.pdf

df <- read_delim("data/RKI_COVID19_Simplified_2020-07-29.csv", delim = ",")

## ## ## ## ## ## ## ## ## ## 
## histogram of the delay  ## 
## ## ## ## ## ## ## ## ## ## 

df %>%
  janitor::clean_names() %>%
  ## Create the delay variable:
  mutate(delay = reporting_date - symptom_start_date) %>%
  mutate(delay = as.numeric(delay)) -> delay_df

## Graph the delay
hist(delay_df$delay)
summary(delay_df$delay)

########
## C1 ## 
########  

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

## ## ## ## ## ## ## ## ##
## Nowcasting Approach  ## 
## ## ## ## ## ## ## ## ##

df <- read_delim("data/RKI_COVID19_Simplified_2020-07-29.csv", delim = ",") %>%
  janitor::clean_names() %>%
  mutate(delay = reporting_date - symptom_start_date) %>%
  # filter positive cases, some cases are negative
  filter(cases > 0) %>%
  filter(symptom_start_date >= "2020-06-01") %>%
  select(symptom_start_date, reporting_date, cases) %>%
  # the data is presented as "count", I use "uncount"
  ## -> in order to transform this time series to the INDIVIDUAL REPORT LEVEL
  uncount(cases)

df <- as.data.frame(df)

# Only do nowcasts for the last max_delay days!
now <- max(df$reporting_date) 
max_delay <- 30
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

###############################################################################
# A list with named arguments controlling the functionality of the nowcasting #
###############################################################################

nc.control <- list(
  # 4 to the power of 3 = 4000
  N.tInf.max = 4e3,
  ## Delay prior is modeled as stable over time via a Poisson / Gamma
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
              # Selecting the method:
              ## "bayes.trunc"
              ## -> 
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
  # A consequence of using D=30 is that older delays than 30 days are not adjusted at all.
  filter(date > (max(date) - weeks(6))) %>% 
  mutate(pred_obs = predicted - observed)

nc_df <- nc_tidy
nc_tidy <- nc_tidy %>% 
  select(date, observed, pred_obs) %>% 
  gather(key, value, - date) %>% 
  ungroup()

# take the last case observed
last_linelist_case <- df %>% 
  summarise(n = n(),
            last_case = max(reporting_date)) %>% 
  pull(last_case)

# Plot nowcasts + CI
nc_tidy %>% 
  mutate(key = case_when(key == "pred_obs" ~ "nowcast",
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
  scale_x_date(date_breaks = "4 days") +
  labs(x = "", y = "Cases") +
  theme(axis.title.x = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))
