library(tidyverse)
library(broom)
#library(infer)
#install.packages("gtsummary")

# Create df

df <- data.frame(stringsAsFactors=FALSE,
          type = c("A", "A", "B", "B"),
      setting = c("platform_1", "platform_2", "platform_1", "platform_2"),
        clicks = c(174000, 526000, 540000, 160000),
   conversions = c(16200, 38400, 46800, 11000))

# Calculate conversion rates 

df %>%
  mutate(prop = round(conversions/clicks*100,1)) -> df

# reshape data
df %>%
  spread()


# Aggregate per type
df %>%
  group_by(type) %>%
  summarise(conversions = sum(conversions),
            clicks = sum(clicks)) %>%
  mutate(cvr = round(conversions / clicks*100,1)) -> df_agg

# Apply propotion test on aggregate df 

prop.test(x = c(54600, 57800), n = c(700000, 700000))

## -> Simpson Paradox
### The question has no clear answer



##### Dataset

y <- rep(0:1, c(4, 4))
trt <- rep(c("A", "B"), 4)
platform <- rep(c("IOS", "IOS", "Android", "Android"), 2)
all_cnt <- c(174000, 540000, 526000, 160000)
subscriptions <- c(16200, 46800, 38400, 11000)
non_subscriptions <- all_cnt - subscriptions

weight <- c(non_subscriptions, subscriptions)

df <- tibble(
  y,
  trt = factor(trt, levels = c("B", "A")),
  platform,
  weight
)


