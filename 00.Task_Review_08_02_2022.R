##############
### MSG CVR ##
##############
library(tidyverse)

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


