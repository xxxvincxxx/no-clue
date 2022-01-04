library(tidyverse)

# Create df

df <- data.frame(stringsAsFactors=FALSE,
          type = c("A", "A", "B", "B"),
      platform = c("iOS", "Android", "iOS", "Android"),
        clicks = c(174000, 526000, 540000, 160000),
   conversions = c(16200, 38400, 46800, 11000))

# Calculate conversion rates 

df %>%
  mutate(cvr = round(conversions/clicks*100,2)) 


# 
