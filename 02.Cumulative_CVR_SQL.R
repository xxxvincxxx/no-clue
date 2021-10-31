## Generate random sample 

### Table users
# 1. user_id - VARCHAR(40)
# 2. first_seen - TIMESTAMP 

## Mettiamo 5 settimane tipo:
#### 2018-08-20 + 5 weeks


### Table paid_subscription
# 1. user_id - VARCHAR(40)
# 2. event_time - TIMESTAMP 
# 3. subscription_product_id - VARCHAR(40)




## B.1 - Write a SQL query that aggregates subscribers into weekly cohorts based on first_seen 
# and calculates a cumulative subscription conversion rate at a given set of offsets 
# from first_seen. The offsets are 1, 30, 90 and 360 days and we call the respective 
# cumulative conversion rates D1, D30, D90 and D360. So D30 for a weekly cohort is the fraction of 
# users from that cohort who subscribed in the first 30 days after their first_seen.
