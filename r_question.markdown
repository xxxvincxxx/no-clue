Hello!
  
  I have some problems understanding the results of this online experiment.

I sent my website users 2 types of messages that can result in a subscription or not.

The users can belong to two different platform categories (iOS / Android).

The results are the following:
  
  &#x200B;
  
  ||Message A|Message B|
  |:-|:-|:-|
  |iOS|**9.3%** (16200 subscriptions / 174000 messages sent)|8,7% (46800/540000)|
  |Android|**7.3%** (38400 subscriptions / 526000 messages sent)|6.9% (11000 / 160000)|
  |Both|7.8% (54600 subscriptions / 700000 messages sent)|**8.3%** (57800  / 700000)|
  
  Message A seems to be better on iOS and on Android, in terms of subscriptions yielded. But overall message B looks like the better option.

How do I interpret the result?
  
  What kind of test should I perform?
  aa
  
  I used the formula as:
  
  prop.test(x = c(54600, 57800), n = c(700000, 700000))

on the "Both" category but I still am not sure which kind of message could be sent to all users. 

Thanks for your help!