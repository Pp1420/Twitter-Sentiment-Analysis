# Importing the snscrape library's twitter module, which allows us to scrape tweets
import snscrape.modules.twitter as sntwitter

# Importing the pandas library for data manipulation and analysis
import pandas as pd

# Importing the datetime library for working with dates and times
import datetime


query = "mood OR #mood OR feeling OR #feeling OR #emotion OR emotion lang:en until:2022-06-22 since:2022-06-01"
tweets =[]
limit = 50000

for tweet in sntwitter.TwitterSearchScraper(query).get_items():

    if len(tweets) == limit:
        break
    
    else:
        tweets.append([tweet.date, tweet.user.username, tweet.content, tweet.url])

df = pd.DataFrame(tweets, columns=['Date', 'User','Tweet', 'URL'])

print(df)

#store DataFrame to CSV file
df.to_csv('tweetsfinal6.csv', index=False)

print('CSV file successfully saved.')

