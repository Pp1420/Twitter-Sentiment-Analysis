
##===============================Libraries===========================##


install.packages(c("tidytext", "tidyverse", "DT", "fontawesome", "dplyr", "tm", "knitr", "GGally", "stringr", "tidyr", "sentimentr", "quanteda", "wordcloud", "wordcloud2", "caTools", "syuzhet", "lubridate", "ggplot2", "scales", "reshape2", "rpart", "rpart.plot", "ggrepel", "text2vec", "randomForest", "stats", "e1071", "caret", "lattice", "psych", "shiny", "shinydashboard", "xgboost", "webshot", "htmlwidgets", "gridExtra", "readr", "curl", "shinyauthr", "DT", "viridis"))

install.packages("tinytex")

library(tidytext); library(tidyverse); library(DT); library(fontawesome); library(dplyr); library(tm); library(knitr); library(GGally); library(stringr); library(tidyr); library(sentimentr); library(quanteda); library(wordcloud); library(wordcloud2); library(caTools); library(syuzhet); library(lubridate); library(ggplot2); library(scales); library(reshape2); library(rpart); library(rpart.plot); library(ggrepel); library(text2vec); library(randomForest); library(stats); library(e1071); library(caret); library(lattice); library(psych); library(shiny); library(shinydashboard); library(xgboost); library(webshot); library(htmlwidgets); library(gridExtra); library(readr); library(curl); library(shinyauthr); library(DT); library(stringr); library(viridis)

library(tinytex)

tinytex::install_tinytex()

#=================================Read Data=================================#

# read data
Tweets <- read.csv("/Users/PP/Desktop/tweets/tweetsfinal6.csv")

str(Tweets)

# checks which rows in the object Tweets are complete
# ensure they do not have any missing values
which(!complete.cases(Tweets))

# check structure
dim(Tweets)
str(Tweets)

# creates a new data frame
Tweets1 = subset(Tweets, select = c(Tweet))

# creates a corpus object
corpus <- Corpus(VectorSource(Tweets$Tweet))
print(corpus)

# for presenting before and after pre-processing
before <- data.frame(corpus = sapply(corpus, as.character), stringsAsFactors = FALSE)

#===============================Data Cleaning=============================#

corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)

removeURL <- function(x) gsub('http[[:alnum:]]*', '', x)
corpus <- tm_map(corpus, content_transformer(removeURL))

removeOtherCharacters <- function(x) gsub("([^A-Za-z0-9 ])+", "", x)
corpus <- tm_map(corpus, content_transformer(removeOtherCharacters))

cleanset <- tm_map(corpus, removeWords, stopwords('english'))

after <- data.frame(cleanset = sapply(cleanset, as.character), stringsAsFactors = FALSE)

dim(after)

dim(cleanset)

head(after)

#========================Dashboard Purpose under DATA==========================#


# Add row numbers to after data frame
after_with_rownum <- cbind(TweetNum = seq(nrow(after)), after)

# Rename the second column as "Tweet"
colnames(after_with_rownum)[2] <- "Tweet"

# Show the first 6 rows of the updated data frame
head(after_with_rownum)

dim(after_with_rownum)

second_column <- after_with_rownum[, 2]

num_rows <- length(second_column)

num_rows

#testing

# Calculate the frequency of uppercase letters, lowercase letters, numbers, and special characters in the "before" dataset
before_stats <- before %>% 
  summarise(uppercase = sum(grepl("[A-Z]", before)),
            lowercase = sum(grepl("[a-z]", before)),
            numbers = sum(grepl("[0-9]", before)))

head(before_stats)

# Calculate the frequency of uppercase letters, lowercase letters, numbers, and special characters in the "after" dataset
after_stats <- after %>% 
  summarise(uppercase = sum(grepl("[A-Z]", after)),
            lowercase = sum(grepl("[a-z]", after)),
            numbers = sum(grepl("[0-9]", after)))

head(after_stats)

# Combine the "before" and "after" statistics into a single data frame
stats <- bind_rows(before_stats, after_stats, .id = "dataset")
str(stats)
head(stats)

# Convert the data frame to long format
stats_long <- stats %>% 
  pivot_longer(cols = c("lowercase", "uppercase", "numbers"),
               names_to = "category",
               values_to = "count")

str(stats_long)
stats_long

# Create a bar plot of the frequency of uppercase letters, lowercase letters, and numbers in each dataset
ggplot(stats_long, aes(x = category, y = count, fill = dataset)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Frequency of Text Elements in 'Before' and 'After' Datasets",
       x = "Text Element",
       y = "Count",
       fill = "") +
  theme_light() +
  scale_x_discrete(labels = c("Lowercase", "Uppercase", "Numbers")) +
  scale_fill_manual(labels = c("Before", "After"), 
                    values=c("#A9A9A9", "#FFA07A"))




##until here


#========================Visualization Most Frequent Words==========================#

# Create term document matrix
tdm <- TermDocumentMatrix(cleanset)

# Convert to document term matrix
tdm2 <- as.DocumentTermMatrix(tdm)

# Find terms with frequency greater than or equal to 100
freq_terms <- findFreqTerms(tdm2, lowfreq = 100)

# Print top 10 frequent terms
head(freq_terms, 10)

# Compute word frequencies
word_freqs <- colSums(as.matrix(tdm2))

head(tdm2)

set.seed(222)
# Generate word cloud
hw <- wordcloud(words = names(word_freqs), 
                freq = word_freqs,
                max.words = 200,
                random.order = FALSE,
                min.freq = 5,
                colors = brewer.pal(8, 'Dark2'),
                scale = c(5, 0.3),
                rot.per = 0.7)


# Sort word frequencies in descending order and select top 20 words
top_words <- sort(word_freqs, decreasing = TRUE)[1:20]

# Create data frame with word frequencies
df <- data.frame(words = names(top_words), freq = top_words)

# Create bar plot using ggplot2
ggplot(df, aes(x = words, y = freq, fill = words)) +
  geom_col(width = 0.5, color = "black") +
  labs(title = "Top 20 Words", y = "Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.5, size = 9),
        plot.title = element_text(hjust = 0.5, size = 16),
        legend.position = "none") # Add this line to remove the legend



#===============================Emotion Analysis=============================#


# Create data frame with cleaned tweets
cleantweets <- data.frame(text = sapply(cleanset, as.character), stringsAsFactors = FALSE)

str(cleantweets)

# set the file path and name for the CSV file
file_path <- "/Users/PP/Desktop/tweets/cleaned.csv"

# write the cleantweets data frame to a CSV file
write.csv(cleantweets, file = file_path, row.names = FALSE)

# Convert text encoding to UTF-8
tweets1 <- iconv(cleantweets$text)

# Perform sentiment analysis using NRC sentiment lexicon
s <- get_nrc_sentiment(tweets1)


# Calculate the sum of each emotion category in the sentiment analysis
emotion_bar <- colSums(s)

# Create a data frame containing the count and names of each emotion category
emotion_sums <- data.frame(count=emotion_bar, s = names(emotion_bar))

# Create a bar chart using ggplot2 to visualize the count of each emotion category
ggplot(emotion_sums, aes(x = reorder(s, -count), y = count)) +
  geom_bar(stat = 'identity', fill = "#0072B2", color = "black", width = 0.5) +
  labs(x = "Sentiment Category", y = "Count") + # add axis labels
  ggtitle("Sentiment Scores of Tweets") + # add plot title
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5)) # move title to the middle


# Print the data frame containing the count and names of each emotion category

kable(emotion_sums, 
      caption = "Emotion Categories and their Counts", 
      align = "c")

# Spare
Tweets2 <- as.data.frame(Tweets1)


#===============================Correlation=============================#

# Define a list of keywords to search for in the tweets
keywords <- c("depression","loneliness", "suicidal")

# Find terms that are strongly associated with the list of keywords
for (kw in keywords) {
  cat(paste0("Strongly associated terms with '", kw, "':\n"))
  assoc_terms <- findAssocs(tdm, terms = kw, corlimit = 0.15)
  print(assoc_terms)
}


#graph

# Find terms that are strongly associated with the list of keywords
cor_terms <- lapply(keywords, function(kw) {
  assoc_terms <- findAssocs(tdm, terms = kw, corlimit = 0.15)
  assoc_terms
})

# Combine the associated terms into a single vector
cor_terms <- unlist(cor_terms)

# Remove the original keywords from the list of associated terms
cor_terms <- cor_terms[!cor_terms %in% keywords]

# Convert the TDM to a matrix
tdm_matrix <- as.matrix(tdm[, cor_terms])

library(Matrix)

# Load data
data(mtcars)

# Compute correlation matrix
cor_matrix <- cor(mtcars)

# Convert to sparse matrix
cor_sparse <- sparseMatrix(i = row(cor_matrix), j = col(cor_matrix), x = cor_matrix)

# Remove rows and columns with all NA values
cor_sparse <- cor_sparse[drop = TRUE]

# Convert the sparse matrix back to a regular matrix
cor_matrix <- as.matrix(cor_sparse)

# Melt the matrix into a long format
cor_melted <- melt(cor_matrix)


# Plot the correlation matrix as a heatmap
cor_melted <- melt(cor_matrix)
ggplot(cor_melted, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "#FFFFFF", mid = "#F7A546", high = "#8C2B1E", 
                       midpoint = 0, limits = c(-1, 1), name = "Correlation") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))



#===============================BING Lexicon=============================#

bing <- get_sentiments("bing")

# Extract individual words from tweets and join with bing lexicon sentiment scores
bing_words <- cleantweets %>% 
  unnest_tokens(output = word, input = text) %>%  # split the text into individual words
  inner_join(get_sentiments("bing")) %>%  # join with the bing lexicon sentiment scores
  count(word, sentiment, sort = TRUE)  # count the occurrences of each word for each sentiment

# Identify the top 20 words by sentiment according to the bing lexicon
bingTop20words <- bing_words %>% 
  group_by(sentiment) %>%  # group by sentiment
  slice_max(order_by = n, n = 20) %>%  # get the top 20 words by count for each sentiment
  ungroup() %>% 
  mutate(word = reorder(word, n))  # reorder the words by count for better visualization

# Barplot of word sentiments according to bing lexicon
bingTop20words %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Sentiment" , x = NULL) +
  coord_flip() +
  ggtitle("Top 20 Words Using Bing Lexicon")



cleantweets1 <- cleantweets
cleantweets1$TweetNum <- 1:nrow(cleantweets1)


# Split tweets into individual words, remove numbers, and remove stop words
brkwords <- cleantweets1 %>%
  unnest_tokens(word, text) %>%      
  filter(!grepl('[0-9]', word)) %>%   
  anti_join(stop_words) %>%           
  group_by(TweetNum) %>%             # Group words by tweet number
  ungroup()                          # Remove grouping

tibble(brkwords) %>% print(n=10)


# Sentiment Polarity according to Bing Lexicon
brkBing <- brkwords %>%
  left_join(get_sentiments(lexicon = "bing"), by = "word") %>%
  mutate(sentiment = ifelse(is.na(sentiment), "neutral", sentiment)) %>%
  group_by(TweetNum) %>%
  ungroup()

brkBing %>%print(n = 20)

# Count the number of words in each tweet with Bing sentiment scores

brkBingTweet <- brkBing %>%  
  count(TweetNum, sentiment) 

# 'n' represents the count of how many times a particular sentiment 
# (positive, negative, or neutral) appears in each TweetNum
brkBingTweet

brkBingTweet <- brkBing %>%  
  count(TweetNum, sentiment) %>%
  spread(key = sentiment, value = n) %>%
  select("TweetNum", "positive", "negative", "neutral") 
brkBingTweet %>% print(n=20)

brkBingTweet[is.na(brkBingTweet)] <- 0

brkBingTweet <- brkBing %>%  
  count(TweetNum, sentiment) %>%
  spread(key = sentiment, value = n) %>%
  mutate(positive = ifelse(is.na(positive), 0, positive),
         negative = ifelse(is.na(negative), 0, negative),
         neutral = ifelse(is.na(neutral), 0, neutral)) %>%
  mutate(Total = (positive + negative + neutral)) %>%   
  mutate(Score = ((positive - negative) / Total)) %>% 
  select("TweetNum", "positive", "negative", "neutral", "Total", "Score")

# E.g. score of -0.286, indicating that the overall sentiment of the tweet is negative
brkBingTweet %>% print(n=20)


# Sentiment Score Chart for Bing Lexicon
ggplot(brkBingTweet, aes(TweetNum, Score, fill = Score > 0)) +
  geom_col(show.legend = FALSE) +
  scale_fill_manual(values = c("lightcoral", "lightblue")) +
  labs(x = "Tweet Number", y = "Score",
       title = "Bing Sentiment Scores for Twitter Tweets") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  ylim(-0.7, 0.7)


# See positive sentiment words
brkBing %>%
  filter(sentiment == "positive") %>%
  count(word, sentiment, sort = TRUE)

# See negative sentiment words
brkBing %>%
  filter(sentiment == "negative") %>%
  count(word, sentiment, sort = TRUE)

brkBing_filtered <- brkBing %>%
  left_join(brkBingTweet, by = "TweetNum") %>%
  filter(sentiment == "negative") %>%
  filter(word == "overwhelmed") %>%
  filter(Score > 0)

# Overwhelmed is contained in a positive tweet by many times
brkBing_filtered %>%
  count(word, sentiment, sort = TRUE) %>%
  print(n=5)



#===============================AFINN Lexicon=============================#

# Load the brkwords dataset and join it with the AFINN lexicon to get the sentiment score for each word

afinn_words <- cleantweets %>% 
  unnest_tokens(output = word, input = text) %>%  # split the text into individual words
  inner_join(get_sentiments(lexicon = "afinn"), by = "word")%>%   # join with the AFINN lexicon sentiment scores
  rename(sentiment = value) 
  
# Identify the top 20 words by sentiment according to the AFINN lexicon
afinnTop20words <- afinn_words %>% 
  group_by(sentiment) %>%  # group by sentiment
  count(word, sort = TRUE) %>% # count the occurrences of each word for each sentiment
  slice_max(order_by = n, n = 20) %>%  # get the top 20 words by count for each sentiment
  ungroup() %>% 
  mutate(word = reorder(word, n))  # reorder the words by count for better visualization

afinnTop20words %>%
  arrange(desc(n))

afinnTop20words %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Sentiment" , x = NULL) +
  coord_flip() +
  ggtitle("Top 20 Words Using AFINN Lexicon") +
  labs(subtitle = "AFINN scores range from -5 (most negative) to 5 (most positive)") +
  ylim(0, 1000)


# Summarize the sentiment score for each tweet by grouping by TweetNum and taking the sum of sentiment scores
# Store the results in the brkAFINNTweet dataset
brkAFINNTweet <- brkAFINN %>%
  group_by(TweetNum) %>%
  summarise(afinn = sum(sentiment)) %>%
  ungroup()

# Print the first 10 rows of the resulting brkAFINNTweet dataset
brkAFINNTweet %>% print(n=10)


# Sentiment Score Chart for AFINN lexicon
ggplot(brkAFINNTweet, aes(TweetNum, afinn, fill = afinn>0,)) +
  geom_col(show.legend = FALSE) +
  scale_fill_manual(values=c("lightcoral","lightsteelblue3"))+
  labs(x= "Tweet Number", y = "Sentiment Score",
       title = "AFINN Sentiment Scores for Twitter Tweets")+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  ylim(-15, 15)



# Calculating the total sentiment score for each word by multiplying its 
# frequency by its sentiment score
# positive feeling is on the top since it has largest sentiment scores

brkAFINN %>%
  count(word, sentiment) %>% # count the frequency of each word-sentiment pair in brkAFINN
  mutate(total = sentiment * n) %>% # calculate the total score for each word-sentiment pair
  arrange(desc(total)) # sort the rows by the total score in ascending order

# negative
brkAFINN %>%
  count(word, sentiment) %>% 
  mutate(total = sentiment * n) %>% 
  arrange(total)

# The first row in the brkGood dataset has a TweetNum of 37019
# and an afinn score of -76 (lowest), 
# which means that this tweet has a highly negative sentiment.
brkGood <- brkAFINNTweet %>%
  filter(afinn < 0) %>%   
  group_by(TweetNum) %>%            
  arrange(afinn) %>%
  ungroup()

brkGood %>% print(n=10)



#===============================NRC Lexicon=============================#

# Extract individual words from tweets and join with nrc sentiment lexicon scores
nrc_words1 <- cleantweets %>% 
  unnest_tokens(output = word, input = text) %>%  # split the text into individual words
  inner_join(get_sentiments("nrc")) %>%  # join with the nrc lexicon sentiment scores
  count(word, sentiment, sort = TRUE)  # count the occurrences of each word for each sentiment

# Top 20 words by sentiment according to nrc lexicon
nrcTop20words <- nrc_words1 %>%
  group_by(sentiment) %>%
  slice_max(order_by = n, n = 20) %>%
  ungroup() %>%
  mutate(word = reorder(word, n))  # reorder the words by count for better visualization

# Barplot of word sentiments according to nrc lexicon
nrcTop20words %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Sentiment" , x = NULL) +
  coord_flip() +
  ggtitle("Top 20 Words Using NRC Lexicon")
  
# Sentiment Polarity according to NRC Lexicon
nrc <- get_sentiments("nrc") %>%
  filter(sentiment == "negative" | sentiment == "positive")

brkNRC <- brkwords %>%
  left_join(nrc, by = "word") %>%
  mutate(sentiment = ifelse(is.na(sentiment), "neutral", sentiment)) %>%
  group_by(TweetNum) %>%
  ungroup()

brkNRC %>% print(n=10)

brkNRCTweet <- brkNRC %>%  
  count(TweetNum, sentiment) %>%
  spread(key = sentiment, value = n) %>%
  mutate(positive = ifelse(is.na(positive), 0, positive),
         negative = ifelse(is.na(negative), 0, negative),
         neutral = ifelse(is.na(neutral), 0, neutral)) %>%
  mutate(Total = (positive + negative + neutral)) %>%  
  mutate(NRC = ((positive - negative) / Total)) %>% 
  select("TweetNum", "positive", "negative", "neutral", "Total", "NRC")

brkNRCTweet %>% print(n=20)


#Sentiment chart for NRC lexicon
ggplot(brkNRCTweet, aes(TweetNum, NRC, fill = NRC > 0)) +
  geom_col(show.legend = FALSE) +
  scale_fill_manual(values = c("lightcoral","lightsteelblue3")) +
  labs(x= NULL,y=NULL,
       title="NRC Sentiment Scores for Twitter Tweets")+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  ylim(-0.7, 0.7)

brkNRC %>%
  filter(sentiment == "negative") %>%
  count(word, sentiment, sort = TRUE)


#===============================Syuzhet Lexicon=============================#

syuzhet <- get_sentiment_dictionary("syuzhet")

brkSyu <- brkwords %>%      
  inner_join(syuzhet, by = "word") 
brkSyu

#negative
brkSyuTop <- brkSyu %>%
  count(word, value) %>%
  mutate(impact = value * n) %>%
  arrange(impact)
brkSyuTop

# Top 20 words by sentiment according to Syuzhet lexicon
SyuzhetTop20 <- brkSyuTop %>%
  slice_min(order_by = impact, n = 20) %>%
  slice_max(order_by = impact, n = 20) %>%
  ungroup() %>%
  mutate(word = reorder(word, n))

# Barplot of word sentiments according to Syuzhet lexicon


# Top 20 negative words by impact according to Syuzhet lexicon
neg_syuzhet <- brkSyuTop %>%
  filter(impact < 0) %>%
  slice_min(order_by = impact, n = 20) %>%
  ungroup() %>%
  mutate(word = reorder(word, n))

# Top 20 positive words by impact according to Syuzhet lexicon
pos_syuzhet <- brkSyuTop %>%
  filter(impact > 0) %>%
  slice_max(order_by = impact, n = 20) %>%
  ungroup() %>%
  mutate(word = reorder(word, n))

# Create separate plots for negative and positive words
ggplot() +
  geom_col(data = neg_syuzhet, aes(word, n, fill = impact), show.legend = FALSE) +
  scale_fill_gradient(low = "red", high = "green") +
  labs(y = "Impact" , x = NULL) +
  coord_flip() +
  ggtitle("Top 20 Negative Words Using Syuzhet Lexicon")

ggplot() +
  geom_col(data = pos_syuzhet, aes(word, n, fill = impact), show.legend = FALSE) +
  scale_fill_gradient(low = "red", high = "green") +
  labs(y = "Impact" , x = NULL) +
  coord_flip() +
  ggtitle("Top 20 Positive Words Using Syuzhet Lexicon")




# Sentiment polarity Syuzhet lexicon
syuzhet <- get_sentiment_dictionary("syuzhet")

syuzhet

brkSyuTweet <- brkSyu %>%
  group_by(TweetNum) %>%
  summarise(syuzhet = sum(value)) %>%
  ungroup()

brkSyuTweet %>% print(n=10)

# Sentiment chart for Syuzhet lexicon
ggplot(brkSyuTweet, aes(TweetNum, syuzhet, fill = syuzhet > 0)) +
  geom_col(show.legend = FALSE) +
  scale_fill_manual(values = c("lightcoral", "lightsteelblue3")) +
  labs(x = "Tweet Number", y = "Score",
       title = "Syuzhet Sentiment Scores for Twitter Tweets") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  ylim(-5, 5)


brkSyuTop <- brkSyu %>%
  count(word, value) %>%
  mutate(impact = value * n) %>%
  arrange(impact)
brkSyuTop


#========================Prepare for Algorithm======================#

str(brkSyuTweet)

brkSyuTweetTest <- merge(cleantweets1,brkSyuTweet, by="TweetNum")
brkSyuTweetTest <- brkSyuTweetTest %>% 
  mutate(Sentiment = case_when(
    syuzhet < 0 ~ "Negative",
    syuzhet > 0 ~ "Positive",
    syuzhet == 0 ~ "Neutral"
  ))

brkSyuTweetTest$Sentiment <- as.factor(brkSyuTweetTest$Sentiment) 

str(brkSyuTweetTest)

num_SyuTweet <- brkSyuTweetTest



brkSyuTweetTest <- subset(brkSyuTweetTest, select = -c(TweetNum))

table(brkSyuTweetTest$Sentiment)

# Store counts of negative and positive sentiment in new variables
# Use for dashboard

#table_result <- table(brkSyuTweetTest$Sentiment)
#num_pos_tweets <- table_result["Positive"]

#class(num_SyuTweet)

#num_pos_tweets = as.numeric(num_pos_tweets)
#num_SyuTweet = as.numeric(num_SyuTweet)

#num_SyuTweet <- brkSyuTweetTest$Sentiment %>% table() %>% sum()

#perc_pos_tweets <- round((num_pos_tweets / num_SyuTweet) * 100, 2)

num_pos_tweets <- table(brkSyuTweetTest$Sentiment)[["Positive"]]
num_SyuTweet <- nrow(brkSyuTweetTest)
perc_pos_tweets <- round((num_pos_tweets / num_SyuTweet) * 100, 2)



#Density Plot
brkSyuTweetTest %>% 
  ggplot(aes(x = syuzhet, fill = Sentiment)) +
  geom_density(alpha = 0.7, color = 'black') +
  ggtitle("Density Plot") +
  xlim(-10, 10)

#===============================Algorithm=============================#

#===============================Naive Bayes=============================#

# Naive Bayes Prediction Model
str(brkSyuTweetTest$Sentiment)

set.seed(1234)

split <- sample.split(brkSyuTweetTest, SplitRatio = 0.7)
SyuTrain <- subset(brkSyuTweetTest, split == "TRUE")
SyuTest <- subset(brkSyuTweetTest, split == "FALSE")

table(SyuTest$Sentiment)

SyuModel <- naiveBayes(Sentiment ~ ., data = SyuTrain)

SyuPred <- predict(SyuModel, newdata = SyuTest)

SyuCM <- table(SyuTest$Sentiment, SyuPred)

confusionMatrix(SyuCM)  

cm <- confusionMatrix(SyuCM)
accuracyNB <- cm$overall[1]

cm

SyuCM

# Heatmap

# convert confusion matrix to data frame and add rownames
SyuCM_df <- as.data.frame.matrix(SyuCM)
SyuCM_df$True_Sentiment <- rownames(SyuCM_df)

# Convert data frame to long format
SyuCM_long <- reshape2::melt(SyuCM_df)

# Create heatmap
ggplot(SyuCM_long, aes(x = variable, y = True_Sentiment, fill = value)) +
  geom_tile() +
  geom_text(aes(label = value), color = "black") +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(title = "Confusion Matrix Heatmap",
       x = "Predicted Sentiment",
       y = "True Sentiment")




#===============================SVM=============================#

# SVM Prediction Model
set.seed(1234)

brkSyuTweetTest2 <- brkSyuTweetTest

str(brkSyuTweetTest2)

class(brkSyuTweetTest2$Sentiment) = "Numeric"

brkSyuTweetTest2$Sentiment = cut(brkSyuTweetTest2$Sentiment, 3, labels = c('Negative', 'Neutral','Positive'))

str(brkSyuTweetTest2)

brkSyuTweetTest2 <- subset(brkSyuTweetTest2, select = -c(text))

tail(brkSyuTweetTest2)


split1 <- sample.split(brkSyuTweetTest2, SplitRatio = 0.70)
SyuTrain1 <- subset(brkSyuTweetTest2, split1 == "TRUE")
SyuTest1 <- subset(brkSyuTweetTest2, split1 == "FALSE")

SyuTrain1[-2] = scale(SyuTrain1[-2])
SyuTest1[-2] = scale(SyuTest1[-2])

str(SyuTrain1)

classifier = svm(formula = Sentiment ~ .,
                 data = SyuTrain1,
                 type = 'C-classification',
                 kernel = 'linear')

SyuPred1 = predict(classifier, newdata =  SyuTest1[-2])

confusionMatrix1 = table(SyuTest1[, 2], SyuPred1, dnn=c("Actual", "Predicted"))

confusionMatrix1

#testing heatmap for SVM

library(ggplot2)
library(reshape2)

# Convert the confusion matrix to a data frame
conf_mat <- as.matrix(confusionMatrix1)
conf_df <- melt(conf_mat)

# Rename the columns
names(conf_df) <- c("Actual", "Predicted", "Count")

# Convert the actual and predicted values to factors
conf_df$Actual <- factor(conf_df$Actual, levels = c("Negative", "Neutral", "Positive"))
conf_df$Predicted <- factor(conf_df$Predicted, levels = c("Negative", "Neutral", "Positive"))

# Create a heatmap of the confusion matrix
ggplot(conf_df, aes(x = Predicted, y = Actual, fill = Count)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Count)) +
  scale_fill_gradient(low = "white", high = "red") +
  theme_bw() +
  ggtitle("Confusion Matrix") +
  xlab("Predicted Sentiment") +
  ylab("Actual Sentiment")


#plot(SyuPred1, SyuTest1[, 2])

confusionMatrix1 <- confusionMatrix(SyuPred1, SyuTest1$Sentiment, positive = "positive")

accuracySVM <- confusionMatrix1$overall[1]

confusionMatrix1


#===============================XGBOOST=============================#

# Set seed for reproducibility
set.seed(1234)

# Create a corpus from the text data
XGCorpus <- VCorpus(VectorSource(brkSyuTweetTest$text))

# Create a document term matrix
XGtdm <- DocumentTermMatrix(XGCorpus)

# Set maximum virtual memory size for the R process
R_MAX_VSIZE = 100000

# Convert the document term matrix to a data frame
XGdatamod = as.matrix(XGtdm) %>% as.data.frame()

# Remove terms that are too sparse
XGtdm2 = removeSparseTerms(XGtdm, sparse = 0.99)

# Convert the document term matrix to a data frame
XGdatamod2 = as.matrix(XGtdm2) %>% as.data.frame()


brkSyuTweetTest3 <- brkSyuTweetTest

class(brkSyuTweetTest3$Sentiment) = "Numeric"


#brkSyuTweetTest3 <-replace(brkSyuTweetTest3$Sentiment, brkSyuTweetTest3$Sentiment = 3,1)

# Convert the sentiment column to a factor with labels 0 negative, 1 neutral, 2 positive
brkSyuTweetTest3$Sentiment = with(brkSyuTweetTest3, factor(Sentiment, labels = c(0,1,2)))


# Print the structure of the data frame
str(brkSyuTweetTest3)

# Add the sentiment column to the XG data frame
XGdatamod2$Sentiment = brkSyuTweetTest3$Sentiment

# Create a vector of row indices for the validation set
XGvalidRows = createDataPartition(y = XGdatamod2$Sentiment, p = 0.1)$Resample1

# Create a data frame for the validation set
XGvalidData = XGdatamod2[XGvalidRows,]

# Create a data frame for the training and testing set
XGtrainTest = XGdatamod2[-XGvalidRows,]

# Split the training and testing set into a 70/30 ratio
XGsampleTrainTest = createDataPartition(y = XGtrainTest$Sentiment, p = 0.7)

# Create the training set input and output
XGtrainX = as.matrix(XGtrainTest[XGsampleTrainTest$Resample1, -ncol(XGtrainTest)])
XGtrainY = XGtrainTest$Sentiment[XGsampleTrainTest$Resample1] %>% as.character() %>% as.numeric()

# Create the testing set input and output
XGtestX = as.matrix(XGtrainTest[-XGsampleTrainTest$Resample1, -ncol(XGtrainTest)])
XGtestY = XGtrainTest$Sentiment[-XGsampleTrainTest$Resample1] %>% as.character() %>% as.numeric()

# Convert the training input and output to a format for the XGBoost model
xgb.train = xgb.DMatrix(data = XGtrainX, label = XGtrainY)

# Convert the testing input and output to a format for the XGBoost model
xgb.test = xgb.DMatrix(data = XGtestX, label = XGtestY)

# Set the watchlist for the XGBoost model
watchlist <- list(train = xgb.train, test = xgb.test)

# Set XGBoost parameters
parameters <- list(booster = "gbtree", 
                   objective = "multi:softmax", 
                   num_class = 3,
                   eval_metric = list("mlogloss", "accuracy"), 
                   max.depth = floor(sqrt(ncol(XGdatamod2))))

# Perform cross-validation using xgb.cv()
XGbcv <- xgb.cv(params = parameters, 
                data = xgb.train, 
                nrounds = 100, 
                nfold = 5, 
                showsd = TRUE, 
                stratified = TRUE, 
                print_every_n = 10, 
                early_stopping_rounds = 5, 
                maximize = FALSE, 
                prediction = TRUE)

# Find the number of rounds with the lowest cross-validation error
round.max = which.min(XGbcv$evaluation_log$test_mlogloss_mean)

# train XGBoost model on training data
XGBmod <- xgb.train(data = xgb.train, max.depth = floor(sqrt(ncol(XGdatamod2))),
                    eta = 0.1, nthread = 4, nrounds=round.max,metric = "accuracy",
                    num_class=3,watchlist = watchlist, objective = "multi:softmax")

# make predictions on training and testing data using the trained model
XGpred_train = as.factor(predict(XGBmod, XGtrainX))
XGpred_test = as.factor(predict(XGBmod, XGtestX))

# evaluate model performance on training set
confusionMatrix(XGpred_train, as.factor(XGtrainY))

# evaluate model performance on testing set
confusionMatrix(XGpred_test, as.factor(XGtestY))

# predict sentiment on validation set
XGpred_Valid = as.factor(predict(XGBmod, as.matrix(XGvalidData[, -ncol(XGvalidData)])))

# evaluate model performance on validation set
confusionMatrix(XGpred_Valid, as.factor(XGvalidData$Sentiment))

conf_matrix <- confusionMatrix(XGpred_Valid, as.factor(XGvalidData$Sentiment))
accuracyXG <- conf_matrix$overall[1]



# heatmap 

# create factor variables with the same levels
XGpred_Valid_factor <- factor(XGpred_Valid, levels = c("0", "1", "2"))
XGvalidData_factor <- factor(XGvalidData$Sentiment, levels = c("0", "1", "2"))

# create confusion matrix
conf_matrix <- confusionMatrix(XGpred_Valid_factor, XGvalidData_factor)

conf_matrix

# Convert confusion matrix to data frame
conf_dfXG <- as.data.frame.matrix(as.matrix(conf_matrix))

# Rename row and column names
colnames(conf_dfXG) <- c("Negative", "Neutral", "Positive")
rownames(conf_dfXG) <- c("Negative", "Neutral", "Positive")


# Convert the confusion matrix to a data frame
conf_matXG <- as.matrix(conf_dfXG)

conf_dfXG_melt <- melt(conf_matXG)

names(conf_dfXG_melt) <- c("Actual", "Predicted", "Count")


# Create a heatmap of the confusion matrix
ggplot(conf_dfXG_melt, aes(x = Predicted, y = Actual, fill = Count)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Count)) +
  scale_fill_gradient(low = "white", high = "green") +
  theme_bw() +
  ggtitle("Confusion Matrix") +
  xlab("Predicted Sentiment") +
  ylab("Actual Sentiment")









#Summary of Accuracy
accuracyNB
accuracySVM
accuracyXG

accuracyNB_perc <- accuracyNB * 100
accuracySVM_perc <- accuracySVM * 100
accuracyXG_perc <- accuracyXG * 100


#for dashboard
# Store the accuracies as a named vector
accuraciesdash <- c("Naive Bayes" = accuracyNB_perc, "SVM" = accuracySVM_perc, "XGBoost" = accuracyXG_perc)
accuraciesdash


accuracyhigh <- max(accuraciesdash)
accuracyhigh

#endfordashboard

# create a data frame with model names and accuracies
model_names <- c("Naive Bayes", "SVM", "XGBoost")
accuracies <- c(accuracyNB, accuracySVM, accuracyXG)
accuracy_df <- data.frame(Model = model_names, Accuracy = accuracies)

# create the plot

ggplot(data = accuracy_df, aes(x = model_names, y = accuracies*100)) +
  geom_bar(stat = "identity", fill = "skyblue", width = 0.5) +
  geom_text(aes(label = paste0(round(accuracies * 100, 1), "%")), vjust = -0.5, size = 4) +
  labs(x = "", y = "Accuracy(%)", title = "Comparison of Model Accuracies") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.text = element_text(size = 12), 
        axis.title = element_text(size = 14, face = "bold"))


# comparison between lexicons
bing
str(bing)

nrc
str(nrc)

syuzhet
library(tibble)
syuzhet_tib <- as_tibble(syuzhet)

str(syuzhet_tib)


syuzhet_tib <- syuzhet_tib %>% 
  mutate(sentiment = ifelse(value < 0, "negative", ifelse(value > 0, "positive", "neutral")))


syuzhet_tib <- syuzhet_tib %>% 
  select(-value)

str(syuzhet_tib)

# create a list of tibbles
tib_list <- list(bing, nrc, syuzhet_tib)

# count the number of rows for each tibble and create a data frame
tib_rows <- tibble(
  tibble_name = c("bing", "nrc", "syuzhet_tib"),
  num_rows_tib = sapply(tib_list, nrow)
)


tib_rows <- tib_rows %>% 
  mutate(tibble_name = case_when(
    tibble_name == "bing" ~ "Bing",
    tibble_name == "nrc" ~ "NRC",
    tibble_name == "syuzhet_tib" ~ "Syuzhet",
    TRUE ~ tibble_name
  ))


# create a bar plot
ggplot(tib_rows, aes(x = tibble_name, y = num_rows_tib)) +
  geom_bar(stat = "identity") +
  ylab("Words Stored") +
  xlab("Lexicons") +
  ggtitle("Comparison of Words in Lexicons")



# allow user input
# tweet matches with Syuzhet

SyuTweet <- brkSyuTweetTest
str(SyuTweet)

SyuTweet1 <- subset(SyuTweet, select = -c(syuzhet))
str(SyuTweet1)
head(SyuTweet1)


# add Tweet Num

SyuTweet1 <- SyuTweet1 %>% 
  mutate("TweetNum" = row_number()) %>% 
  rename(Text = text)  %>% 
  select("TweetNum", "Text", "Sentiment")

str(SyuTweet1)
head(SyuTweet1)

SyuTweet1[1,]

syuzhet_tib

library(shinyauthr)
unloadNamespace("shinyauthr")
library(shinyauthr)

library(sodium) 


# dataframe that holds usernames, passwords and other user data
user_base <- tibble::tibble(
  user = c("user1", "user2"),
  password = c("pass1", "pass2"),
  permissions = c("admin", "standard"),
  name = c("User One", "User Two")
)


