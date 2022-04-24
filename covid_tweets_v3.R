require(academictwitteR)
require(dplyr)
require(httr)
require(jsonlite)
require(ggplot2)
require(tm)
require(knitr)
require(stringr)
require(wordcloud)

#set_bearer()
get_bearer()

# set working directory
setwd('/Users/ericasmith/Library/Mobile Documents/com~apple~CloudDocs/UB/NUS 691 Adv Info Tech /Project/Tweets/')

# collect tweets
tweets <-
  get_all_tweets(
    query = c("COVID","covid","coronavirus"),
    place = "a307591cd0413588",
    start_tweets = "2020-04-01T00:00:00Z",
    end_tweets = "2022-04-01T00:00:00Z",
    file = "covidtweets",
    n = Inf,
    bind_tweets = FALSE,
    data_path = '/Users/ericasmith/Desktop/Tweets/COVID/'
  )

# bind into dataframe
tweets2 <- bind_tweets(data_path = '/Users/ericasmith/Desktop/Tweets/COVID/', output_format = 'tidy') 

# add date variable
tweets2$date <- as.Date(tweets2$created_at)

# trend dataframe
byDate <- tweets2 %>%
  group_by(date) %>%
  summarize(n_tweets = n_distinct(tweet_id))

# trend plot
trend <- ggplot(data = byDate, aes(x = date, y = n_tweets))+
  geom_line(color = "#00AFBB", size = 2) +
  theme_classic() + 
  ggtitle("COVID Tweet Trend: Buffalo, NY") +
  xlab("Date") +
  ylab("Number of Tweets")
trend

# Word frequency

# Clean up text field - remove special characters
tweets2$text_clean <- str_replace_all(tweets2$text, "[[:punct:]]", "")

### OVERALL
covid_corpus <- VCorpus(VectorSource(tweets2$text_clean))
covid_corpus

# document term matrix
dtm <- DocumentTermMatrix(covid_corpus) # coerces corpus into a Document Term Matrix
inspect(dtm)
dtm

# Sum all columns(words) to get frequency
words_frequency <- colSums(as.matrix(dtm)) 

t <- words_frequency %>%
  data.frame() %>%
  rename('frequency' = '.')

t$word <- row.names(t)

t$totaltweets <- nrow(tweets2)

t$prcnt_tweets <- round(t$frequency / t$totaltweets,2)

# remove stop words
stopwords <- stopwords(kind = 'en')

final_words <- t %>%
  filter(!word %in% stopwords)

### 2020
tweets_2020 <- tweets2 %>%
  filter(date <= '2020-12-31')

covid_corpus_2020 <- VCorpus(VectorSource(tweets_2020$text_clean))

# document term matrix
dtm_2020 <- DocumentTermMatrix(covid_corpus_2020) # coerces corpus into a Document Term Matrix

# Sum all columns(words) to get frequency
words_frequency_2020 <- colSums(as.matrix(dtm_2020)) 

t_2020 <- words_frequency_2020 %>%
  data.frame() %>%
  rename('frequency' = '.')

t_2020$word <- row.names(t_2020)

t_2020$totaltweets <- nrow(tweets_2020)

t_2020$prcnt_tweets <- round(t_2020$frequency / t_2020$totaltweets,2)

final_words_2020 <- t_2020 %>%
  filter(!word %in% stopwords) %>%
  filter(!word %in% c('covid','covid19','coronavirus')) %>%
  filter(prcnt_tweets >= 0.01)

### 2021
tweets_2021 <- tweets2 %>%
  filter(date > '2020-12-31') %>%
  filter(date <= '2021-12-31')

covid_corpus_2021 <- VCorpus(VectorSource(tweets_2021$text_clean))

# document term matrix
dtm_2021 <- DocumentTermMatrix(covid_corpus_2021) # coerces corpus into a Document Term Matrix

# Sum all columns(words) to get frequency
words_frequency_2021 <- colSums(as.matrix(dtm_2021)) 

t_2021 <- words_frequency_2021 %>%
  data.frame() %>%
  rename('frequency' = '.')

t_2021$word <- row.names(t_2021)

t_2021$totaltweets <- nrow(tweets_2021)

t_2021$prcnt_tweets <- round(t_2021$frequency / t_2021$totaltweets,2)

final_words_2021 <- t_2021 %>%
  filter(!word %in% stopwords) %>%
  filter(!word %in% c('covid','covid19','coronavirus')) %>%
  filter(prcnt_tweets >= 0.01)



### 2022
tweets_2022 <- tweets2 %>%
  filter(date >= '2022-01-01')

covid_corpus_2022 <- VCorpus(VectorSource(tweets_2022$text_clean))

# document term matrix
dtm_2022 <- DocumentTermMatrix(covid_corpus_2022) # coerces corpus into a Document Term Matrix

# Sum all columns(words) to get frequency
words_frequency_2022 <- colSums(as.matrix(dtm_2022)) 

t_2022 <- words_frequency_2022 %>%
  data.frame() %>%
  rename('frequency' = '.')

t_2022$word <- row.names(t_2022)

t_2022$totaltweets <- nrow(tweets_2022)

t_2022$prcnt_tweets <- round(t_2022$frequency / t_2022$totaltweets,2)

final_words_2022 <- t_2022 %>%
  filter(!word %in% stopwords) %>%
  filter(!word %in% c('covid','covid19','coronavirus')) %>%
  filter(prcnt_tweets >= 0.01)

### WORD CLOUDS

# 2020
wordcloud(words = final_words_2020$word, freq = final_words_2020$frequency, min.freq = 20,
          max.words=200, random.order=FALSE, rot.per=0.20,scale = c(2,0.5), 
          colors=brewer.pal(8, "Dark2"))

# 2021
wordcloud(words = final_words_2021$word, freq = final_words_2021$frequency,scale = c(2,0.5),
                             min.freq = 5,max.words=200, random.order=FALSE, rot.per=0.20, 
                            colors=brewer.pal(8, "Dark2"))

# 2022
wordcloud(words = final_words_2022$word, freq = final_words_2022$frequency,scale = c(2,0.5),
          min.freq = 5,max.words=200, random.order=FALSE, rot.per=0.20, 
          colors=brewer.pal(8, "Dark2"), gridSize = 75)
