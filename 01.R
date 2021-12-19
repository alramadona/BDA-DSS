# https://docs.ropensci.org/rtweet/
# install.packages("rtweet")
library(rtweet)


# Search tweets or users --------------------------------------------------

tweets_omicron <- search_tweets("omicron",
                                n=18000, 
                                retryonratelimit=FALSE, 
                                include_rts=FALSE) # rate limits: 18,000 every 15 minutes
save_as_csv(tweets_omicron, "data/tweets_omicron.csv")
# ?search_tweets

users_UGM <- search_users("universitas gadjah mada", 
                          n=1000) # search for 1,000 users with the #rstats in their profile
save_as_csv(users_UGM, "data/users_UGM.csv")


# Stream tweets -----------------------------------------------------------
# randomly sample (approximately 1%) from the live stream of all tweets

tweets_streamID <- stream_tweets(
  c(95,-11, 141,6),
  timeout = 60*60 # 30 minutes
)
save_as_csv(tweets_streamID, "data/tweets_streamID.csv")


# Get timelines -----------------------------------------------------------

tweets_jokowi <- get_timelines("jokowi", 
                               n=3200)
save_as_csv(tweets_jokowi, "data/tweets_jokowi.csv")


# Save images -------------------------------------------------------------

# save.image("data/tweets.RData")
# load("data/tweets.RData")


# Exploratory Data Analysis -----------------------------------------------

## library
#install.packages("dplyr")
library(dplyr)
#install.packages("ggplot2")
library(ggplot2)
#install.packages("igraph")
library(igraph)
#install.packages("tidytext")
library(tidytext)
#install.packages("tm")
library(tm)
#install.packages("SnowballC")
library(SnowballC)

load("data/tweets.RData")

ts_plot(tweets_omicron, by="mins")

tweets_omicron_net <- network_graph(tweets_omicron[c(1:10),])
plot(tweets_omicron_net)

head(tweets_omicron$text, 10)

### remove http elements manually
tweets_omicron$stripped_text <- gsub("http.*","",  tweets_omicron$text)
tweets_omicron$stripped_text <- gsub("https.*","", tweets_omicron$stripped_text)

head(tweets_omicron$stripped_text, 10)

### remove punctuation, convert to lowercase, add id for each tweet!
tweets_text <- tweets_omicron %>%
  dplyr::select(stripped_text) %>%
  unnest_tokens(word, stripped_text)

### load list of stop words - from the tidytext package
data("stop_words")
### view first 6 words
head(stop_words)

nrow(tweets_text)
### [1] 376743

### remove stop words from your list of words
tweets_text <- tweets_text %>%
  anti_join(stop_words)

### there should be fewer words now
nrow(tweets_text)
### [1] 255768

# plot the top 20 words
tweets_text %>%
  dplyr::count(word, sort = TRUE) %>%
  top_n(20) %>% 
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = "Count of unique words found in tweets",
       subtitle = "Stop words removed from the list") +
  theme_bw()

#install.packages("wordcloud")
library(wordcloud)

set.seed(3)

tweets_text <- tweets_text %>%
  dplyr::count(word, sort = TRUE)

tweets_text$n_char <- nchar(tweets_text$word)

tweets_text <- subset(tweets_text, n_char>=3)
tweets_text <- subset(tweets_text, n>=100)

tweets_text <- tweets_text[-c(1:2),]

wordcloud(tweets_text$word, tweets_text$n, random.order = FALSE, random.color = TRUE,
          rot.per = 0.3, colors = 1:nrow(tweets_text))