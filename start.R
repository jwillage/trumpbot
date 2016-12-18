library(twitteR)
library(dplyr)
library(tidytext)
library(ggplot2)
library(quanteda)
library(jsonlite)

creds <- readLines("creds.txt")
setup_twitter_oauth(creds[1], creds[2], creds[3], creds[4])

all_tweets <- userTimeline("realdonaldtrump", n = 3200, includeRts = F)

updateTweets <- function(all_tweets){
  recent <- userTimeline("realdonaldtrump", n = 3200, includeRts = F, sinceID = all_tweets[[1]]$id)
  all_tweets <- c(all_tweets, recent)
  all_tweets
}

getArchive <- function(years) {
  dfs <- lapply(years, function(x) 
          fromJSON(paste0("http://www.trumptwitterarchive.com/data/", x, ".json")))
  bind_rows(dfs)
}

archived <- getArchive(2010:2016)
# http://varianceexplained.org/r/trump-tweets/ suggests iphone == trump, android == staff
# tweets from other sources appear to be in the style of trump
archived <- archived[archived$source != "Twitter for Android", ]
texts <- archived$text
tweets <- data.frame(text = texts, line = 1:length(texts), stringsAsFactors = FALSE)

tidy_tweets <- tweets %>% unnest_tokens(word, text)
#tidy_tweets <- tidy_tweets %>% anti_join(stop_words)
tidy_tweets %>% count(word, sort=T) %>%
  anti_join(stop_words) %>%
  filter(n > 200) %>% mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_bar(stat="identity") +
  coord_flip()

sentiments <- tidy_tweets %>% inner_join(get_sentiments("nrc")) %>% count(word, sentiment, sort=T) %>% ungroup()
  
sentiments %>% 
  filter(n > 1, sentiment %in% c("positive", "negative")) %>%
  mutate(n = ifelse(sentiment=="negative", -n, n)) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_bar(stat="identity") +
  coord_flip()

# break down for NLP
tweet_sentences <- tweets %>% unnest_tokens(sent, text, token="sentences")
tweet_sentences$sent <- paste("zzstart", tweet_sentences$sent, "zzend")
uni_tweets <- tweet_sentences %>% unnest_tokens(word, sent, to_lower = FALSE)
unigrams <- uni_tweets %>% group_by(word) %>% count(sort=T)
unigrams$freq <- unigrams$n/nrow(unigrams)

bi_tweets <- tweet_sentences %>% unnest_tokens(tok, sent, token="ngrams", n=2, to_lower = FALSE)
bigrams <- bi_tweets %>% group_by(tok) %>% count(sort=T)
bigrams$freq <- bigrams$n/nrow(bigrams)
# todo remove garbage ngrams
# 'amp' == &


n = 2
split <- strsplit(bigrams$tok, " ")
bigrams$idx <- sapply(split, function(x) paste(x[1:n - 1], collapse = " "))
bigrams$gram <- sapply(split, function(x) x[n])

gen_tweet()

gen_tweet <- function() {
  tweet <- list(start="zzstart")
  loop = TRUE
  while(loop) {
    l <- tweet[[length(tweet)]]
    tweet <- c(tweet,
               bigrams[bigrams$idx == l, ]
               [round(runif(1, 1, nrow(bigrams[bigrams$idx == l, ])), 0), "gram"])    
    if (tweet[[length(tweet)]] == "zzend")
      loop = FALSE
    
  }
  tokens <- unlist(tweet)
  paste(tokens[3:length(tokens)-1], collapse=" ")
  
}

  