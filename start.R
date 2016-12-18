library(twitteR)
library(dplyr)
library(tidytext)
library(ggplot2)
library(quanteda)


setup_twitter_oauth(consumer_key, consumer_secret, access_token, token_secret)

all_tweets <- userTimeline("realdonaldtrump", n = 3200, includeRts = F)
source <- sapply(tweets, function(y) sub("</a>", "", sub("<(.*?)>", "", y$statusSource)))
# there was a post by @drob a couple months ago on how all trumps pesonal tweets are from iphone
# tweets from android were from his campaign team and not in the classic trump voice
iphone <- which(source == "Twitter for iPhone")
all_texts <- sapply(tweets, function(x) x$text)
texts <- all_texts[iphone]
tweets <- data.frame(text = texts, line = 1:length(iphone), stringsAsFactors = FALSE)

tidy_tweets <- tweets %>% unnest_tokens(word, text)
#tidy_tweets <- tidy_tweets %>% anti_join(stop_words)

tidy_tweets %>% count(word, sort=T) %>%
  anti_join(stop_words) %>%
  filter(n > 15) %>% mutate(word=reorder(word, n)) %>%
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

utf8 <- stringi::stri_enc_isutf8(texts)
texts <- texts[utf8]
tweets <- data.frame(text = texts, line = 1:length(texts), stringsAsFactors = FALSE)
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
  
  # start a random sentence
  # tweet <- c(tweet,
  #            bigrams[bigrams$idx == "zzstart", ]
  #            [round(runif(1, 1, nrow(bigrams[bigrams$idx == "zzstart", ])), 0), "gram"])
  loop = TRUE
  while(loop) {
    tweet <- c(tweet,
               bigrams[bigrams$idx == tweet[[length(tweet)]], ]
               [round(runif(1, 1, nrow(bigrams[bigrams$idx == tweet[[length(tweet)]], ])), 0), "gram"])    
    if (tweet[[length(tweet)]] == "zzend")
      loop = FALSE
    
  }
  tokens <- unlist(tweet)
  paste(tokens[3:length(tokens)-1], collapse=" ")
  
}

  