library(twitteR)
library(dplyr)
library(tidytext)
library(ggplot2)

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

#filter links
links <- grepl("http[s]*", texts)
texts[links] <- gsub("http[s]*://.*", "", texts[links])

tweets <- data.frame(text = texts, line = 1:length(texts), stringsAsFactors = FALSE)
saveRDS(tweets, "data/trump_tweets.RDS")
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

