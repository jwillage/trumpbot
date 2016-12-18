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

#filter links
links <- grepl("http[s]*", texts)
texts[links] <- gsub("http[s]*://.*", "", texts[links])

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
unigrams$Freq <- unigrams$n/nrow(unigrams)

for (i in 1:nrow(tweet_sentences)) {
  tryCatch(
    x <- ngrams(tokenize(tweet_sentences$sent[i], concatenator = "_") )
    ,error = function(e) 
      print(i)
  )
}

problems <- c(7012, 7339, 10810)

tt_bigrams <- buildTidytextModel(tweet_sentences, 2)
tt_trigrams <- buildTidytextModel(tweet_sentences, 3)
qt_bigrams <- buildQuantedaModel(tweet_sentences, 2, problems)
qt_trigrams <- buildQuantedaModel(tweet_sentences, 3, problems)

gen_tweet(tt_bigrams, tt_trigrams)
gen_tweet(qt_bigrams, qt_trigrams)


### tidytext's tokenization
buildTidytextModel <- function(tweet_sentences, n){
  bi_tweets <- tweet_sentences %>% unnest_tokens(tok, sent, token="ngrams", n=n, to_lower = FALSE)
  bigrams <- bi_tweets %>% group_by(tok) %>% count(sort=T)
  bigrams$Freq <- bigrams$n/nrow(bigrams)
  split <- strsplit(bigrams$tok, " ")
  bigrams$idx <- sapply(split, function(x) paste(x[1:n - 1], collapse = " "))
  bigrams$gram <- sapply(split, function(x) x[n])
  # todo remove garbage ngrams (cont, ...)
  # 'amp' == &
  tryCatch({
    bigrams[bigrams$gram == "amp",]$gram <- "&"
    bigrams$idx <- gsub("amp", "&", bigrams$idx)
  })
  bigrams
}

# quanteda tokenization
buildQuantedaModel <- function(tweet_sentences, n, problems){
  bi_tweets <- ngrams(tokenize(tweet_sentences$sent[-problems], concatenator = "_"), n)
  bigrams <- data.frame(table(unlist(bi_tweets)), stringsAsFactors = F)
  bigrams$Var1 <- as.character(bigrams$Var1)
  split <- strsplit(bigrams$Var1, "_")
  bigrams$idx <- sapply(split, function(x) paste(x[1:n - 1], collapse = " "))
  bigrams$gram <- sapply(split, function(x) x[n])  
  bigrams
}

gen_tweet <- function(bigrams, trigrams) {
  # init with start sentence
  tweet <- list(start="zzstart")
  l <- tweet[[1]]
  noise <- rnorm(nrow(bigrams[bigrams$idx==l,]), mean(bigrams[bigrams$idx==l,]$Freq), 10*sd(bigrams[bigrams$idx==l,]$Freq))
  tweet <- c(tweet,
             bigrams[bigrams$idx == l, ][which.max(bigrams[bigrams$idx==l,]$Freq + noise), "gram"])

  loop = TRUE
  while(loop) {
    l <- paste(tweet[[length(tweet)-1]], tweet[[length(tweet)]])
    if (nrow(trigrams[trigrams$idx==l,]) > 1)
      noise <- rnorm(nrow(trigrams[trigrams$idx==l,]), mean(trigrams[trigrams$idx==l,]$Freq), 10*sd(trigrams[trigrams$idx==l,]$Freq))
    else
      noise <- 0
    tweet <- c(tweet,
               trigrams[trigrams$idx == l, ][which.max(trigrams[trigrams$idx==l,]$Freq + noise), "gram"])
                # [round(runif(1, 1, nrow(trigrams[trigrams$idx == l, ])), 0), "gram"])    
    # shouldnt necessarily stop on end of sentence
    if (tweet[[length(tweet)]] == "zzend")
      loop = FALSE
    loop
    
  }
  tokens <- unlist(tweet)
  paste(tokens[3:length(tokens)-1], collapse=" ")
  
}



  