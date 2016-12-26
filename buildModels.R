library(quanteda)
library(tidytext)
library(dplyr)

tweets <- readRDS("data/trump_tweets.RDS")

# tokenize into sentences
tweet_sentences <- tweets %>% unnest_tokens(sent, text, token="sentences", to_lower=F)

# append start-sentence and end-sentence tokens
tweet_sentences$sent <- paste("zzstart", tweet_sentences$sent, "zzend")

# build tidytext bi/tri-gram models
tt_tokens_2 <- tokenizeTidytext(tweet_sentences, n=2, lower=FALSE)
tt_tokens_3 <- tokenizeTidytext(tweet_sentences, n=3, lower=FALSE)
tt_bigrams <- buildNgramModel(tt_tokens_2, n=2, " ")
tt_trigrams <- buildNgramModel(tt_tokens_3, n=3, " ")

# index is always converted to lowercase, while the next gram is left in it's incoming state
tt_bigrams$idx <- tolower(tt_bigrams$idx)
tt_trigrams$idx <- tolower(tt_trigrams$idx)

# quanteda had an issue with some characters, which will be removed
for (i in 1:nrow(tweet_sentences)) {
  tryCatch(
    x <- ngrams(tokenize(tweet_sentences$sent[i], concatenator = "_") )
    ,error = function(e) 
      print(i)
  )
}

problems <- c(7012, 7339, 10810)
# build quanteda-tokenized models
qt_tokens_2 <- tokenizeQuanteda(tweet_sentences, n=2, remove=problems)
qt_tokens_3 <- tokenizeQuanteda(tweet_sentences, n=3, remove=problems)
qt_bigrams <- buildNgramModel(qt_tokens_2, n=2, "_")
qt_trigrams <- buildNgramModel(qt_tokens_3, n=3, "_")




# add some regular text to give a give model more variety and sentence/grammatical structure
supp <- readLines("data/en_US.blogs.txt")
supp_df <- data.frame(supp, stringsAsFactors = F)
supp_sentences <- supp_df %>% unnest_tokens(sent, supp, token="sentences")
supp_sentences$sent <- paste("zzstart", supp_sentences$sent, "zzend")

supp_tokens_2 <- tokenizeTidytext(supp_sentences$sent, n=2, lower=FALSE)
supp_tokens_3 <- tokenizeTidytext(supp_sentences$sent, n=3, lower=FALSE)
tt_supp_bigrams <- buildNgramModel(supp_tokens_2, n=2)
tt_supp_trigrams <- buildNgramModel(supp_tokens_3, n=3)

# todo weight down the n instead of freq, since freq will be recalculatd after models are bound
tt_supp_bigrams$Freq <- tt_supp_bigrams$Freq * 0.15
tt_supp_trigrams$Freq <- tt_supp_trigrams$Freq * 0.15

# aggregate models by index and gram
tt_bi_all <- rbind(tt_bigrams, tt_supp_bigrams_weighted)
tt_tri_all <- rbind(tt_trigrams, tt_supp_trigrams_weighted)
tt_bi_all <- rbind(tt_bigrams, tt_supp_bigrams_weighted)
tt_tri_all <- rbind(tt_trigrams, tt_supp_trigrams_weighted)

gen_tweet(tt_bigrams, tt_trigrams)
gen_tweet(tt_bigrams, tt_trigrams)
gen_tweet(qt_bigrams, qt_trigrams)
gen_tweet(tt_bi_all, tt_tri_all)


# tidytext's tokenization
tokenizeTidytext <- function(tweet_sentences, n, lower){
  grams <- tweet_sentences %>% 
    unnest_tokens(tok, sent, token="ngrams", n=n, to_lower = lower) %>%
    group_by(tok) %>% 
    count(sort=T)
  
 # grams$Freq <- grams$n/nrow(grams)
  grams$Freq <- grams$n/sum(grams$n)
  grams
}

# quanteda tokenization
tokenizeQuanteda <- function(tweet_sentences, n, remove){
  tweets <- ngrams(tokenize(tweet_sentences$sent[-remove], concatenator = "_"), n)
  grams <- data.frame(table(unlist(tweets)), stringsAsFactors = F)
  grams$tok <- as.character(grams$Var1)
  grams$n <- grams$Freq
  #grams$Freq <- grams$n/nrow(grams)
  grams$Freq <- grams$n/sum(grams$n)
  grams
}

buildNgramModel <- function(model, n, concat) {
  split <- strsplit(model$tok, concat)
  model$idx <- sapply(split, function(x) paste(x[1:n - 1], collapse = " "))
  model$gram <- sapply(split, function(x) x[n])
  # todo remove garbage ngrams (cont, ...)
  # 'amp' == &, todo this should be done early on 
  # tryCatch({
  #   model[model$gram == "amp",]$gram <- "&"
  #   model$idx <- gsub("amp", "&", model$idx)
  # })
  model
}

gen_tweet <- function(bigrams, trigrams) {
  # init with start sentence
  tweet <- list(start="zzstart")
  l <- tweet[[1]]
  noise <- rnorm(nrow(bigrams[bigrams$idx==tolower(l),]), mean(bigrams[bigrams$idx==tolower(l),]$Freq), 10*sd(bigrams[bigrams$idx==tolower(l),]$Freq))
  tweet <- c(tweet,
             bigrams[bigrams$idx == l, ][which.max(bigrams[bigrams$idx==tolower(l),]$Freq + noise), "gram"])

  loop = TRUE
  while(loop) {
    l <- tolower(paste(tweet[[length(tweet)-1]], tweet[[length(tweet)]]))
    if (nrow(trigrams[trigrams$idx==tolower(l),]) > 1)
      noise <- rnorm(nrow(trigrams[trigrams$idx==tolower(l),]), mean(trigrams[trigrams$idx==tolower(l),]$Freq), 10*sd(trigrams[trigrams$idx==tolower(l),]$Freq))
    else
      noise <- 0
    tweet <- c(tweet,
               trigrams[trigrams$idx == l, ][which.max(trigrams[trigrams$idx==tolower(l),]$Freq + noise), "gram"])
                # [round(runif(1, 1, nrow(trigrams[trigrams$idx == l, ])), 0), "gram"])    
    # shouldnt necessarily stop on end of sentence
    if (tweet[[length(tweet)]] == "zzend")
      loop = FALSE
    loop
  #  print(tweet)
  }
  
  # need to end on a trigram (bi's not making sense)
  tokens <- unlist(tweet)
  paste(tokens[3:length(tokens)-1], collapse=" ")
  
}



  