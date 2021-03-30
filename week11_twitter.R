# Week 11. Working with Twitter API
# AB
# 30 March 2021

library(tidyverse)
library(rtweet)
library(tidytext)
library(wordcloud)

# See vignettes at https://cran.r-project.org/web/packages/rtweet/

# First you need to apply for access to API at https://developer.twitter.com/

# Once authorised create an app at https://developer.twitter.com/

# Authorisation

api_key <- "SECRET CODE HERE"
api_secret_key <- "SECRET CODE HERE"
access_token <- "SECRET CODE HERE"
access_token_secret <- "SECRET CODE HERE"


token <- create_token(
  app = "Data3ExeterUni",
  consumer_key = api_key,
  consumer_secret = api_secret_key,
  access_token = access_token,
  access_secret = access_token_secret)


# Get 1,000  latest tweets from Boris Johnson and Keir Starmer

df <- get_timelines(c("BorisJohnson", "Keir_Starmer"), n = 1000)


# Clean the data

# Download stop words
data("stop_words")
stop_words

wordCounts <- df %>%
  select(screen_name, text) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by = "word") %>%  # remove stop words
  filter(!str_detect(word, "https")) %>% # remove "https"
  filter(!str_detect(word, "t.co")) %>% # remove "t.co"
  filter(!str_detect(word, "amp")) %>% # remove "amp"
  filter(!str_detect(word, "[:digit:]")) %>% # remove digits
  group_by(screen_name) %>%
  count(word, sort = TRUE)

wordCounts %>% print(n = 100)

# Word clouds

BJ <- wordCounts %>%
  filter(screen_name == "BorisJohnson") 

KS <- wordCounts %>%
  filter(screen_name == "Keir_Starmer") 

# Word cloud for BJ

wordcloud(words = BJ$word, freq = BJ$n, min.freq = 1, max.words = 200,
          random.order = FALSE, rot.per = 0.35, colors=brewer.pal(8, "Dark2"))

wordcloud(words = KS$word, freq = KS$n, min.freq = 1, max.words = 200,
          random.order = FALSE, rot.per = 0.35, colors=brewer.pal(8, "Dark2"))


# Retrieve a list of accounts BJ and KS follow.

bj_follows <- get_friends("BorisJohnson")
bj_follows

ks_follows <- get_friends("Keir_Starmer")
ks_follows

# Are there any intersections?

bj_follows %>%
  inner_join(ks_follows, by = "user_id") %>%
  pull(user_id) %>%
  lookup_users() %>%
  pull(screen_name)

# 18 accounts

# Please see more at https://cran.r-project.org/web/packages/rtweet/vignettes/intro.html








