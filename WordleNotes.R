# Notes for wordle visualisations

library(tidyverse)
library(tidytext)
library(gutenbergr)
library(tm)
library(wordcloud)
library(textstem)
lotta.text <- gutenberg_download(31847)
lotta.text <- lotta.text %>%
  select (-gutenberg_id)
# Or - tidytext it first then use wordle at the end?

lotta.un <- lotta.text %>%
  unnest_tokens(word, text)
lotta.un$lemword <- lemmatize_strings(lotta.un$word)


lotta.corpus <- Corpus(VectorSource(lotta.un$lemword))
dtm.lotta <- DocumentTermMatrix(lotta.corpus)
lottawordle <- wordcloud(lotta.corpus, scale = c(5,0.5), max.words = 100, random.order = FALSE, 
                    random.color = FALSE, rot.per = 0, use.r.layout = FALSE)

