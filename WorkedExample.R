# Worked full text example

library(tidyverse)
library(tidytext)
library(gutenbergr)
library(tm)
library(wordcloud)
library(textstem)
lotta.text <- gutenberg_download(31847)
lotta.text <- lotta.text %>%
  select (-gutenberg_id)
otta.un <- lotta.text %>%
  unnest_tokens(word, text)
lotta.un$lemword <- lemmatize_strings(lotta.un$word)



lotta_freq <- lotta.un %>%
  anti_join(stop_words) %>%
  filter(lemword != "NA")%>%
  count(lemword, sort = TRUE) %>%
  top_n (10) %>%
  mutate(text_order = nrow(.):1) %>%
  ggplot(aes(reorder(lemword, text_order), n)) +
  geom_bar (stat = "identity") +
  labs (x = "Word", y = "Frequency in example data") +
  coord_flip() +
  theme_classic()
lotta_freq



# Bigrams

lotta.bi <- lotta.text %>%
  unnest_tokens(bigram, text, token ="ngrams", n = 2)
lotta.bi$lembigram <- lemmatize_strings(lotta.bi$bigram)
lotta.bi %>%
  count(lembigram, sort = TRUE)

lotta.bi <- lotta.bi %>%
  filter (lembigram != "NA")

lotta.bi %>%
  separate (lembigram, c("word1", "word2"), sep = " ") %>%
  filter (!word1 %in% stop_words$word,
          !word2 %in% stop_words$word) %>%
  count(word1, word2, sort = TRUE)


bigramfreq <- lotta.bi %>%
  separate (lembigram, c("word1", "word2"), sep = " ") %>%
  filter (!word1 %in% stop_words$word,
          !word2 %in% stop_words$word) %>%
  count(word1, word2, sort = TRUE) %>%
  unite("lembigram", c(word1,word2), sep = " ") %>%
  top_n (10)
bigramfreq

ggplot(data = bigramfreq, aes(x = (reorder(lembigram, n)), y = n)) +
  geom_bar (stat = "identity") +
  labs (x = "Word", y = "Frequency in example data") +
  coord_flip() +
  theme_classic()
