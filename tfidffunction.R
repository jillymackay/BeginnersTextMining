require(tidyverse)
require(tidytext)

# I want a function to do the steps of the term frequency - inverse document frequency
# I use this a lot. 


# Sample Data

starwars1 <- tibble (film = c("ANH", "ESB", "ROJ"),
                    text = c("It is a period of civil war. Rebel spaceships, striking from a hidden base, have won their first victory against the evil Galactic Empire. During the battle, Rebel spies managed to steal secret plans to the Empire's ultimate weapon, the DEATH STAR, an armored space station with enough power to destroy an entire planet. Pursued by the Empire's sinister agents, Princess Leia races home aboard her starship, custodian of the stolen plans that can save her people and restore freedom to the galaxy.....",
                             "It is a dark time for the Rebellion. Although the Death Star has been destroyed, Imperial troops have driven the Rebel forces from their hidden base and pursued them across the galaxy. Evading the dreaded Imperial Starfleet, a group of freedom fighters led by Luke Skywalker has established a new secret base on the remote ice world of Hoth. The evil lord Darth Vader, obsessed with finding young Skywalker, has dispatched thousands of remote probes into the far reaches of space....",
                             "Luke Skywalker has returned to his home planet of Tatooine in an attempt to rescue his friend Han Solo from the clutches of the vile gangster Jabba the Hutt. Little does Luke know that the GALACTIC EMPIRE has secretly begun construction on a new armored space station even more powerful than the first dreaded Death Star. When completed, this ultimate weapon will spell certain doom for the small band of rebels struggling to restore freedom to the galaxy...")) %>%
  unnest_tokens(word, text) %>%
  mutate(film = as.factor(film)) %>%
  count(film, word, sort = TRUE) %>%
  ungroup()



# Old workflow to replace:

total_wars <- starwars1 %>%
  group_by(film) %>%
  summarize(total = sum(n))

starwars <- left_join(starwars1, total_wars)

starwars <- starwars %>%
  bind_tf_idf(word, film, n)


# Test
starwars %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  group_by(film) %>%
  top_n(3) %>%
  ungroup %>%
  ggplot(aes(word, tf_idf, fill = film)) +
  geom_col(show.legend = FALSE) +
  labs (x = NULL, y = "tf-idf") +
  facet_wrap(~film, ncol = 2, scales = "free") +
  coord_flip()
# this is an ugly chart don't worry about it


# My function:

quick_tf_idf <- function (data, word, grouping_factor) {
  qgv <- enquo(grouping_factor)
  word <- enquo(word)
  step1 <- data %>%
    group_by(!!qgv) %>%
    summarise(total = sum(n))
  step1 <- left_join(data, step1)
  data <- data %>%
    bind_tf_idf (!!word, !!qgv, n)
}

mytest <- quick_tf_idf (starwars1, word, film)

# And my test:
mytest %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  group_by(film) %>%
  top_n(3) %>%
  ungroup %>%
  ggplot(aes(word, tf_idf, fill = film)) +
  geom_col(show.legend = FALSE) +
  labs (x = NULL, y = "tf-idf") +
  facet_wrap(~film, ncol = 2, scales = "free") +
  coord_flip()


# My next task is getting this one to work:
# i.e. have the function able to assume 'word' without being told
# But I'm 
starwars <- quick_tf_idf (starwars1, grouping_factor = film)

