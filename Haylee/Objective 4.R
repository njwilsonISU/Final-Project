install.packages("fivethirtyeight")

library(fivethirtyeight)

data("classic_rock_raw_data")
data("classic_rock_song_list")

library(dplyr)
library(tidyverse)
library(stringr)

str(classic_rock_song_list)

title <- classic_rock_song_list %>%
  group_by(song) %>%
  filter(n() > 1) %>%
  count(song) %>%
  arrange(desc(n)) %>%
  mutate(counts = n) 
title
## There are 63 different song titles that each have more than 1 artist that have made songs with that title,
## because 63 titles is a lot to analyze, we decided to focus on songs titles that have more than 2 artists.

same_title <- classic_rock_song_list %>%
  group_by(song) %>%
  filter(n() > 2)
same_title
## There are 8 different song titles that each have 3 artists that have made songs with that title

same_title1 <- same_title %>%
  select(song, artist, release_year, combined, has_year, playcount, playcount_has_year) 

library(RColorBrewer)

same_title1 %>%
  filter(song == "Changes") %>%
  ggplot(aes(x=combined, y=playcount, fill = factor(combined))) +
  geom_bar(stat = "identity", position = position_dodge(), color = 'black') +
  theme_minimal() + scale_fill_brewer(palette = "YlOrRd") +
  theme(axis.text.x = element_text(angle = 25)) 
  

same_title1 %>%
  filter(song == "Loser") %>%
  ggplot(aes(x=combined, y=playcount, fill = factor(combined))) +
  geom_bar(stat = "identity", position = position_dodge(), color = 'black') +
  theme_minimal() + scale_fill_brewer(palette = "YlOrRd") +
  theme(axis.text.x = element_text(angle = 25))

same_title1 %>%
  filter(song == "Knockin' On Heaven's Door") %>%
  ggplot(aes(x=combined, y=playcount, fill = factor(combined))) +
  geom_bar(stat = "identity", position = position_dodge(), color = 'black') +
  theme_minimal() + scale_fill_brewer(palette = "YlOrRd") +
  theme(axis.text.x = element_text(angle = 25)) 

same_title1 %>%
  filter(song == "Photograph") %>%
  ggplot(aes(x=combined, y=playcount, fill = factor(combined))) +
  geom_bar(stat = "identity", position = position_dodge(), color = 'black') +
  theme_minimal() + scale_fill_brewer(palette = "YlOrRd") +
  theme(axis.text.x = element_text(angle = 25)) 

same_title1 %>%
  filter(song == "Don't Bring Me Down") %>%
  ggplot(aes(x=combined, y=playcount, fill = factor(combined))) +
  geom_bar(stat = "identity", position = position_dodge(), color = 'black') +
  theme_minimal() + scale_fill_brewer(palette = "YlOrRd") +
  theme(axis.text.x = element_text(angle = 25)) 

same_title1 %>%
  filter(song == "Funk #49") %>%
  ggplot(aes(x=combined, y=playcount, fill = factor(combined))) +
  geom_bar(stat = "identity", position = position_dodge(), color = 'black') +
  theme_minimal() + scale_fill_brewer(palette = "YlOrRd") +
  theme(axis.text.x = element_text(angle = 25))

same_title1 %>%
  filter(song == "Stop Draggin' My Heart Around") %>%
  ggplot(aes(x=combined, y=playcoun, fill = factor(combined))) +
  geom_bar(stat = "identity", position = position_dodge(), color = 'black') +
  theme_minimal() + scale_fill_brewer(palette = "YlOrRd") +
  theme(axis.text.x = element_text(angle= 25))

same_title1 %>%
  filter(song == "Summertime Blues") %>%
  ggplot(aes(x=combined, y=playcount, fill = factor(combined))) +
  geom_bar(stat = "identity", position = position_dodge(), color = 'black') +
  theme_minimal() + scale_fill_brewer(palette = "YlOrRd") +
  theme(axis.text.x = element_text(angle = 25)) 
## After running all of these ggplots to look at the playcounts for each song title, comparing the different artists,
## we decided to create a plot of all song titles in this dataset that have a playcount over 20.

same_title1 %>%
  filter(playcount > 15) %>%
  ggplot(aes(x=combined, y=playcount, fill = factor(combined))) +
  geom_bar(stat = "identity", position = position_dodge(), color = 'black') +
  theme_minimal() + scale_fill_brewer(palette = "YlOrRd") +
  theme(axis.text.x = element_blank())  +
  labs(
    title = "Most Common Songs Out of the Duplicate Titles",
    x = "",
    y = "Playcounts"
  )

common_name <- classic_rock_song_list %>% 
  select(song)

words <- strsplit(common_name$song, " ") 
words1 <- unlist(words)
freq_words <- table(words1) 
freqtable <- cbind.data.frame(word=names(freq_words),freq=as.integer(freq_words)) 

freqtable

freqtable %>%
  arrange(desc(freq)) %>%
  filter(str_length(word) > 3 & freq > 20) %>%
  ggplot(aes(x = word, y = freq, fill = factor(freq))) +
  geom_bar(stat = "identity", position = position_dodge(), color = 'black') +
  theme_minimal() +
  scale_fill_brewer(palette = "YlOrRd") + 
  theme(axis.text.x = element_text(angle = 45)) +
  labs(
    title = "Most Common Words in Rock Songs",
    x = "Words",
    y = "Number of Times the Word Occurs"
  )
