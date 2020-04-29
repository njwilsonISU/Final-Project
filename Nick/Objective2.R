# --------------------------------
library(fivethirtyeight)
library(tidyverse)
library(lubridate)
library(RColorBrewer)

data("classic_rock_raw_data")

# head(classic_rock_raw_data)
# --------------------------------

# --------------------------------
# separate the date and times
crrd2 <- classic_rock_raw_data %>%
  mutate(
    weekday = wday(date_time, label = TRUE),
    hour = hour(date_time),
    minute = minute(date_time),
    second = second(date_time)
  )

# head(crrd2)
# --------------------------------


# --------------------------------

# Define the breaks

# # by hour
# breaks <- c(seq(0,24,1))
# labels <- c("12 AM", "1 AM", "2 AM", "3 AM", "4 AM", "5 AM", "6 AM", "7 AM", "8 AM", "9 AM", "10 AM", "11 AM", 
#             "12 PM", "1 PM", "2 PM", "3 PM", "4 PM", "5 PM", "6 PM", "7 PM", "8 PM", "9 PM", "10 PM", "11 PM" )

# by 3 hour period
breaks <- c(seq(0,24,3))
labels <- c("12 AM - 3 AM", "3 AM - 6 AM", "6 AM - 9 AM", "10 AM - 12 PM", "12 PM - 3 PM", "3 PM - 6 PM", "6 PM - 9 PM", "9 PM - 12 PM" )

# # by 6 hour period
# breaks <- c(seq(0,24,6))
# labels <- c("12 AM - 6 AM", "6 AM - 12 PM", "12 PM - 6 PM", "6 PM - 12 PM" )

# --------------------------------


# --------------------------------

# apply the values to each play
crrd3 <- crrd2 %>%
  mutate(
    time_of_day = cut(hour
                      ,breaks = breaks
                      ,include.lowest = TRUE
                      ,right = FALSE
                      ,labels = labels
    )
  )

# head(crrd3)

# --------------------------------


# --------------------------------

# Number of total plays for each time of day

crrd3 %>% 
  group_by(time_of_day) %>%
  summarise( n_plays = n() ) %>%
  arrange(desc(time_of_day)) %>%
  ggplot(aes(x = time_of_day, y = n_plays, fill = time_of_day)) +
  geom_bar(stat = "identity", position = position_dodge(), color = 'black') + 
  theme_minimal() + 
  scale_fill_brewer(palette = "YlOrRd")


# --------------------------------


# --------------------------------

song_plays_per_time_of_day <- crrd3 %>% 
  group_by(time_of_day, song) %>%
  summarise( n_plays = n() )

song_plays_per_time_of_day <- song_plays_per_time_of_day %>%
  mutate( percent = 100*n_plays/sum(n_plays) )

# Get top values from each 
top_songs_per_time_of_day <- song_plays_per_time_of_day %>% 
  top_n(1, n_plays)

top_songs_per_time_of_day

# # most popular songs for each time of day by # of plays
# top_songs_per_time_of_day %>%
#   ggplot(aes(x = time_of_day, y = n_plays, fill = factor(song))) +
#   geom_bar(stat = "identity", position = position_dodge(), color = 'black') + 
#   theme_minimal() + 
#   scale_fill_brewer(palette = "YlOrRd")
# 
# # most popular songs for each time of day by % of total plays
# top_songs_per_time_of_day %>%
#   ggplot(aes(x = time_of_day, y = percent, fill = factor(song), order = time_of_day)) +
#   geom_bar(stat = "identity", position = position_dodge(), color = 'black') + 
#   theme_minimal() + 
#   scale_fill_brewer(palette = "YlOrRd")

# --------------------------------


# --------------------------------

# when are the most popular songs featured most?

# get the most popular songs
top_songs_overall <- crrd3 %>% 
  group_by(song) %>% 
  summarise( n_plays = n() ) %>% 
  top_n(3, n_plays) %>%
  arrange(desc(n_plays))


top_songs_overall_over_time <- song_plays_per_time_of_day %>%
  filter(song %in% top_songs_overall$song)

top_songs_overall_over_time %>%
  ggplot(aes(x = time_of_day, y = percent, group = factor(song), color = factor(song))) + 
  geom_line() + 
  theme_minimal() +
  scale_color_manual(values=c('#C8102E','#F1BE48', '#9B945F'))

# --------------------------------


### Artists

# ---------------------------------

artist_plays_per_time_of_day <- crrd3 %>% 
  group_by(time_of_day, artist) %>%
  summarise( n_plays = n() )

artist_plays_per_time_of_day <- artist_plays_per_time_of_day %>%
  mutate( percent = 100*n_plays/sum(n_plays) )

# Get top values from each 
top_artists <- artist_plays_per_time_of_day %>% top_n(3, n_plays)

# most popular artists for each time of day by # of plays
top_artists %>%
  ggplot(aes(x = time_of_day, y = n_plays, fill = factor(artist))) +
  geom_bar(stat = "identity", position = position_dodge(), color = 'black') + 
  theme_minimal() + 
  scale_fill_brewer(palette = "YlOrRd")

# most popular artists for each time of day by % of total plays
top_artists %>%
  ggplot(aes(x = time_of_day, y = percent, fill = factor(artist))) +
  geom_bar(stat = "identity", position = position_dodge(), color = 'black') + 
  theme_minimal() + 
  scale_fill_brewer(palette = "YlOrRd")

# --------------------------------


# ---------------------------------

# when are the most popular songs featured most?

top_artists_per_time_of_day <- artist_plays_per_time_of_day %>%
  filter(artist %in% top_artists$artist)

top_artists_per_time_of_day %>%
  ggplot(aes(x = time_of_day, y = percent, group = factor(artist), color = factor(artist))) + 
  geom_line() + 
  theme_minimal() +
  scale_color_manual(values=c('#C8102E','#F1BE48', '#9B945F', '#BE531C'))


