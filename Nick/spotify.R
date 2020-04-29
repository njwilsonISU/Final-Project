# get spotify info
# https://cran.r-project.org/web/packages/spotifyr/spotifyr.pdf
# https://github.com/charlie86/spotifyr

# install.packages("spotifyr")

Sys.setenv(SPOTIFY_CLIENT_ID = '71a4b0a0f0424a6d835776e2b23c0e16')
Sys.setenv(SPOTIFY_CLIENT_SECRET = '258557b7056c4dc393788d7728f390ef')

library(spotifyr)

access_token <- get_spotify_access_token()

rolling_stones <- get_artist_audio_features('The Rolling Stones')
# song <- get_tracks(ids, market = NULL, authorization = get_spotify_access_token(), include_meta_info = FALSE)

rolling_stones

#---------------------------------------------------------------------------------------------------------------

Sys.setenv(SPOTIFY_CLIENT_ID = '71a4b0a0f0424a6d835776e2b23c0e16')
Sys.setenv(SPOTIFY_CLIENT_SECRET = '258557b7056c4dc393788d7728f390ef')

library(spotifyr)

access_token <- get_spotify_access_token()

morning_song <- top_songs_per_time_of_day %>% filter(time_of_day == "6 AM - 9 AM")

morning_song_complete <- classic_rock_song_list %>% filter(song == morning_song$song)

artist <- morning_song_complete$artist
artist_spotifyID <- get_artist_audio_features(artist)$artist_id[1]

#Getting artist albums data available on spotify and saving it on
#the albums variable
albums <- get_artist_albums(artist_spotifyID, include_groups = c("album"), 
                            limit=50, market = "US", offset = 0,
                            authorization = access_token,
                            include_meta_info = FALSE) %>%
  #line below adds an id column called "num" to identify each row of data
  mutate(num = row_number()) %>%
  #line below is used to select the albums you are interested, use the num
  #column to filter, if you want all albums just delete or comment the line 
  #filter(num %in% c(22,20,18,15,14,12,11,8,6,3)) %>%
  #line below selects some columns from all the data that the API returns,
  #if you want different columns just add their name
  select(id, name, release_date, total_tracks)

#Getting tracks from each album and saving it on the tracks variable
tracks <- albums$id %>%
  #line below applies a function to get all tracks from each album
  #saved on the albums variable
  map_dfr(~ get_album_tracks(.x, limit = 30, offset = 0, market = NULL,
                             authorization = access_token,
                             include_meta_info = FALSE)) %>%
  #line below selects some columns from all the tracks in album data that
  #the API returns, if you want different columns just add their name
  select(id, track_number, name, duration_ms)

#Getting track info for each track collected by the previous code
#section and saving it in the tracks_info variable
tracks_info <- tracks$id %>%
  #line below applies a function to get the info from each track
  #saved on the tracks variable
  map_dfr(~ get_tracks(.x, market = NULL, authorization = access_token)) %>%
  #line below selects some columns from all the columns that
  #the API returns, if you want different columns just add their name
  select(id, name, popularity, album.id, album.name, album.total_tracks)

#Getting track audio features for each track collected by the previous code
#section and saving it in the tracks_audio_features variable
track_audio_features <- tracks$id %>%
  map_dfr(~ get_track_audio_features(.x, authorization = access_token)) %>%
  select(-c(type,uri,track_href,analysis_url))


morning_song_spotify_info <- tracks_info %>% filter(name == morning_song$song)

morning_song_spotify_info





