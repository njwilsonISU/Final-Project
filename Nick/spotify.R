# get spotify info
# https://cran.r-project.org/web/packages/spotifyr/spotifyr.pdf
# https://github.com/charlie86/spotifyr

install.packages("spotifyr")

Sys.setenv(SPOTIFY_CLIENT_ID = '71a4b0a0f0424a6d835776e2b23c0e16')
Sys.setenv(SPOTIFY_CLIENT_SECRET = '258557b7056c4dc393788d7728f390ef')

library(spotifyr)

access_token <- get_spotify_access_token()

# rolling_stones <- get_artist_audio_features('The Rolling Stones')
# song <- get_tracks(ids, market = NULL, authorization = get_spotify_access_token(), include_meta_info = FALSE)



