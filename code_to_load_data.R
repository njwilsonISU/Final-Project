# Data load code

install.packages("fivethirtyeight")

library(fivethirtyeight)

data("classic_rock_raw_data")
data("classic_rock_song_list")

str(classic_rock_raw_data)
# Classes 'tbl_df', 'tbl' and 'data.frame':	37673 obs. of  7 variables:
# $ song     : chr  "Caught Up in You" "Caught Up in You" "Caught Up in You" "Caught Up in You" ...
# $ artist   : chr  ".38 Special" ".38 Special" ".38 Special" ".38 Special" ...
# $ callsign : chr  "KGLK" "KGB" "KGB" "KGLK" ...
# $ time     : int  1402943314 1403398735 1403243924 1403470732 1403380737 1403105300 1402970932 1403456303 1403056697 1403179167 ...
# $ date_time: POSIXct, format: "2014-06-16 13:28:34" "2014-06-21 19:58:55" "2014-06-20 00:58:44" "2014-06-22 15:58:52" ...
# $ unique_id: chr  "KGLK1536" "KGB0260" "KGB0703" "KGLK0036" ...
# $ combined : chr  "Caught Up in You by .38 Special" "Caught Up in You by .38 Special" "Caught Up in You by .38 Special" "Caught Up in You by .38 Special" ...

str(classic_rock_song_list)
# Classes 'tbl_df', 'tbl' and 'data.frame':	2229 obs. of  7 variables:
# $ song              : chr  "Caught Up in You" "Fantasy Girl" "Hold On Loosely" "Rockin' Into the Night" ...
# $ artist            : chr  ".38 Special" ".38 Special" ".38 Special" ".38 Special" ...
# $ release_year      : int  1982 NA 1981 1980 1975 2000 2000 2002 1992 1985 ...
# $ combined          : chr  "Caught Up in You by .38 Special" "Fantasy Girl by .38 Special" "Hold On Loosely by .38 Special" "Rockin' Into the Night by .38 Special" ...
# $ has_year          : logi  TRUE FALSE TRUE TRUE TRUE TRUE ...
# $ playcount         : int  82 3 85 18 1 13 1 6 3 1 ...
# $ playcount_has_year: int  82 0 85 18 1 13 1 6 3 1 ...

