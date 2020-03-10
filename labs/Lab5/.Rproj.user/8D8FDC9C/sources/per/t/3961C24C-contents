library(lubridate)
library(dplyr)
edited_spotify_clean_dates$AlbumReleaseDate = parse_date_time(edited_spotify_clean_dates$AlbumReleaseDate,orders=c("y","ym","ymd"))
data1 <- edited_spotify %>% dplyr::mutate(Release_year = lubridate::year(edited_spotify_clean_dates$AlbumReleaseDate), 
                Release_month = lubridate::month(edited_spotify_clean_dates$AlbumReleaseDate), 
                Release_day = lubridate::day(edited_spotify_clean_dates$AlbumReleaseDate))

data2 <- select (data1,-c(AlbumReleaseDate, Release_day))

Season <- data2$Release_month

Season[Season %in% 12] <- "winter"
Season[Season %in% 1] <- "winter"
Season[Season %in% 2] <- "winter"
Season[Season %in% 3] <- "spring"
Season[Season %in% 4] <- "spring"
Season[Season %in% 5] <- "spring"
Season[Season %in% 6] <- "summer"
Season[Season %in% 7] <- "summer"
Season[Season %in% 8] <- "summer"
Season[Season %in% 9] <- "autumn"
Season[Season %in% 10] <- "autumn"
Season[Season %in% 11] <- "autumn"

data2 <- cbind(data2, Season)

data2$Release_month[data2$Release_month %in% 1] <- "January"
data2$Release_month[data2$Release_month %in% 2] <- "February"
data2$Release_month[data2$Release_month %in% 3] <- "March"
data2$Release_month[data2$Release_month %in% 4] <- "April"
data2$Release_month[data2$Release_month %in% 5] <- "May"
data2$Release_month[data2$Release_month %in% 6] <- "June"
data2$Release_month[data2$Release_month %in% 7] <- "July"
data2$Release_month[data2$Release_month %in% 8] <- "August"
data2$Release_month[data2$Release_month %in% 9] <- "September"
data2$Release_month[data2$Release_month %in% 10] <- "October"
data2$Release_month[data2$Release_month %in% 11] <- "November"
data2$Release_month[data2$Release_month %in% 12] <- "December"

library(xlsx)
write.csv(data2, file = "edited_spotify_04112019.csv")


spring: 3,4,5
summer: 6,7,8
autumn: 9,10,11
winter: 12,1,2