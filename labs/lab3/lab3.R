library(dplyr)
library(lubridate)
library(ggplot2)
require(dplyr)
require(ggplot2)
library(readxl)
library(tidyverse)
library(rio)
Spotify <- import("edited_spotify.xlsx")
MyData = {Spotify %>% 
    group_by(Artist,AlbumName,AlbumReleaseDate) %>% 
    summarize(AlbumValence = mean(TrackValence),
              AlbumSpeechiness = mean(TrackSpeechiness),
              AlbumLiveness = mean(TrackLiveness) )}
MyData$AlbumReleaseDate = parse_date_time(MyData$AlbumReleaseDate,orders=c("y","ym","ymd"))
MyData$Genre <- "Other"
MyData$Genre[MyData$AlbumSpeechiness>0.2] <- "Rap"





MyData$Genre <- "Other"
MyData$Genre[MyData$AlbumSpeechiness>0.2] <- "Rap"
myplot1 <- { MyData %>% ggplot(aes(x = AlbumReleaseDate,  y = AlbumValence, fill = Genre, color = Genre))} + geom_point(aes(size=AlbumSpeechiness)) +
  geom_smooth(method = loess, se=FALSE) + xlab( "Album Release Year") + ylab ( "Average Valence of  the Album") + theme(legend.position= "top", legend.background = element_rect(fill="white", size=0.5, linetype = "solid", colour="black"))  +
  labs(title = "Average valence of the non-rap album is decreasing over the years", subtitle = "Rap Albums tend to have higher valence than non-rap ones.")
myplot1