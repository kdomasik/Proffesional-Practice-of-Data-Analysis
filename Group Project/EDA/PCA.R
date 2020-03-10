library(timevis)
library(rio)
library(data.table)
library(ggplot2)
library(dplyr)
library(lubridate)
library(xlsx)
library(ggbiplot)
library(robustHD)
# 
# myData <- import("cleanedData.csv", setclass = "tibble")
# reducedData <- select(myData, -ArtistPopularity, -ArtistNumFollowers, 
#                       -AlbumWeeksOnChart, -AlbumWeeksNumberOne, -Artist, -ArtistID, 
#                       -ArtistGenres, -AlbumName, -AlbumID, -TrackInstrumentalness,-TrackAcousticness, 
#                       -TrackID, -TrackName, -AlbumBestChartPosition, -TrackKey)
# 
# reducedData$Season = as.numeric(as.factor(reducedData$Season))
# reducedData$Release_month = as.numeric(as.factor(reducedData$Release_month))
# reducedData <- select(reducedData, -isPop, - isRock, -isFolk, -isTechno, -isRap, -isJazz, -isIndie, -isMetal, -isBlues, -isPunk)
# reducedData <- select(reducedData, -TrackNumber, -TrackMode, -TrackDuration)
# reducedData <- na.omit(reducedData)
#reducedData <- standardize(reducedData, centerFun = mean, scaleFun = sd)

myData <- import("cleanedData.csv", setclass= "tibble")
reducedData <- select(myData, TrackValence, TrackTempo, TrackAcousticness, TrackDanceability, TrackEnergy, 
                      TrackSpeechiness, TrackLiveness, TrackInstrumentalness)
reducedData <- standardize(reducedData, centerFun = mean, scaleFun = sd)
pca <- prcomp(reducedData)
ggbiplot(pca, alpha = 0.1)
cor(myData$TrackAcousticness, myData$TrackEnergy)
cor(myData)

