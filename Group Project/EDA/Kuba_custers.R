library(tidyverse)
library(GGally)
library(pander)
library(gridExtra)
library(dplyr)
library(robustHD)
library(devtools)
library(ggfortify)
library(meanShiftR)
library("FactoMineR")
library("factoextra")
library(klaR)


spotify <- read.csv("cleanedData.csv")
spotify <- dplyr::select(spotify, -Artist, - AlbumName, -ArtistGenres, -AlbumBestChartPosition, -AlbumWeeksOnChart, -AlbumWeeksNumberOne, -TrackName, - TrackNumber)

FeatureData <- dplyr::select(spotify, TrackValence, TrackTempo, TrackAcousticness, TrackDanceability, TrackEnergy, 
                      TrackSpeechiness, TrackLiveness, TrackInstrumentalness, TrackLoudness, TrackMode)
GenreData <- dplyr::select(spotify, isRap, isRock, isFolk, isPop, isMetal, isTechno, isJazz, isIndie, isPunk, isBlues)

res.cfviz_screeplot(res.pca, addlabels = TRUE, ylim = c(0, 45))

fviz_pca(FeatureData)

plot(GenreData, col=clusters2$cluster)

cor(spotify[,8:18])

plot(x = FeatureData$TrackEnergy,y = FeatureData$TrackLoudness)
mca <- MCA(GenreData)

# Danceability + Valence = 0.5
# Energy + loudness = 0.77
# Energy + accousticness = -0.7
# Loudness + Accousticness = - 0.6


