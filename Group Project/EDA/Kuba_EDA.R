library(tidyverse)
library(GGally)
library(pander)
library(gridExtra)

# Cleaning and grouping data

data <- read.csv("cleanedData.csv")
data <- select(data, -ArtistID, -ArtistGenres, -AlbumID, -TrackID, -TrackTimeSignature, 
               -Release_month, -TrackKey, -TrackMode, -TrackNumber)

AlbumData = {data %>% 
    group_by(Artist,AlbumName, isPop, isRock, isFolk, isTechno, isRap, isJazz, isIndie, isMetal, 
             isBlues, isPunk, Release_year, Season, AlbumWeeksNumberOne, AlbumWeeksOnChart,AlbumPopularity) %>% 
    summarize(AlbumValence = mean(TrackValence),AlbumSpeechiness = mean(TrackSpeechiness),
              AlbumLiveness = mean(TrackLiveness), AlbumEnergy = mean(TrackEnergy),
                                   AlbumDanceability = mean(TrackDanceability), AlbumLoudness = mean(TrackLoudness),
             AlbumAcousticness = mean(TrackAcousticness), AlbumInstrumentalness = mean(TrackInstrumentalness), 
             AlbumTempo = mean(TrackTempo)) }

#basic things
cor(AlbumData[,15:26])
summary(AlbumData)
str(AlbumData)


#Hit indicator
AlbumData$Hit <- 0
AlbumData$Hit[AlbumData$AlbumPopularity > 50] <- 1
AlbumData$Hit[AlbumData$AlbumPopularity <= 50] <- 0
AlbumData$Hit <- as.factor(AlbumData$Hit)

#basic things
cor(AlbumData[,15:26])
summary(AlbumData)
str(AlbumData)


#Basic correlation
ggplot(AlbumData, aes(x = AlbumPopularity, y = AlbumDanceability, colour = Season)) + 
  geom_jitter() + geom_smooth(method = loess, se=FALSE)

ggplot(AlbumData, aes(x = AlbumPopularity, y = AlbumAcousticness, colour = Season)) + 
  geom_jitter() + geom_smooth(method = loess, se=FALSE)

ggplot(AlbumData, aes(x = AlbumWeeksOnChart, y = AlbumDanceability, colour = Season)) + 
  geom_jitter() + geom_smooth(method = loess, se=FALSE) + xlim(0, 140)

ggplot(AlbumData, aes(x = AlbumWeeksOnChart, y = AlbumAcousticness, colour = Season)) + 
  geom_jitter() + geom_smooth(method = loess, se=FALSE) + xlim(0, 140)

#distribution of album popularity
ggplot(AlbumData, aes(x = AlbumPopularity)) + geom_histogram(color="black", fill="white", aes(y=..density..)) + 
  geom_vline(aes(xintercept=mean(AlbumPopularity)), color="blue", linetype="dashed", size=1) +
  geom_density(alpha = 0.2, fill = 'red') 


#template for hit/not hit graphs
ggplot(AlbumData, aes(x = AlbumDanceability, y = AlbumAcousticness, colour = Hit)) + 
  geom_jitter() + geom_smooth(method = loess, se=FALSE)

#somelm models
model1 <- lm(AlbumPopularity ~ isRap + isTechno + isRap + isFolk + isPop + isMetal + isPunk + isBlues + isJazz, data= AlbumData)
model2 <- lm(AlbumPopularity ~ AlbumValence, data = AlbumData)
model3 <- lm(AlbumPopularity ~ AlbumValence + AlbumDanceability, data = AlbumData)
model4 <- lm(AlbumPopularity ~ AlbumValence + AlbumDanceability + AlbumAcousticness, data = AlbumData)
model5 <- lm(AlbumPopularity ~ AlbumValence + AlbumDanceability + AlbumAcousticness + AlbumSpeechiness, data = AlbumData)
model6 <- lm(AlbumPopularity ~ AlbumValence + AlbumDanceability + AlbumAcousticness + AlbumLiveness+isRap+isMetal, data = AlbumData)

#Filtering data
AlbumDataPop <- filter(AlbumData,Hit == '1')
AlbumDataNot <- filter(AlbumData,Hit == '0')

#hit vs not-hit plots

#Acousticness
plot1 <- ggplot(AlbumDataPop, aes(x = AlbumAcousticness)) + geom_histogram() + 
  geom_vline(aes(xintercept=mean(AlbumAcousticness)), color="blue", linetype="dashed", size=1) +
  geom_density(alpha = 0.2, fill = 'red') + labs(title = "Popular") 
plot2 <- ggplot(AlbumDataNot, aes(x = AlbumAcousticness)) + geom_histogram() +  
  geom_vline(aes(xintercept=mean(AlbumAcousticness)), color="blue", linetype="dashed", size=1) +
  geom_density(alpha = 0.2, fill = 'red') + labs(title = 'Unpopular')
grid.arrange(plot1, plot2, ncol =2)

#Tempo
plot3 <- ggplot(AlbumDataPop, aes(x = AlbumTempo)) + 
  geom_vline(aes(xintercept=mean(AlbumTempo)), color="blue", linetype="dashed", size=1) +
  geom_density(alpha = 0.2, fill = 'red') + labs(title = "Popular") 
plot4 <- ggplot(AlbumDataNot, aes(x = AlbumTempo)) +  
  geom_vline(aes(xintercept=mean(AlbumTempo)), color="blue", linetype="dashed", size=1) +
  geom_density(alpha = 0.2, fill = 'red') + labs(title = 'Unpopular')
grid.arrange(plot3, plot4, ncol =2)

#Danceability
plot5 <- ggplot(AlbumDataPop, aes(x = AlbumDanceability)) + geom_histogram() + 
  geom_vline(aes(xintercept=mean(AlbumDanceability)), color="blue", linetype="dashed", size=1) +
  geom_density(alpha = 0.2, fill = 'red') + labs(title = "Popular") 
plot6 <- ggplot(AlbumDataNot, aes(x = AlbumDanceability)) + geom_histogram() +  
  geom_vline(aes(xintercept=mean(AlbumDanceability)), color="blue", linetype="dashed", size=1) +
  geom_density(alpha = 0.2, fill = 'red') + labs(title = 'Unpopular')
grid.arrange(plot5, plot6, ncol =2)

#Loudness
plot7 <- ggplot(AlbumDataPop, aes(x = AlbumLoudness)) + 
  geom_vline(aes(xintercept=mean(AlbumLoudness)), color="blue", linetype="dashed", size=1) +
  geom_density(alpha = 0.2, fill = 'red') + labs(title = "Popular") 
plot8 <- ggplot(AlbumDataNot, aes(x = AlbumLoudness)) +  
  geom_vline(aes(xintercept=mean(AlbumLoudness)), color="blue", linetype="dashed", size=1) +
  geom_density(alpha = 0.2, fill = 'red') + labs(title = 'Unpopular')
grid.arrange(plot7, plot8, ncol =2)

#Significance of danceability over time
<<<<<<< HEAD
plot9 <- ggplot(AlbumDataPop, aes(x = Release_year, y = AlbumValence)) + geom_jitter() +
  labs(title = "Album Popularity over 50") + geom_smooth() + xlim(1960, 2020) + ylim(0.2, 0.9)
plot10 <- ggplot(AlbumDataNot, aes(x = Release_year, y = AlbumValence)) + geom_jitter() +
  labs(title = 'Album Popularity less than 50')+geom_smooth() + xlim(1960, 2020) + ylim(0.2, 0.9)
=======
plot9 <- ggplot(AlbumDataPop, aes(x = Release_year, y = AlbumDanceability)) + geom_jitter() +
  labs(title = "Popular") + geom_smooth() + xlim(1960, 2020)
plot10 <- ggplot(AlbumDataNot, aes(x = Release_year, y = AlbumDanceability)) + geom_jitter() +
  labs(title = 'Unpopular')+geom_smooth() + xlim(1960, 2020)
>>>>>>> f650051d6629eea6731e6feaea57a7a278e8fda2
grid.arrange(plot9, plot10, ncol =2)

#significance ofspeechiness over time
plot9 <- ggplot(AlbumDataPop, aes(x = Release_year, y = AlbumSpeechiness)) + geom_jitter() +
  labs(title = "Popular") + geom_smooth() + xlim(1960, 2020)
plot10 <- ggplot(AlbumDataNot, aes(x = Release_year, y = AlbumSpeechiness)) + geom_jitter() +
  labs(title = 'Unpopular')+geom_smooth() + xlim(1960, 2020)

grid.arrange(plot9, plot10, ncol =2)

#significance of acousticness over time
plot11 <- ggplot(AlbumDataPop, aes(x = Release_year, y = AlbumAcousticness)) + geom_jitter() +
  labs(title = "Popular") + geom_smooth() + xlim(1960, 2020)
plot12 <- ggplot(AlbumDataNot, aes(x = Release_year, y = AlbumAcousticness)) + geom_jitter() +
  labs(title = 'Unpopular')+geom_smooth() + xlim(1960, 2020)

grid.arrange(plot11, plot12, ncol =2)


grid.arrange(plot9, plot10, ncol =2)

#significance of valence over time
plot13 <- ggplot(AlbumDataPop, aes(x = Release_year, y = AlbumValence)) + geom_jitter() +
  labs(title = "Popular") + geom_smooth() + xlim(1960, 2020)
<<<<<<< HEAD
plot14 <- ggplot(AlbumDataNot, aes(x = Release_year, y = AlbumLoudness)) + geom_jitter() +
=======
plot14 <- ggplot(AlbumDataNot, aes(x = Release_year, y = AlbumValance)) + geom_jitter() +
>>>>>>> f650051d6629eea6731e6feaea57a7a278e8fda2
  labs(title = 'Unpopular')+geom_smooth() + xlim(1960, 2020)

grid.arrange(plot13, plot14, ncol =2)


plot15 <- ggplot()
