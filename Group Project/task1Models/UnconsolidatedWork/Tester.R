library(rio)
library(data.table)
library(dplyr)


myData <- import("cleanedData.csv", setclass= "tibble")

TestAlbums <- c("Shirley Bassey", "In Dreams","Meddle","London Calling",
                "It Takes A Nation Of Millions To Hold Us Back",
                "Absolutely", "Blue Lines","Dig Your Own Hole", "Yoshimi Battles The Pink Robots", 
                "Original Pirate Material",
                "Disc-Overy", "What Went Down")
#TestAlbums <- import("test_albums_6.txt")[2]
#TestAlbums <- apply(TestAlbums, 1, function(x) gsub(".*: ","",x))
myData <- {myData %>% 
    group_by(AlbumName, ArtistPopularity, ArtistNumFollowers, 
             AlbumBestChartPosition, AlbumWeeksOnChart, AlbumWeeksNumberOne,
             AlbumPopularity, Release_year, Release_month, Season, isPop, isFolk, 
             isRock, isIndie, isTechno, isRap, isJazz, isMetal, isBlues, isPunk) %>% 
    summarize(AlbumLoudness = mean(TrackLoudness),		
              AlbumTempo = mean(TrackTempo),
              AlbumAvgDuration = mean(TrackDuration),		
              AlbumDanceability = mean(TrackDanceability),
              AlbumEnergy = mean(TrackEnergy),
              AlbumKey = mean(TrackKey),		
              AlbumSpeechiness = mean(TrackSpeechiness),
              AlbumAcousticness = mean(TrackAcousticness),
              AlbumInstrumentalness = mean(TrackInstrumentalness),		
              AlbumLiveness = mean(TrackLiveness),
              AlbumValence = mean(TrackValence),
              AlbumTimeSignature = mean(TrackTimeSignature),
              
              AlbumTempo = mean(TrackTempo)
    )}
myData <- ungroup(myData)
#names <- myData$AlbumName
#myData <- standardize(select(myData, -AlbumName, ))
#myData$AlbumName <- names
TestData <- filter(myData, AlbumName %in% TestAlbums)
TestData <- select(TestData, -AlbumName)
TrainData <- filter(myData, !AlbumName %in% TestAlbums)
TrainData <- select(TrainData, -AlbumName)

train.X <- as.matrix(select(TrainData, -AlbumPopularity))
train.Y <- as.matrix(select(TrainData, AlbumPopularity))
test.X <- as.matrix(select(TestData, -AlbumPopularity))
test.Y <- select(TestData, AlbumPopularity)
popVect <- test.Y
#train.X <- standardize(train.X)
train.Y <- (train.Y)
#train.Y <- standardize(train.Y)
#test.X <- standardize(test.X)
test.Y <- (test.Y)

GoodModel  <-  lm((AlbumPopularity) ~ (AlbumAcousticness +AlbumValence + AlbumDanceability + isPunk +isBlues + isJazz+ isRock + isPop),data = TrainData)
print(sum(abs(predict(GoodModel, TestData) - popVect)))
data.frame(popVect, predict(GoodModel, TestData), (predict(GoodModel, TestData) - popVect))


GoodModel2  <-  lm((AlbumPopularity) ~ ArtistPopularity+AlbumAcousticness +AlbumValence + AlbumDanceability + isPunk +isBlues + isJazz+ isRock + isPop + isFolk +isRap,data = TrainData)
print(sum((predict(GoodModel2, TestData) - popVect)^2))
data.frame(popVect, predict(GoodModel2, TestData), (predict(GoodModel2, TestData) - popVect))

GoodModel2  <-  lm((AlbumPopularity) ~ ArtistPopularity+ArtistNumFollowers,data = TrainData)
print(sum((predict(GoodModel2, TestData) - popVect)^2))
data.frame(popVect, predict(GoodModel2, TestData), (predict(GoodModel2, TestData) - popVect))




GoodModel3  <-  lm((AlbumPopularity) ~ AlbumLoudness+		
                   AlbumTempo +
                   AlbumAvgDuration +		
                   AlbumDanceability+
                   AlbumEnergy +
                   #AlbumKey+
                   AlbumSpeechiness +
                   AlbumAcousticness +
                   AlbumInstrumentalness+	
                   AlbumLiveness+
                   AlbumValence +
                   #AlbumTimeSignature,
                   
                   AlbumTempo, data = TrainData)
print(sum((predict(GoodModel3, TestData) - popVect)^2))
data.frame(popVect, predict(GoodModel3, TestData), (predict(GoodModel3, TestData) - popVect))


# GoodModel3  <-  lm((AlbumPopularity) ~ AlbumAcousticness +AlbumValence + AlbumDanceability + Release_year+
#                      AlbumTempo+AlbumLoudness+AlbumAvgDuration+AlbumLiveness+AlbumTimeSignature+
#                      AlbumKey, data = TrainData)
# print(sum((predict(GoodModel3, TestData) - popVect)^2))
# data.frame(popVect, predict(GoodModel3, TestData), (predict(GoodModel3, TestData) - popVect))


# library(betareg)
# 
# GoodModel3  <-  betareg((AlbumPopularity) ~ AlbumAcousticness +AlbumValence + AlbumDanceability + Release_year+
#                           AlbumTempo+AlbumLoudness+AlbumLiveness
#                         , data = TrainData)
# print(sum((predict(GoodModel3, TestData) - popVect)^2))
# data.frame(popVect, predict(GoodModel3, TestData), (predict(GoodModel3, TestData) - popVect))
# 



AllModel <- lm(AlbumPopularity ~. ,data = TrainData)

print(sum((predict(AllModel, TestData) - popVect)^2))
data.frame(popVect, predict(AllModel, TestData), (predict(AllModel, TestData) - popVect))


