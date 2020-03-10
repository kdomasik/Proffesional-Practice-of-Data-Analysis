library(rio)
library(data.table)
library(dplyr)


myData <- import("cleanedData.csv", setclass= "tibble")

TestAlbums <- c("Shirley Bassey", "In Dreams","Meddle","London Calling",
                "It Takes A Nation Of Millions To Hold Us Back",
                "Absolutely", "Blue Lines","Dig Your Own Hole", "Yoshimi Battles The Pink Robots", 
                "Original Pirate Material",
                "Disc-Overy", "What Went Down")

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
TestData <- filter(myData, AlbumName %in% TestAlbums)
TestData <- ungroup(TestData)
popVect <- select(TestData, AlbumPopularity)
TestData <- select(TestData, -AlbumPopularity, -AlbumName)
TrainData <- filter(myData, !AlbumName %in% TestAlbums)
TrainData <- select(ungroup(TrainData), -AlbumName)

GoodModel  <-  lm((AlbumPopularity) ~ (Release_month == "January") + AlbumAcousticness +AlbumValence + AlbumDanceability + isPunk +isBlues + isJazz+ isRock + isPop,data = TrainData)
print(sum((predict(GoodModel, TestData) - popVect)^2))


AllModel <- lm(AlbumPopularity ~. ,data = TrainData)
print(sum((predict(AllModel, TestData) - popVect)^2))


#KNN
distances.l2 <- function(x,y)
{
  apply(y,1,function(p) apply(x,1,function(q) sqrt(sum((p-q)^2))))
}

knn.regression.test <- function(k,train.X,train.Y,test.X,test.Y,distances)
{
  estimates <- vector()
  dist <-  distances(train.X, test.X)
  #Go through each data point to be tested 
  #and create a vector holding the responses of the k nearest neighbours
  for(each in 1:dim(test.X)[1])
  {
    adjacency <- data.frame(train.Y, dist[,each])
    colnames(adjacency) <- c("Train", "Distances")
    adjacency <- adjacency[order(adjacency$Distances),]
    
    #Calculate distance weighted mean
    
    dis <- 0
    weightDis <- 0
    for(i in 1:k)
    {
      dis <- dis + 1/adjacency$Distances[i]
      weightDis <- weightDis + 1/(adjacency$Distances[i]) * adjacency$Train[i] 
    }
    
    estimates[each] = weightDis/dis
  }
  
  print(sum((test.Y-estimates)^2))
  return(estimates)
}

myData <- import("cleanedData.csv", setclass= "tibble")

TestAlbums <- c("Shirley Bassey", "In Dreams","Meddle","London Calling",
                "It Takes A Nation Of Millions To Hold Us Back",
                "Absolutely", "Blue Lines","Dig Your Own Hole", "Yoshimi Battles The Pink Robots", 
                "Original Pirate Material",
                "Disc-Overy", "What Went Down")

myData <- {myData %>% 
    group_by(AlbumName, 
             AlbumPopularity, Release_year,) %>% 
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


TestData <- filter(myData, AlbumName %in% TestAlbums)
TestData <- ungroup(TestData)
TestData <- select(TestData, -AlbumName)
TrainData <- filter(myData, !AlbumName %in% TestAlbums)
TrainData <- select(ungroup(TrainData), -AlbumName)
train.X <- as.matrix(select(TrainData, -AlbumPopularity))
train.Y <- as.matrix(select(TrainData, AlbumPopularity))
test.X <- as.matrix(select(TestData, -AlbumPopularity))
test.Y <- select(TestData, AlbumPopularity)

train.X <- standardize(train.X)
#train.Y <- standardize(train.Y)
test.X <- standardize(test.X)
#test.Y <- standardize(test.Y)
est <- knn.regression.test(k,train.X,train.Y,test.X,test.Y, distances.l2)
goodness <- vector()
for(k in 1:100)
{
  goodness[k] <- knn.regression.test(k,train.X,train.Y,test.X,test.Y, distances.l2)
}
print(which.min(goodness))

myplot <- ggplot(myData, )) + 
  geom_point() +
  geom_smooth(method = 'glm', formula = y ~ x, se = FALSE,
              method.args = list(
                family = quasi(link = "log") ))
