library(rio)
library(data.table)
library(dplyr)
library(pracma)
library( splus2R)
#KNN


distances.l2 <- function(x,y)
{
  apply(y,1,function(p) apply(x,1,function(q) nthroot(sum((p-q)^2),2)))
}

distances.l1 <- function(x,y) {
  apply(y,1,function(p) apply(x,1,function(q) sum(abs(p-q))))
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

#TestAlbums <- import("test_albums_6.txt")[2]
#TestAlbums <- apply(TestAlbums, 1, function(x) gsub(".*: ","",x))
myData <- {myData %>% 
    group_by(AlbumName,AlbumPopularity ,
             Release_year, Release_month, Season, isPop, isFolk, 
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
#Best Model
#myData <- select(myData, -AlbumAvgDuration, -AlbumKey, -AlbumSpeechiness,-AlbumTimeSignature)
#test2 Model
#myData <- select(myData, -AlbumAvgDuration, -AlbumKey, -AlbumSpeechiness,-AlbumTimeSignature, -AlbumDanceability)
#test2 Model
myData <- select(myData, -AlbumAvgDuration, -AlbumKey,-AlbumTimeSignature, -isPop, -isFolk, 
                 -isRock, -isIndie, -isTechno, -isRap, -isJazz, -isMetal, -isBlues, -isPunk)
for (each in 3:dim(myData)[2])
{
  if (!is.number(myData[1,each]))
  {
    myData[,each] <- as.numeric(as.factor(unlist(myData[,each])))
  }
  myData[,each] <- standardize(myData[,each])
  
}
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

#train.X <- standardize(train.X)
#train.Y <- (train.Y)
#train.Y <- standardize(train.Y)
#test.X <- standardize(test.X)
#test.Y <- (test.Y)
#test.Y <- standardize(test.Y)
k = 3


goodness <- vector()
for(k in 1:20)
{
  print(k)
  goodness[k] <- knn.regression.test(k,train.X,train.Y,test.X,test.Y, distances.l2)
}
print(which.min(goodness))

est <- knn.regression.test(2,train.X,train.Y,test.X,test.Y, distances.l2)
data.frame(est, test.Y, test.Y - est)



library(glmnet)

lambdas <- 10^seq(2, -2, by = -.1)
ridge <- glmnet(y = train.Y, x = train.X, lambda = lambdas)
lambdaFind <- cv.glmnet(y = train.Y, x = train.X, alpha = 0, lambda = lambdas)
optimalLambda <- lambdaFind$lambda.min
estimates <- predict(ridge, s = optimalLambda, newx = test.X)
print("Ridge regression")
print(sum(((test.Y-estimates)^2)))