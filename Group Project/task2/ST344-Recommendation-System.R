library(dplyr)
library(readxl)
library(robustHD)

#read the data
data <- read.csv("cleanedData.csv")
mydata <- dplyr::select(data, ArtistID, AlbumID, TrackID, AlbumPopularity, TrackDanceability, TrackEnergy, TrackLoudness, TrackSpeechiness, TrackAcousticness, TrackInstrumentalness, TrackLiveness, TrackValence, TrackTempo)


#Remove the songs from the same album
#This part of code delete the songs from the same album.
delete_same_album <- function(trackid, data1){
  albumid <- data1$AlbumID[mydata$TrackID == trackid]
  n <- which(grepl(albumid, data1$AlbumID))
  index <- 1:dim(data1)[1]
  newindex <- index[!index %in% n]
  return(newindex) # this part return the index for chosen tracks with respect to the original data set
}


#Eulidean Distance
#this part will be used as the metric for k-n-n algorithm.
distances.l2 <- function(x,y){
    n <- length(x)
    distance <- 0
    for (i in 1:n) {
      distance <- distance + ((x[i] - y[i])^2)
    }
    return(sqrt(distance))
}


# Extract
#This function extract the standardize ocontinuous variable.
library(robustHD)
extract <- function(trackid){
  n <- which(grepl(trackid, mydata$TrackID))
  newdata <- standardize(mydata[,5:13])
  return(as.matrix(newdata[n,]))
}
test <- extract("4q3z6aThomt6qSdVhjW8R8")


#Subset data by release year
#this part of code returns the data of tracks that released in the recent decade of a particular song:
decade_data <- function(trackid,data1){
  release_year <- data$Release_year[data$TrackID == trackid]
  decade <- (release_year - 5):(release_year + 5)
  decades <- which(as.vector(data1$Release_year) %in% decade)
  return(decades)
}


#Genre
genresubset <- function(trackid, dataset){
  row1 <- data[data$TrackID==trackid, ]
  index <- c()
  for (i in 31:40) {
    if(row1[i] == TRUE){
      index <- c(index,i)
    }
  }
  
 n <- dim(dataset)[1]
  n1 <- length(index)
  removeindex <- c()
  
  for (j in 1:n) {
    if(identical(dataset[j, index], rep(FALSE, n1)) == TRUE){
      removeindex <- c(removeindex, j)
    }
  }
  if(length(removeindex) == 0){
    return(dataset)
  }else{
  return(dataset[-removeindex,])
  }
}


#Probability
probability <- function(rec){
  n <- length(rec)
  distribution <- c()
  for (i in 1:n) {
    distribution <- c(distribution, data$AlbumPopularity[data$TrackID == rec[i]])
  }
  distribution <- distribution/sum(distribution)
  return(distribution)
}


#Convertor
convertid <- function(trackid){
  artist <- as.character(data$Artist[data$TrackID == trackid])
  track <- as.character(data$TrackName[data$TrackID == trackid])
  album <- as.character(data$AlbumName[data$TrackID == trackid])
  result <- paste(artist, ":", album, ":", track)
  print(result)
}


#Main Code
main <- function(trackid){
  x <- extract(trackid)
  standardizedata <- standardize(data[,c(16,17,19,21:26)]) #we must standardize the data set before subsetting
  data_album <- data[delete_same_album(trackid, data),] #delete tracks within the same album
  data1 <- data_album[decade_data(trackid, data_album),] #subset the data within the nearest 10 years
 
  if(dim(genresubset(trackid, data1))[1] != 0){
    data1 <- genresubset(trackid,data1)
  } #choose the songs that are within the nearest decade and have at least one common genre, provided such tracks exist
  
  train.index1 <- which(data$TrackID %in% data1$TrackID)
  train.X1 <- standardizedata[train.index1,]
  train.X1 <- as.matrix(train.X1)
  n1 <- dim(train.X1)[1]
  distances.array1 <- rep(0,n1)

    for (i in 1:n1){
    distances.array1[i] <-distances.l2(x, as.vector(train.X1[i,]))
    }
  num1 <- min(5,n1)
  shortest_distance1 <- sort(distances.array1)[1:num1]
  neibourindex1 <- match(shortest_distance1, distances.array1) #this part use k-nearst-neibour algorithm
  neibour1 <- as.character(data1[neibourindex1,]$TrackID)
  
  data2 <- data_album[-decade_data(trackid, data_album),]#here choose the tracks that are not in the '10-years period'
  
  if(dim(genresubset(trackid, data2))[1] != 0){
    data1 <- genresubset(trackid,data2)
  }
  
  train.index2 <- which(data$TrackID %in% data2$TrackID)
  train.X2 <- standardizedata[train.index2, ]
  train.X2 <- as.matrix(train.X2)
  n2 <- dim(train.X2)[1]
  distances.array2 <- rep(0,n2)
  
  for (j in 1:n2) {
    distances.array2[j] <- distances.l2(x, as.vector(train.X2[j,]))
  }
  num2 <- min(5,n2)
  shortest_distance2 <- sort(distances.array2)[1:num2]
  neibourindex2<- match(shortest_distance2, distances.array2)
  neibour2 <- as.character(data2[neibourindex2,]$TrackID)
  
  neibour <- c(neibour1, neibour2)
  distribution <- probability(neibour)
  sample <- sample(neibour, size = 5, replace = FALSE, prob = distribution)
  
  for (k in 1:5){
    convertid(sample[k])
  }
}