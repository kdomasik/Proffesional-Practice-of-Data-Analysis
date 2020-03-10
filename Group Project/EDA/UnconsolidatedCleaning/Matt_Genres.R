library(timevis)
library(rio)
library(data.table)
library(ggplot2)
library(dplyr)
library(lubridate)
library(xlsx)

myData <- import("edited_spotify_04112019.csv", setclass = "tibble")
#myData <- mutate(myData, ArtistGenres2 = strsplit(ArtistGenres, ","))

Genres <- myData$ArtistGenres
Genre <- vector(mode = "list", 1)
count = 1
for(each in Genres)
{
  for(i in as.list(strsplit(each, ",")[[1]]))
  {
    Genre[count] = i
    count = count + 1
  }
}
Genre <- unique(Genre)

addCol <- function(myData, names, isName, Genre)
{
  isGenre <- as.list(grep(paste(names,collapse="|"), Genre, value=TRUE))
  isGenre <- unique(isGenre)
  numberCol <- ncol(myData) + 1
  for(each in 1:length(myData$Artist))
  {
    myData[each, numberCol] =  length(intersect(as.list(strsplit(myData[each, 4][[1]], ",")[[1]]), isGenre)) > 0
  }
  colnames(myData)[numberCol] <- paste (isName, sep = "_", collapse = NULL)
  return(myData)
}
reducedGenre <- Genre[! Genre  %in% as.list(grep("pop", Genre, value=TRUE))]
reducedGenre <- reducedGenre[! reducedGenre  %in% as.list(grep("rock", Genre, value=TRUE))]
reducedGenre <- reducedGenre[! reducedGenre  %in% as.list(grep("folk", Genre, value=TRUE))]
reducedGenre <- reducedGenre[! reducedGenre  %in% as.list(grep("country", Genre, value=TRUE))]
reducedGenre <- reducedGenre[! reducedGenre  %in% as.list(grep("electro", Genre, value=TRUE))]
reducedGenre <- reducedGenre[! reducedGenre  %in% as.list(grep("techno", Genre, value=TRUE))]
reducedGenre <- reducedGenre[! reducedGenre  %in% as.list(grep("hip hop", Genre, value=TRUE))]
reducedGenre <- reducedGenre[! reducedGenre  %in% as.list(grep("grime", Genre, value=TRUE))]
reducedGenre <- reducedGenre[! reducedGenre  %in% as.list(grep("rap", Genre, value=TRUE))]
reducedGenre <- reducedGenre[! reducedGenre  %in% as.list(grep("jazz", Genre, value=TRUE))]
reducedGenre <- reducedGenre[! reducedGenre  %in% as.list(grep("punk", Genre, value=TRUE))]
reducedGenre <- reducedGenre[! reducedGenre  %in% as.list(grep("soul", Genre, value=TRUE))]
reducedGenre <- reducedGenre[! reducedGenre  %in% as.list(grep("metal", Genre, value=TRUE))]
reducedGenre <- reducedGenre[! reducedGenre  %in% as.list(grep("blues", Genre, value=TRUE))]
reducedGenre <- reducedGenre[! reducedGenre  %in% as.list(grep("indie", Genre, value=TRUE))]
reducedGenre <- reducedGenre[! reducedGenre  %in% as.list(grep("dance", Genre, value=TRUE))]
reducedGenre <- unique(reducedGenre)

myData <- addCol(myData, c("pop"), "isPop",Genre)
myData <- addCol(myData, c("rock"), "isRock",Genre)
myData <- addCol(myData, c("folk", "country"), "isFolk",Genre)
myData <- addCol(myData, c("electro", "techno"), "isTechno",Genre)
myData <- addCol(myData, c("hip hop","grime", "rap"), "isRap",Genre)
myData <- addCol(myData, c("jazz"), "isJazz",Genre)
myData <- addCol(myData, c("indie"), "isIndie",Genre)
myData <- addCol(myData, c("metal"), "isMetal",Genre)
myData <- addCol(myData, c("blues", "r&b", "soul"), "isBlues",Genre)
myData <- addCol(myData, c("punk"), "isPunk",Genre)

# for(each in 1:length(myData$Artist))
# {
#   myData[each, 4] =  paste(myData[each, 4][[1]], sep = ",", collapse = ",")[[1]]
# }
# list = myData$ArtistGenres

#testData <- filter(myData, (myData$isIndie == FALSE & myData$isMetal == FALSE & myData$isBlues == FALSE & myData$isPunk == FALSE & myData$isFolk == FALSE & myData$isPop == FALSE & myData$isRock == FALSE & myData$isTechno == FALSE & myData$isRap == FALSE & myData$isJazz == FALSE))
#badGenre = unique(testData$ArtistGenres)
write.csv(myData, file = "cleanedData.csv")