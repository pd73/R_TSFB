gustometerData <- function(fileName, timeStamps = FALSE) {
# Read in an electrogustometry file
# If fewer than two time stamps are read, the function returns empty
# Otherwise assumes that last two stamps delineate the data of interest
# i.e. there was a restart before the set that was used
# Output is dataframe of file contents with new column of 'Correct'
# If timeStamps is TRUE then the list of timestamps is returned
  
fileData <-readLines(fileName)

#verify that there are at least two time stamps - indicating completed data
searchS = 'Time: '
times = as.numeric(grepl(searchS,fileData))
numStamps <- sum(times)

if (numStamps <2) {
  return()
}

startnend = which(times == 1)

if (startnend[2]-startnend[1]<18) {
  return()
}

if (timeStamps) {
  return(fileData[startnend])
}

if (numStamps > 2) {
  startnend<-rev(rev(startnend)[1:2])
  #this takes the last two elements of startnend
}



thisData <- read.table(
  fileName,
  header = FALSE,
  sep = "\t",
  col.names = c("Stim","nLev","Resp"),
  nrows = startnend[2]-startnend[1]-1,
  skip = startnend[1]
)

actStim <- c(6,6,6)
sideNum <- c(1,2,1)
rNum <- 2
lNum <- 3
levels(thisData$Resp) <- c("L", "R")

if (thisData$Stim[1] == thisData$Resp[1]) {
  actStim[2] = 6
} else {actStim[2] = 8}
  
  
for (i in 4:length(thisData$Stim)){
  if (thisData$Stim[i] == "R") {
    sideNum[i] <- rNum
    rNum <- rNum+1
  } else {sideNum[i] <- lNum
          lNum <- lNum+1}
  if (thisData$Stim[i] == thisData$Stim[i-1]) {
    actStim[i] = thisData$nLev[i-1]
  } else if (thisData$Stim[i] == thisData$Stim[i-2]) {
    actStim[i] = thisData$nLev[i-2]
  } else if (thisData$Stim[i] == thisData$Stim[i-3]) {
    actStim[i] = thisData$nLev[i-3]  
  } else if (thisData$Stim[i] == thisData$Stim[i-4]) {
    actStim[i] = thisData$nLev[i-4]  }
}

thisData$corr <- as.numeric(thisData$Stim == thisData$Resp)
thisData$track <- actStim
thisData$num <- sideNum

return(thisData)
}