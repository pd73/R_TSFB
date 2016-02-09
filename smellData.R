smellData <- function(fileName) {
  # Read in a Smell Threshold file
  # - This function gets data into a format useful for batch analysis
  # - it is vulnerable to files with imperfect structure
  # Make sure analysis directory is cleaned up
  
  
  tryCatch(
{
  fileData1 <-read.xlsx(fileName, 1, header = F)
  fileData2 <-read.xlsx(fileName, 2, header = T)
  fileData3 <-read.xlsx(fileName, 3, header = T)
  
  track <- fileData3$Strength
  ntrial <- length(track)
  stim <- fileData3$Bottle
  resp <- fileData3$Response
  corr <- rep(F, ntrial)
  thresh <- rep(NA, ntrial)
  STT_ID <- rep(NA, ntrial)
  STT_ID[1:11] <- as.character(fileData1[,2])
  
  corr[fileData3$Bottle == "Odor"] <- fileData3$Response[fileData3$Bottle == "Odor"] == 'First'
  corr[fileData3$Bottle == "Blank"] <- fileData3$Response[fileData3$Bottle == "Blank"] == 'Second'
  nrevs <- length(fileData2$Strength[fileData2$Reversal == T])
  thresh[3:(nrevs+2)] <- fileData2$Strength[fileData2$Reversal == T]
  thresh[2] <- fileData2$Strength[fileData2$Reversal == TRUE][1]
  thresh[1] <- as.numeric(as.character(fileData1$X2[fileData1$X1 == 'Threshold']))
},

error = function(err) {print("There was an error in this file")
                       track <- -1:-10
                       stim <- rep(c("Blank", "Odor"),5)
                       resp <- rep(c("First", "Second"),5)
                       corr <- rep(0,10)
                       thresh <- rep(99, 10)
},

finally = {
  thisData <- data.frame(STT_ID, track, stim, resp, corr, thresh, check.rows = TRUE)
  names(thisData) <- c('info', 'track', 'stim', 'resp', 'corr', 'thresh')}
  )
return(thisData)
}