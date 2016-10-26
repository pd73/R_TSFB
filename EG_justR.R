require(rmarkdown)
require(ggplot2)
require(aod)
require(stats)
library("psyphy")
library("knitr")
library("xlsx")

#ENT 
dataDir = 'X:/Research/BennettoLab/TSFB Study/Data/Cleaned data/Electrogustometry Data Files'
filedir = 'X:/Research/BennettoLab/TSFB Study/Data/Cleaned data'
source('C:/Users/pallen/Documents/GitHub/R_TSFB/gustometerData.R')

#notefile = 'notesEGST_2016_02_05.xlsx'
#demofile = 'DemoEG_2016_02_05.xlsx'
notefile = 'EG_ST_NOTES_2016_10_21.xlsx'
demofile = 'DemoEG_2016_10_21.xlsx'

gustnotes <-  read.xlsx(paste(sep = "", collapse = NULL, filedir, '/', notefile),1)
DemoEG <-  read.xlsx(paste(sep = "", collapse = NULL, filedir, '/', demofile),1)

gustnotes$ID <- paste('TS', gustnotes$EGSTT.FamilyID, gustnotes$EGSTT.MemberID, sep ="")
DemoEG$type <- paste(DemoEG$dem.pedPosition, DemoEG$dem.research.group, sep ="_")

allFiles = dir(dataDir, pattern ="*.txt")
numStamps = rep(NA,length(allFiles))
for (i in 1:length(allFiles)) {
  fileName <- paste(dataDir,"/",allFiles[i], sep='')
  numStamps[i] <- length(gustometerData(fileName, timeStamps = TRUE))
}
timeStampData = data.frame(
  id = allFiles,
  ntimes = numStamps
)
SID <- substr(allFiles, start = 7, stop = 13)
status <- rep(NA, length(SID))
excluded <- rep(NA, length(SID))
gender <- rep(NA, length(SID))
age <- rep(NA, length(SID))
group <- rep(NA, length(SID))
L_logThresh <- rep(NA, length(SID))
L_logprob <- rep(NA, length(SID))
R_logThresh <- rep(NA, length(SID))
R_logprob <- rep(NA, length(SID))
L_revThresh <- rep(NA, length(SID))
R_revThresh <- rep(NA, length(SID))
L_allRevs <- rep(NA, length(SID))
R_allRevs <- rep(NA, length(SID))
L_last4 <- rep(NA, length(SID))
R_last4 <- rep(NA, length(SID))
L_nRev <- rep(NA, length(SID))
R_nRev <- rep(NA, length(SID))
L_Range <- rep(NA, length(SID))
R_Range <- rep(NA, length(SID))
L_Stdev <- rep(NA, length(SID))
R_Stdev <- rep(NA, length(SID))
chiSqr <- rep(NA, length(SID))
GuessAll <- rep(NA, length(SID))
guessLeft <- rep(NA, length(SID))
guessRight <- rep(NA, length(SID))
propLeft <- rep(NA, length(SID))
pLeft <- rep(NA, length(SID))
propRight <- rep(NA, length(SID))
pRight <- rep(NA, length(SID))
pchi <- rep(NA, length(SID))
Rratio <- rep(NA, length(SID))
bias <- rep(NA, length(SID))
biasRept <- rep(NA, length(SID))
note <- rep(NA, length(SID))
demonote <- rep(NA, length(SID))

summaryData <- data.frame(SID,status, excluded, gender,age,group,chiSqr,pchi,GuessAll,Rratio,bias,biasRept,L_nRev, R_nRev, L_revThresh, R_revThresh,L_allRevs, R_allRevs,L_last4,R_last4,L_Range,R_Range,L_Stdev,R_Stdev,L_logThresh, R_logThresh,propRight,pRight,guessRight,R_logprob, propLeft,pLeft, guessLeft,L_logprob, note, demonote)

numtoanalyse = length(allFiles)
for (i in 1:numtoanalyse) {
  thisNote <- gustnotes$EGSTT.EGadminNotes[gustnotes$ID == SID[i]]
  thisNote <- paste(thisNote, collapse="; ")
  if (length(thisNote)==0) {thisNote = 'No note in the database!'}
  summaryData$note[i] <- thisNote
  
  thisGender <- as.character(DemoEG$dem.gender[DemoEG$Dem.recordID == SID[i]])
  if (length(thisGender)==0) {thisGender = 'NA'}  
  summaryData$gender[i] <- thisGender
  
  thisStatus <- as.character(DemoEG$dem.status[DemoEG$Dem.recordID == SID[i]])
  if (length(thisStatus)==0) {thisStatus = 'NA'}  
  summaryData$status[i] <- thisStatus
  
  thisExcluded <- as.character(DemoEG$TSFB.excluded[DemoEG$Dem.recordID == SID[i]])
  if (length(thisExcluded)==0) {thisExcluded = 'NA'}  
  summaryData$excluded[i] <- thisExcluded
  
  thisAge <- DemoEG$AgeEval[DemoEG$Dem.recordID == SID[i]]
  if (length(thisAge)==0) {thisAge = 'NA'} 
  summaryData$age[i] <- thisAge
  
  thisGroup <- DemoEG$type[DemoEG$Dem.recordID == SID[i]]
  if (length(thisGroup)==0) {thisGroup = 'NA'} 
  summaryData$group[i] <- thisGroup
  
  summaryData$demonote[i] <- paste(DemoEG$TSFB.excluded_notes[DemoEG$Dem.recordID == SID[i]], collapse="; ")
  if (numStamps[i] == 2) {
    
    fileName <- paste(dataDir,"/",allFiles[i], sep='')
    trialTracks <- gustometerData(fileName)
    
    # Responding at chance Calculation
    nLeft <- sum(trialTracks$Stim == 'L')
    corLeft <- sum(trialTracks$corr[trialTracks$Stim == 'L'])
    #minL <- min(trialTracks$track[trialTracks$Stim == 'L'])
    lastL <- trialTracks$track[trialTracks$Stim == 'L'][length(trialTracks$track[trialTracks$Stim == 'L'])]
    nRight <- sum(trialTracks$Stim == 'R')
    corRight <- sum(trialTracks$corr[trialTracks$Stim == 'R'])
    #minR <- min(trialTracks$track[trialTracks$Stim == 'R'])
    lastR <- trialTracks$track[trialTracks$Stim == 'R'][length(trialTracks$track[trialTracks$Stim == 'R'])]
    fractionLR <- nLeft/(nLeft+nRight)
    rLeft <- sum(trialTracks$Resp == 'L')
    rRight <- sum(trialTracks$Resp == 'R')
    # these go to the binomial tests below
    
    #Left Side
    leftT <- trialTracks$track[trialTracks$Stim == "L"]
    leftD <- rep(0,length(leftT))
    for (j in 2:length(leftT)){
      if (leftT[j]<leftT[j-1]) {
        leftD[j] <- -1
      } else if (leftT[j]>leftT[j-1]) {
        leftD[j] <- 1
      } else {leftD[j] <- leftD[j-1]}
    }
    
    leftD[length(leftD)+1] <- 1-2*trialTracks$corr[trialTracks$Stim == "L"][length(leftD)]
    revL <- rep(NA,4)
    k = 1
    for (j in 2:length(leftD)){
      if (abs(leftD[j]-leftD[j-1])==2) {
        revL[k] = leftT[j-1]
        k<- k+1
      }
    }
    revL[is.na(revL)] <- leftT[nLeft]
    nrevL <- length(revL)

    xdatL <- c(trialTracks$track[trialTracks$Stim =='L'],-20, 50)
    ydatL <- c(trialTracks$corr[trialTracks$Stim =='L'],0,1)
    
    levs <- sort(unique(xdatL))
    correct <- rep(0, length(levs))
    incorrect <- rep(0, length(levs))
    fraction <- rep(0, length(levs))

    for (j in 1:length(levs)) {
      correct[j] <- sum(ydatL[xdatL == levs[j]])
      incorrect[j] <- sum(1-ydatL[xdatL == levs[j]])
      fraction[j] <- correct[j] /(correct[j] +incorrect[j])
    }
    
    datL <- as.data.frame(cbind(levs,correct,incorrect,fraction))
    gLeft <- glm(cbind(correct, incorrect) ~ levs,family=binomial(mafc.logit(2)),datL)
    
    pLogL <- with(gLeft, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))
    
    #Right Side
    rightT <- trialTracks$track[trialTracks$Stim == "R"]
    rightD <- rep(0,length(rightT))
    
    for (j in 2:length(rightT)){
      if (rightT[j]<rightT[j-1]) {
        rightD[j] <- -1
      } else if (rightT[j]>rightT[j-1]) {
        rightD[j] <- 1
      } else {rightD[j] <- rightD[j-1]}
    }
    rightD[length(rightD)+1] <- 1-2*trialTracks$corr[trialTracks$Stim == "R"][length(rightD)]
    revR <- rep(NA,4)
    k = 1
    for (j in 2:length(rightD)){
      if (abs(rightD[j]-rightD[j-1])==2) {
        revR[k] = rightT[j-1]
        k<- k+1
      }
    }
    revR[is.na(revR)] <- rightT[nRight]
    nrevR <- length(revR)
    
    xdatR <- c(trialTracks$track[trialTracks$Stim =='R'], -20, 50)
    ydatR <- c(trialTracks$corr[trialTracks$Stim =='R'], 0, 1)
    
    levs <- sort(unique(xdatR))
    correct <- rep(0, length(levs))
    incorrect <- rep(0, length(levs))
    fraction <- rep(0, length(levs))
    for (j in 1:length(levs)) {
      correct[j] <- sum(ydatR[xdatR == levs[j]])
      incorrect[j] <- sum(1-ydatR[xdatR == levs[j]])
      fraction[j] <- correct[j] /(correct[j] +incorrect[j])
    }
    
    datR <- as.data.frame(cbind(levs,correct,incorrect,fraction))
    gRight <- glm(cbind(correct, incorrect) ~ levs,family=binomial(mafc.logit(2)),datR)
    
    pLogR <- with(gRight, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))
    
    if (is.na(mean(revR[3:6],na.rm = TRUE))) {
      revR <- rep(lastR,6)
    }
    
    if (is.na(mean(revL[3:6],na.rm = TRUE))) {
      revL <- rep(lastL,6)
    }
    
    biasp <- binom.test(c(rLeft, rRight), p = fractionLR)
    if (biasp$p.value < 0.05) {
      biasResp <- "biased"
    } else {biasResp <- "not biased"}
    
    levels(trialTracks$Stim) <- c("Stim L", "Stim R")
    levels(trialTracks$Resp) <- c("Resp L", "Resp R")
    
    chisqr<- chisq.test(table(trialTracks$Stim,trialTracks$Resp))
    
    summaryData$L_Range[i] <- max(leftT)- min(leftT)
    summaryData$R_Range[i] <- max(rightT)- min(rightT)
    summaryData$L_last4[i] <- mean(leftT[(nLeft-3):nLeft])
    summaryData$R_last4[i] <- mean(rightT[(nRight-3):nRight])
    summaryData$L_nRev[i] <- nrevL
    summaryData$R_nRev[i] <- nrevR
    
    summaryData$chiSqr[i] <- chisqr$statistic
    summaryData$pchi[i] <- chisqr$p.value
    summaryData$Rratio[i] <- rRight/(rLeft + rRight)
    summaryData$bias[i] <- biasp$p.value
    summaryData$biasRept[i] <- biasResp
    
    summaryData$L_logThresh[i] <- -coef(gLeft)[1] / coef(gLeft)[2]
    summaryData$L_logprob[i] <- pLogL
    if (pLogL < 0.05) {
      L_ThresSig = "sig"} else { L_ThresSig = "not sig"}
    summaryData$L_revThresh[i] <- mean(revL[3:6])
    summaryData$L_allRevs[i] <- mean(revL,na.rm = TRUE)
    summaryData$L_Stdev[i] <- sd(revL[3:6])
    
    summaryData$R_logThresh[i] <-  -coef(gRight)[1] / coef(gRight)[2]
    summaryData$R_logprob[i] <- pLogR
    if (pLogR < 0.05) {
      R_ThresSig = "sig"} else { R_ThresSig = "not sig"}
    summaryData$R_revThresh[i] <- mean(revR[3:6])
    summaryData$R_allRevs[i] <- mean(revR,na.rm = TRUE)
    summaryData$R_Stdev[i] <- sd(revR[3:6])
    
    if (is.na(chisqr$p.value)) {
      GuessAll <- "fully biased"
    } else if (chisqr$p.value < 0.05) {
      GuessAll <- "not guessing"
    } else {
      GuessAll <- "maybe guessing!"
      if (chisqr$statistic < 1) {GuessAll <- "surely guessing"}
    }
    
    Leftbinomial <- binom.test(corLeft, nLeft, 0.5)
    if (Leftbinomial$p.value < 0.05) {
      guessLeft <- "not guessing"
    } else {
      guessLeft <- "guessing"
      optimal <- binom.test(corLeft, nLeft, 0.666)
      if (optimal$p.value > 0.05) {guessLeft <- "2D-1U optimal"}
    }
    
    Rightbinomial <- binom.test(corRight, nRight, 0.5)
    if (Rightbinomial$p.value < 0.05) {
      guessRight <- "not guessing"
    } else {
      guessRight <- "guessing"
      optimal <- binom.test(corRight, nRight, 0.666)
      if (optimal$p.value > 0.05) {guessRight <- "2D-1U optimal"}
    }
    
    summaryData$GuessAll[i] <- GuessAll
    summaryData$propLeft[i] <- corLeft/nLeft
    summaryData$guessLeft[i] <- guessLeft
    
    summaryData$pLeft[i] <- Leftbinomial$p.value
    summaryData$propRight[i] <- corRight/nRight
    summaryData$guessRight[i] <- guessRight
    summaryData$pRight[i] <- Rightbinomial$p.value
  }
}

write.csv(summaryData, file = paste('EG_SummaryData_Draft_',Sys.Date(),".csv", sep = ''))