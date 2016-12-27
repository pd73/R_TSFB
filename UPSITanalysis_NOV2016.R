library("ez")
library("dplyr")
library("xlsx")

# filedir = 'C:/Users/pallen/Box Sync/Developmental Neuropsych Lab Management/Bennetto Lab Writing/Olfactory Fx in Autism Paper/Data'
# 
# UPSITdatafile <- 'OlfactionData_2016-11-13.xlsx'
# 
# smell <-  read.xlsx(paste(sep = "", collapse = NULL, filedir, '/', UPSITdatafile),1)

smell <- read.csv("C:/Users/pallen/Box Sync/Developmental Neuropsych Lab Management/Bennetto Lab Writing/Olfactory Fx in Autism Paper/Data/OlfactionData_2016-11-13.csv")

#Clean data
smell <- smell[smell$TSFB.excluded != "yes",]
smell <- smell[smell$TSFB.group != "Parent",]

smell$TSFB.group<-droplevels(smell$TSFB.group)

# FamilyID <- as.character(smell$dem.familyID)
# SID <- paste('TS', FamilyID, smell$memberID, sep ="")
# group <- paste(smell$Group, smell$tyoe, sep = "_")
# 
# smelldata <- data.frame(SID, FamilyID, smell$gender, smell$Age, group, smell$smell_SCORE)
# names(smelldata) <- c('SID','Family', 'gender', 'age', 'group', 'score')

# This section is for data clean-up, e.g. to make sure everyone has a valid age
# smelldata$SID[smell$TSFB_UPSIT..UPSIT.CA> 70]
# smelldata$SID[is.na(smelldata$age)]
# # data cleaned manually 1155 E,F, G not used

with(smell, table(TSFB_UPSIT..UPSIT.01, TSFB.group, useNA = "no"))

chisqr<- chisq.test(table(smell$TSFB.group,smell$TSFB_UPSIT..UPSIT.20))



numtoanalyse = 40
UPSIT_F3 <- rep(NA,numtoanalyse)
UPSIT_p3 <- rep(NA,numtoanalyse)
UPSIT_F2 <- rep(NA,numtoanalyse)
UPSIT_p2 <- rep(NA,numtoanalyse)
UPSIT_AA_ratio <- rep(NA,numtoanalyse)
UPSIT_AC_ratio <- rep(NA,numtoanalyse)
UPSIT_B_ratio <- rep(NA,numtoanalyse)
UPSIT_NA_ratio <- rep(NA,numtoanalyse)



for (i in 1:numtoanalyse) { 
  if (i<10) {numstr <- paste("0",as.character(i), sep ="")
  } else {numstr <- as.character(i)}
  
  table(smell$TSFB.group,eval(parse(text = paste0("smell$TSFB_UPSIT..UPSIT.",numstr))))
  chisqr<- chisq.test(table(smell$TSFB.group,eval(parse(text = paste0("smell$TSFB_UPSIT..UPSIT.",numstr)))))
  UPSIT_F3[i] <- chisqr$statistic
  UPSIT_p3[i] <- chisqr$p.value
  
  table(smell$ASDvsNot,eval(parse(text = paste0("smell$TSFB_UPSIT..UPSIT.",numstr))))
  chisqr<- chisq.test(table(smell$ASDvsNot,eval(parse(text = paste0("smell$TSFB_UPSIT..UPSIT.",numstr)))))
  UPSIT_F2[i] <- chisqr$statistic
  UPSIT_p2[i] <- chisqr$p.value
  
  temp <- eval(parse(text = paste0("smell$TSFB_UPSIT..UPSIT.",numstr)))[smell$TSFB.group == "Autism"]
  UPSIT_AA_ratio[i] <- sum(temp, na.rm = TRUE)/length(temp)
  temp <- eval(parse(text = paste0("smell$TSFB_UPSIT..UPSIT.",numstr)))[smell$TSFB.group == "Control"]
  UPSIT_AC_ratio[i] <- sum(temp, na.rm = TRUE)/length(temp)
  temp <- eval(parse(text = paste0("smell$TSFB_UPSIT..UPSIT.",numstr)))[smell$TSFB.group == "Sibling"]
  UPSIT_B_ratio[i] <- sum(temp, na.rm = TRUE)/length(temp)
  temp <- eval(parse(text = paste0("smell$TSFB_UPSIT..UPSIT.",numstr)))[smell$ASDvsNot == 0]
  UPSIT_NA_ratio[i] <- sum(temp, na.rm = TRUE)/length(temp)
}

UPSIT_performance <- data.frame(UPSIT_AA_ratio, UPSIT_AC_ratio, UPSIT_B_ratio, UPSIT_NA_ratio, UPSIT_F3, UPSIT_p3, UPSIT_F2, UPSIT_p2)

write.csv(UPSIT_performance, file = paste('UPSIT_performance_',Sys.Date(),".csv", sep = ''))

### old stuff below here

famList <- unique(smelldata$Family[smelldata$group == "Proband_Family"])
numFam <- length(famList)
                  
ASDfamilies <- data.frame(famList,rep(NA, numFam),rep(NA, numFam),rep(NA, numFam),rep(NA, numFam),rep(NA, numFam), rep(NA, numFam),rep(NA, numFam),rep(NA, numFam),rep(NA, numFam))
names(ASDfamilies) <- c('FID','proband','sibling','mom','dad', 'famAve', 'parentAve', 'trioAve', 'sibsDiff', 'probparDiff')

for (i in 1:numFam) {
  ASDfamilies$proband[i] <- smelldata$score[smelldata$Family == ASDfamilies$FID[i] & (smelldata$group == "Proband_Family")]
  ASDfamilies$sibling[i] <- mean(smelldata$score[smelldata$Family == ASDfamilies$FID[i] & (smelldata$group == "Sibling_Family")])
  ASDfamilies$mom[i] <- mean(smelldata$score[smelldata$Family == ASDfamilies$FID[i] & (smelldata$group == "Mother_Family")])
  ASDfamilies$dad[i] <- mean(smelldata$score[smelldata$Family == ASDfamilies$FID[i] & (smelldata$group == "Father_Family")])
  ASDfamilies$famAve[i] <- mean(smelldata$score[smelldata$Family == ASDfamilies$FID[i]])
  ASDfamilies$trioAve[i] <- (ASDfamilies$proband[i] + ASDfamilies$mom[i] + ASDfamilies$dad[i])/3
  ASDfamilies$parentAve[i] <-  (ASDfamilies$mom[i] + ASDfamilies$dad[i])/2
  ASDfamilies$sibsDiff[i] <- ASDfamilies$proband[i] - ASDfamilies$sibling[i]
  ASDfamilies$probparDiff[i] <- ASDfamilies$proband[i] - ASDfamilies$parentAve[i]
}

write.csv(ASDfamilies, "C:/Users/pallen/Desktop/Dropbox/@R-working/gustometry/smell_FamiliesDEC2014.csv")

write.csv(smelldata, "S:/Research/BennettoLab/TSFB Study/Data/smelldataforSPSS_2014-12-01.csv")

ezDesign(
  smelldata
  , x = score
  , y = group
  , cell_border_size = 10
)

ezPlot(
  smelldata
  , dv = score
  , wid = SID
  , within = NULL
  , between = .(group)

  , x = group
  , do_lines = FALSE
  , do_bars = TRUE
  , bar_width = NULL
  , bar_size = NULL
  , split = NULL
  , row = NULL
  , col = NULL
  , to_numeric = NULL
  , x_lab = NULL
  , y_lab = NULL
  , split_lab = NULL
  , levels = NULL
  , diff = NULL
  , reverse_diff = FALSE
  , type = 3
  , dv_levs = NULL
  , dv_labs = NULL
  , y_free = FALSE
  , print_code = FALSE
)

head(smelldata)
ezPrecis(smelldata)

ezStats(
  smelldata
  , dv = score
  , wid = SID
  , within = NULL
  , between = .(group)
  , type = 3
)

ezANOVA(
  smelldata
  , dv = score
  , wid = SID
  , within = NULL
  , between = .(group)
  , type = 3
)