library("ez")
library("dplyr")

UPSIT <- read.csv("C:/Users/pallen/Desktop/Dropbox/@R-working/gustometry/UPSIT_DEC2014.csv")

FamilyID <- as.character(UPSIT$FamilyID)
SID <- paste('TS', FamilyID, UPSIT$memberID, sep ="")
group <- paste(UPSIT$Group, UPSIT$tyoe, sep = "_")

UPSITdata <- data.frame(SID, FamilyID, UPSIT$gender, UPSIT$Age, group, UPSIT$UPSIT_SCORE)
names(UPSITdata) <- c('SID','Family', 'gender', 'age', 'group', 'score')

# This section is for data clean-up, e.g. to make sure everyone has a valid age
UPSITdata$SID[UPSITdata$age < 7]
UPSITdata$SID[is.na(UPSITdata$age)]
# data cleaned manually 1155 E,F, G not used

famList <- unique(UPSITdata$Family[UPSITdata$group == "Proband_Family"])
numFam <- length(famList)
                  
ASDfamilies <- data.frame(famList,rep(NA, numFam),rep(NA, numFam),rep(NA, numFam),rep(NA, numFam),rep(NA, numFam), rep(NA, numFam),rep(NA, numFam),rep(NA, numFam),rep(NA, numFam))
names(ASDfamilies) <- c('FID','proband','sibling','mom','dad', 'famAve', 'parentAve', 'trioAve', 'sibsDiff', 'probparDiff')

for (i in 1:numFam) {
  ASDfamilies$proband[i] <- UPSITdata$score[UPSITdata$Family == ASDfamilies$FID[i] & (UPSITdata$group == "Proband_Family")]
  ASDfamilies$sibling[i] <- mean(UPSITdata$score[UPSITdata$Family == ASDfamilies$FID[i] & (UPSITdata$group == "Sibling_Family")])
  ASDfamilies$mom[i] <- mean(UPSITdata$score[UPSITdata$Family == ASDfamilies$FID[i] & (UPSITdata$group == "Mother_Family")])
  ASDfamilies$dad[i] <- mean(UPSITdata$score[UPSITdata$Family == ASDfamilies$FID[i] & (UPSITdata$group == "Father_Family")])
  ASDfamilies$famAve[i] <- mean(UPSITdata$score[UPSITdata$Family == ASDfamilies$FID[i]])
  ASDfamilies$trioAve[i] <- (ASDfamilies$proband[i] + ASDfamilies$mom[i] + ASDfamilies$dad[i])/3
  ASDfamilies$parentAve[i] <-  (ASDfamilies$mom[i] + ASDfamilies$dad[i])/2
  ASDfamilies$sibsDiff[i] <- ASDfamilies$proband[i] - ASDfamilies$sibling[i]
  ASDfamilies$probparDiff[i] <- ASDfamilies$proband[i] - ASDfamilies$parentAve[i]
}

write.csv(ASDfamilies, "C:/Users/pallen/Desktop/Dropbox/@R-working/gustometry/UPSIT_FamiliesDEC2014.csv")

write.csv(UPSITdata, "S:/Research/BennettoLab/TSFB Study/Data/UPSITdataforSPSS_2014-12-01.csv")

ezDesign(
  UPSITdata
  , x = score
  , y = group
  , cell_border_size = 10
)

ezPlot(
  UPSITdata
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

head(UPSITdata)
ezPrecis(UPSITdata)

ezStats(
  UPSITdata
  , dv = score
  , wid = SID
  , within = NULL
  , between = .(group)
  , type = 3
)

ezANOVA(
  UPSITdata
  , dv = score
  , wid = SID
  , within = NULL
  , between = .(group)
  , type = 3
)