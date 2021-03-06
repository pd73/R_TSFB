library("ez")
library("dplyr")

dataDir = 'X:/Research/BennettoLab/TSFB Study/Data/Cleaned data'
filedir = 'X:/Research/BennettoLab/TSFB Study/Data/exported from filemaker'
smellexport <- 'TSFB_SMELL_EXPORT.xlsx'
smellthreshfile <- 'ST_SummaryData_2016-02-19.xlsx'

smell <-  read.xlsx(paste(sep = "", collapse = NULL, filedir, '/', smellexport),1)
smellthresh <-  read.xlsx(paste(sep = "", collapse = NULL, dataDir, '/', smellthreshfile),1)

FamilyID <- as.character(smell$FamilyID)
SID <- paste('TS', FamilyID, smell$memberID, sep ="")
group <- paste(smell$Group, smell$tyoe, sep = "_")

smelldata <- data.frame(SID, FamilyID, smell$gender, smell$Age, group, smell$smell_SCORE)
names(smelldata) <- c('SID','Family', 'gender', 'age', 'group', 'score')

# This section is for data clean-up, e.g. to make sure everyone has a valid age
smelldata$SID[smelldata$age < 7]
smelldata$SID[is.na(smelldata$age)]
# data cleaned manually 1155 E,F, G not used

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