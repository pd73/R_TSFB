library("ez")
library("dplyr")

TasteStrip <- read.csv("C:/Users/pallen/Desktop/Dropbox/@R-working/gustometry/TasteStrips_DEC2014.csv")

TasteStrip <- data.frame(TasteStrip$TSID, as.character(TasteStrip$FID), TasteStrip$Gender, TasteStrip$Age, TasteStrip$group, TasteStrip$TasteStrip.TotalScore)
names(TasteStrip) <- c('SID','Family', 'gender', 'age', 'group', 'score')

# This section is for data clean-up, e.g. to make sure everyone has a valid age
TasteStrip$SID[TasteStrip$age < 7]
TasteStrip$SID[is.na(TasteStrip$age)]
# data cleaned manually 1155 E,F, G not used

famList <- unique(TasteStrip$Family[TasteStrip$group == "Proband_Family"])
numFam <- length(famList)
                  
ASDfamilies <- data.frame(famList,rep(NA, numFam),rep(NA, numFam),rep(NA, numFam),rep(NA, numFam),rep(NA, numFam), rep(NA, numFam),rep(NA, numFam),rep(NA, numFam),rep(NA, numFam))
names(ASDfamilies) <- c('FID','proband','sibling','mom','dad', 'famAve', 'parentAve', 'trioAve', 'sibsDiff', 'probparDiff')

for (i in 1:numFam) {
  ASDfamilies$proband[i] <- TasteStrip$score[TasteStrip$Family == ASDfamilies$FID[i] & (TasteStrip$group == "Proband_Family")]
  ASDfamilies$sibling[i] <- mean(TasteStrip$score[TasteStrip$Family == ASDfamilies$FID[i] & (TasteStrip$group == "Sibling_Family")])
  ASDfamilies$mom[i] <- mean(TasteStrip$score[TasteStrip$Family == ASDfamilies$FID[i] & (TasteStrip$group == "Mother_Family")])
  ASDfamilies$dad[i] <- mean(TasteStrip$score[TasteStrip$Family == ASDfamilies$FID[i] & (TasteStrip$group == "Father_Family")])
  ASDfamilies$famAve[i] <- mean(TasteStrip$score[TasteStrip$Family == ASDfamilies$FID[i]])
  ASDfamilies$trioAve[i] <- (ASDfamilies$proband[i] + ASDfamilies$mom[i] + ASDfamilies$dad[i])/3
  ASDfamilies$parentAve[i] <-  (ASDfamilies$mom[i] + ASDfamilies$dad[i])/2
  ASDfamilies$sibsDiff[i] <- ASDfamilies$proband[i] - ASDfamilies$sibling[i]
  ASDfamilies$probparDiff[i] <- ASDfamilies$proband[i] - ASDfamilies$parentAve[i]
}

write.csv(ASDfamilies, "C:/Users/pallen/Desktop/Dropbox/@R-working/gustometry/TasteStrips_FamiliesDEC2014.csv")

write.csv(TasteStrip, "S:/Research/BennettoLab/TSFB Study/Data/TasteStripforSPSS_2014-12-01.csv")

ezDesign(
  TasteStrip
  , x = score
  , y = group
  , cell_border_size = 10
)

ezPlot(
  TasteStrip
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

head(TasteStrip)
ezPrecis(TasteStrip)

ezStats(
  TasteStrip
  , dv = score
  , wid = SID
  , within = NULL
  , between = .(group)
  , type = 3
)

ezANOVA(
  TasteStrip
  , dv = score
  , wid = SID
  , within = NULL
  , between = .(group)
  , type = 3
)