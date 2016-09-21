# Script for analysing taste strip intensity responses

library("xlsx")

fileDir = 'C:/Users/pallen/Box Sync/Developmental Neuropsych Lab Management/Bennetto Lab Writing/Taste in ASD Paper'
demofile = 'tastestrips_intensit_19SEP2016.xlsx'

tastestrips <-  read.xlsx(paste(sep = "", collapse = NULL, fileDir, '/', demofile),1)

tastestrips$group <- paste(tastestrips$TSFB_Demographics..dem.research.group, tastestrips$TSFB_Demographics..dem.pedPosition, sep = "_")

data <- na.omit(tastestrips$TasteStrip.01response)

hist(data, breaks = c(0.5, 1.5, 2.5, 3.5, 4.5),
     main = paste("Histogram of Responses to Item number ", as.character(2))
)