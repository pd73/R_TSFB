# Taste Strips Patterns of Responding

library("xlsx")
library("stats")
library("dplyr")
#library(devtools)
#install_github("vqv/ggbiplot")
library("knitr")
library(ggbiplot)

fileDir = 'C:/Users/pallen/Box Sync/Developmental Neuropsych Lab Management/Bennetto Lab Writing/Taste in ASD Paper'
demofile = 'TSFB_Winter_2016_KS_ConfusionTS v2.xlsx'

tastestrips <-  read.xlsx(paste(sep = "", collapse = NULL, fileDir, '/', demofile),1)

tastestrips<-na.omit(tastestrips)
tastestrips$group <- paste(tastestrips$TSFB_Demographics..dem.research.group, tastestrips$TSFB_Demographics..dem.pedPosition, sep = "_")

#TSgroup <- paste(tastestrips[5], tastestrips[4], sep = "_")


tastestrips.pca <- prcomp(tastestrips[,7:44], center= TRUE, scale. = TRUE)


print(tastestrips.pca)

# plot method
plot(tastestrips.pca, type = "l")

summary(tastestrips.pca)


g <- ggbiplot(tastestrips.pca, obs.scale = 1, var.scale = 1, 
               ellipse = TRUE, 
              circle = TRUE)
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal', 
               legend.position = 'top')
print(g)

ggbiplot(tastestrips.pca, obs.scale = 1, var.scale = 1,
         groups = tastestrips$group, ellipse = TRUE, circle = F) +
  scale_color_discrete(name = '') +
  theme(legend.direction = 'horizontal', legend.position = 'top')

ggscreeplot(tastestrips.pca)

actualtastes <- c(1,4,3,2,3,2,4,1,2,0,1,4,3,1,2,4,3,0,2,1,0,3,4,1,2,0,4,3,1,2,3,4,1,2,4,3)

pred<- t(slice(tastestrips[,7:42],1:4))
s2 <- table(actualtastes, pred)
###
probandcontrols <- filter(tastestrips, tastestrips$group == "Control_Proband")
s1<- t(slice(probandcontrols[,7:42],1))
probandcontrol.table <- table(actualtastes, s1) 
for (j in 2:length(probandcontrols[,1])) {
  sn <- t(slice(probandcontrols[,7:42],j))
  probandcontrol.table <- probandcontrol.table + table(actualtastes, sn) 
}

probandfamily <- filter(tastestrips, tastestrips$group == "Family_Proband")
s1<- t(slice(probandfamily[,7:42],1))
probandfamily.table <- table(actualtastes, s1) 
for (j in 2:length(probandfamily[,1])) {
  sn <- t(slice(probandfamily[,7:42],j))
  probandfamily.table <- probandfamily.table + table(actualtastes, sn) 
}

siblingfamily <- filter(tastestrips, tastestrips$group == "Family_Sibling")
s1<- t(slice(siblingfamily[,7:42],1))
siblingfamily.table <- table(actualtastes, s1) 
for (j in 2:length(siblingfamily[,1])) {
  sn <- t(slice(siblingfamily[,7:42],j))
  siblingfamily.table <- siblingfamily.table + table(actualtastes, sn) 
}

motherfamily <- filter(tastestrips, tastestrips$group == "Family_Mother")
s1<- t(slice(motherfamily[,7:42],1))
motherfamily.table <- table(actualtastes, s1) 
for (j in 2:length(motherfamily[,1])) {
  sn <- t(slice(motherfamily[,7:42],j))
  temp <- table(actualtastes, sn)
  if (length(temp)!= 20){
    print(temp)
    oddresp<-temp
  } else {
  motherfamily.table <- motherfamily.table + temp }
}

motherfamily <- filter(tastestrips, tastestrips$group == "Family_Mother")
s1<- t(slice(motherfamily[,7:42],1))
motherfamily.table <- table(actualtastes, s1) 
for (j in 2:length(motherfamily[,1])) {
  sn <- t(slice(motherfamily[,7:42],j))
  temp <- table(actualtastes, sn)
  if (length(temp)!= 20){
    print(motherfamily$TSFB.ID[j])
    print(temp)
    #oddresp<-temp
  } else {
    motherfamily.table <- motherfamily.table + temp }
}

fatherfamily <- filter(tastestrips, tastestrips$group == "Family_Father")
s1<- t(slice(fatherfamily[,7:42],1))
fatherfamily.table <- table(actualtastes, s1) 
for (j in 2:length(fatherfamily[,1])) {
  sn <- t(slice(fatherfamily[,7:42],j))
  temp <- table(actualtastes, sn)
  if (length(temp)!= 20){
    print(fatherfamily$TSFB.ID[j])
    print(temp)
    #oddresp<-temp
  } else {
    fatherfamily.table <- fatherfamily.table + temp }
}

### Now limit to the high intensity stimuli
hilowtastes <- c(-1,-4,-3,-2,-3,-2,-4,-1,-2,0,-1,-4,-3,-1,-2,-4,-3,0,2,1,0,3,4,1,2,0,4,3,1,2,3,4,1,2,4,3)

probandcontrols <- filter(tastestrips, tastestrips$group == "Control_Proband")
s1<- t(slice(probandcontrols[,7:42],1))
probandcontrol.table <- table(hilowtastes, s1) 
for (j in 2:length(probandcontrols[,1])) {
  sn <- t(slice(probandcontrols[,7:42],j))
  probandcontrol.table <- probandcontrol.table + table(hilowtastes, sn) 
}

print(probandcontrol.table)

probandfamily <- filter(tastestrips, tastestrips$group == "Family_Proband")
s1<- t(slice(probandfamily[,7:42],1))
probandfamily.table <- table(hilowtastes, s1) 
for (j in 2:length(probandfamily[,1])) {
  sn <- t(slice(probandfamily[,7:42],j))
  probandfamily.table <- probandfamily.table + table(hilowtastes, sn) 
}

print(probandfamily.table)

siblingfamily <- filter(tastestrips, tastestrips$group == "Family_Sibling")
s1<- t(slice(siblingfamily[,7:42],1))
siblingfamily.table <- table(hilowtastes, s1) 
for (j in 2:length(siblingfamily[,1])) {
  sn <- t(slice(siblingfamily[,7:42],j))
  siblingfamily.table <- siblingfamily.table + table(hilowtastes, sn) 
}

print(siblingfamily.table)

motherfamily <- filter(tastestrips, tastestrips$group == "Family_Mother")
s1<- t(slice(motherfamily[,7:42],1))
motherfamily.table <- table(hilowtastes, s1) 
for (j in 2:length(motherfamily[,1])) {
  sn <- t(slice(motherfamily[,7:42],j))
  temp <- table(hilowtastes, sn)
   if (length(temp)!= 36){
     print(motherfamily$TSFB.ID[j])
     print(temp)
     #oddresp<-temp
   } else {
    motherfamily.table <- motherfamily.table + temp }
}

print(motherfamily.table)

fatherfamily <- filter(tastestrips, tastestrips$group == "Family_Father")
s1<- t(slice(fatherfamily[,7:42],1))
fatherfamily.table <- table(hilowtastes, s1) 
for (j in 2:length(fatherfamily[,1])) {
  sn <- t(slice(fatherfamily[,7:42],j))
  temp <- table(hilowtastes, sn)
  if (length(temp)!= 36){
    print(fatherfamily$TSFB.ID[j])
    print(temp)
    #oddresp<-temp
  } else {
    fatherfamily.table <- fatherfamily.table + temp }
}

print(fatherfamily.table)

table(tastestrips$TasteStrip.PTCControlResponse, tastestrips$TasteStrip.PTCPaperResponse)
