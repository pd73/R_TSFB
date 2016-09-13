library(foreign)
library(pROC)

dataset = read.spss("C:\\Users\\pallen\\Desktop\\AuditoryData_2016-04-12_DPTR_FinalwDx.sav", to.data.frame=TRUE)


dataset2 <- dataset[dataset$Omit == 0,2:198]

logitfit <- glm(Auditory.Group.Number ~ Auditory.CA , data = dataset2, family = "binomial")
summary(logitfit)
exp(cbind(OR = coef(logitfit), confint(logitfit)))

plot.roc(dataset2$Auditory.Group.Number, dataset2$Auditory.CA)

logitfit <- glm(Auditory.Group.Number ~ IQ.FSIQ , data = dataset2, family = "binomial")
summary(logitfit)
exp(cbind(OR = coef(logitfit), confint(logitfit)))

plot.roc(dataset2$Auditory.Group.Number, dataset2$IQ.FSIQ)

logitfit <- glm(Auditory.Group.Number ~ SRS.TotalScore.Tscore , data = dataset2, family = "binomial")
summary(logitfit)
exp(cbind(OR = coef(logitfit), confint(logitfit)))

plot.roc(dataset2$Auditory.Group.Number, dataset2$SRS.TotalScore.Tscore)

logitfit <- glm(Auditory.Group.Number ~ ADOS_SeverityScore , data = dataset2, family = "binomial")
summary(logitfit)
exp(cbind(OR = coef(logitfit), confint(logitfit)))

plot.roc(dataset2$Auditory.Group.Number, dataset2$ADOS_SeverityScore)

logitfit <- glm(Auditory.Group.Number ~ DP.1000.70.70.SNR , data = dataset2, family = "binomial")
summary(logitfit)
exp(cbind(OR = coef(logitfit), confint(logitfit)))

plot.roc(dataset2$Auditory.Group.Number, dataset2$DP.1000.70.70.SNR, 
         identity = TRUE, 
         print.thres = c(10,13.25,15,20,25), 
         print.auc = TRUE, print.auc.x = 0.8, print.auc.y = 0.1,
         ci = TRUE)

plot.roc(dataset2$Auditory.Group.Number, dataset2$DP.4000.70.70.SNR, 
         identity = TRUE, 
         print.thres = c(15,20,25,30, 35), 
         print.auc = TRUE, print.auc.x = 0.8, print.auc.y = 0.1,
         ci = TRUE)

t.test(DP.1000.70.70.SNR ~ Auditory.Group.Number, data = dataset2)

logitfit <- glm(Auditory.Group.Number ~ DP.1000.70.70.SNR + IQ.FSIQ, data = dataset2, family = "binomial")
summary(logitfit)
exp(cbind(OR = coef(logitfit), confint(logitfit)))

roc(dataset2$Auditory.Group.Number, dataset2$DP.1000.70.70.SNR)