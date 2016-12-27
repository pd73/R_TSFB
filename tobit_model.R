#Tobit model for UPSIT vs Smell threshold

# the package "VGAM" is needed for the Tobit routine.
library(VGAM)
OlfactionData_for_tobit <- read.csv("C:/Users/pallen/Box Sync/Developmental Neuropsych Lab Management/Bennetto Lab Writing/Olfactory Fx in Autism Paper/Data/OlfactionData_for_tobit.csv")


# Fit regression model to censored data
lin_model <- lm(STT ~ UPSIT*ASD, data=OlfactionData_for_tobit)
summary(lin_model)

# Fit Tobit model

int_model <-  vglm(STT ~ UPSIT*ASD, data=OlfactionData_for_tobit, tobit(Lower=-10.5, Upper=-1.5))
summary(int_model)
coef(int_model, matrix = TRUE) 
coef(int_model, matrix = FALSE) 

reg_model <-  vglm(STT ~ UPSIT+ASD, data=OlfactionData_for_tobit, tobit(Lower=-10.5, Upper=-1.5))
summary(reg_model)
coef(reg_model, matrix = TRUE) 

uni_model <-  vglm(STT ~ UPSIT, data=OlfactionData_for_tobit, tobit(Lower=-10.5, Upper=-1.5))
summary(uni_model)
coef(uni_model, matrix = TRUE) 

# 
# plot(STT ~ UPSIT*ASD, data = OlfactionData_for_tobit)
# plot(STT ~ UPSIT + ASD, data = OlfactionData_for_tobit)
# plot(STT ~ Group, data = OlfactionData_for_tobit)
# plot(UPSIT ~ Group, data = OlfactionData_for_tobit)
# plot(UPSIT ~ ASD, data = OlfactionData_for_tobit)

plot( STT ~ UPSIT, data = OlfactionData_for_tobit, col = ASD, ylim = c(-10.5,0))

lines(fitted(int_model) ~ UPSIT, data = OlfactionData_for_tobit, col = 3)
lines(fitted(uni_model) ~ UPSIT, data = OlfactionData_for_tobit, col = 4)
lines(fitted(lin_model) ~ UPSIT, data = OlfactionData_for_tobit, col = 5)

